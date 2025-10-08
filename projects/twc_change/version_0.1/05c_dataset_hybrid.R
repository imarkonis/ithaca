##### NEED TO CHECK - MAYBE ERRORS

source('source/twc_change.R')

# Data loading (unchanged)
masks <- pRecipe::pRecipe_masks()
robin_meta <- fread('~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv')
robin_coords <- readRDS(paste0(PATH_OUTPUT_RAW, 'robin_coords.rds'))
avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_grid.rds'))
runoff_year <- readRDS('~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_yearly.rds')
dataset_weights <- readRDS(paste0(PATH_OUTPUT_DATA, 'dataset_weights.Rds'))

avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_grid.rds'))
avail_flux_change_weighted <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_grid_weighted.rds'))
avail_flux_change_best_performer_kg <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_best_performer_kg.rds'))

# --- 1. IQR calculation ---
dataset_iqr <- avail_flux_change[,
                                 .(iqr = IQR(avail_change, na.rm = TRUE)),
                                 by = .(lon, lat)
]

# --- 2. Standardize IQR (optional) ---
iqr_mean <- mean(dataset_iqr$iqr, na.rm = TRUE)
iqr_sd <- sd(dataset_iqr$iqr, na.rm = TRUE)
dataset_iqr[, iqr_std := (iqr - iqr_mean) / iqr_sd]

# --- 3. Merge IQR with ensemble outputs ---
weighted_iqr <- merge(dataset_iqr, avail_flux_change_weighted, by = c("lon", "lat"))
best_performing_iqr <- merge(dataset_iqr, avail_flux_change_best_performer_kg, by = c("lon", "lat"))

# --- 4. Thresholds for IQR ---
iqr_probs <- seq(0.01, 0.99, by = 0.01)
iqr_values <- dataset_iqr$iqr_std
iqr_quantiles <- quantile(iqr_values, probs = iqr_probs, na.rm = TRUE)

iqr_rmse <- data.table(quantile = iqr_probs, iqr_std = iqr_quantiles, rmse = NA_real_)

# --- 5. Hybrid dataset builder using IQR ---
build_hybrid_dataset <- function(iqr_threshold_value, weighted_iqr, best_performing_iqr) {
  weighted_iqr_sub <- weighted_iqr[iqr_std <= iqr_threshold_value]
  best_performing_iqr_sub <- best_performing_iqr[iqr_std > iqr_threshold_value]
  hybrid_dataset <- rbind(weighted_iqr_sub, best_performing_iqr_sub)
  return(hybrid_dataset)
}

# --- 6. Compute robin residuals (unchanged) ---
compute_robin_residual <- function(hybrid_dataset, robin_coords, robin_meta, runoff_year, START_PERIOD_1, END_PERIOD_1, END_PERIOD_2) {
  water_avail_change_robin <- merge(hybrid_dataset, robin_coords, by = c('lon', 'lat'))
  water_avail_change_robin <- water_avail_change_robin[, .(
    water_avail_change_mean = mean(avail_change)
  ), by = robin_id]
  
  runoff_periods <- runoff_year[year >= year(START_PERIOD_1) & year <= year(END_PERIOD_2)]
  runoff_periods[, period := ordered('pre_2001')]
  runoff_periods[year > year(END_PERIOD_1), period := ordered('aft_2001')]
  
  runoff_change <- runoff_periods[, .(value = mean(flow)), .(robin_id, period)]
  runoff_change_wide <- dcast(runoff_change, robin_id ~ period)
  runoff_change_wide[, runoff_diff := aft_2001 - pre_2001]
  
  validation_dt <- merge(water_avail_change_robin, runoff_change_wide[, .(robin_id, runoff_diff)], by = 'robin_id')
  validation_dt <- merge(validation_dt, robin_meta[, .(robin_id = ROBIN_ID, hydrobelt = HYDROBELT)], by = "robin_id")
  validation_dt[, residual := water_avail_change_mean - runoff_diff]
  
  return(validation_dt[, .(robin_id, hydrobelt, water_avail_change_mean, runoff_diff, residual)])
}

# --- 7. Threshold sweep loop ---
n_sample <- 10000
set.seed(1979)
for (i in seq_along(iqr_quantiles)) {
  
  threshold_value <- iqr_quantiles[i]
  
  hybrid_dataset <- build_hybrid_dataset(
    iqr_threshold_value = threshold_value,
    weighted_iqr,
    best_performing_iqr
  )
  
  robin_residual <- compute_robin_residual(
    hybrid_dataset,
    robin_coords,
    robin_meta,
    runoff_year,
    START_PERIOD_1,
    END_PERIOD_1,
    END_PERIOD_2
  )
  
  rmse_iqr_rmse <- robin_residual[, {
    sampled_residuals <- sample(residual, n_sample, replace = TRUE)
    rmse_value <- sqrt(mean(sampled_residuals^2, na.rm = TRUE))
    list(rmse = rmse_value)
  }, by = hydrobelt]
  
  iqr_rmse[i, rmse := mean(rmse_iqr_rmse$rmse)]
}

IQR_THRESHOLD <- iqr_rmse[which.min(rmse)]$iqr_std
weighted_iqr_final <- weighted_iqr[iqr_std <= IQR_THRESHOLD]
best_performing_iqr_final <- best_performing_iqr[iqr_std > IQR_THRESHOLD]

hybrid_dataset <- rbind(weighted_iqr_final, best_performing_iqr_final)

saveRDS(hybrid_dataset[, .(lon, lat, avail_change, flux_change)], file = paste0(PATH_OUTPUT, 'avail_flux_change_hybrid.rds'))

# Optionally, analyze or map where thresholded by IQR
iqr_rmse_classification <- merge(dataset_iqr, masks[land_mask == 'land', .(lon, lat, KG_class)])
iqr_KG <- iqr_rmse_classification[iqr_std > IQR_THRESHOLD, mean(iqr_std, na.rm = TRUE), KG_class]
iqr_KG[order(V1),]
iqr_KG <- iqr_rmse_classification[iqr_std > IQR_THRESHOLD]
iqr_KG[, table(KG_class)]






######################OLD VERSION

source('source/twc_change.R')

# Data
masks <- pRecipe::pRecipe_masks()
robin_meta <- fread('~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv')
robin_coords <- readRDS(paste0(PATH_OUTPUT_RAW, 'robin_coords.rds'))
avail_flux <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_grid.rds'))
runoff_year <- readRDS('~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_yearly.rds')
dataset_weights <- readRDS(paste0(PATH_OUTPUT, 'dataset_weights.Rds'))

avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))
avail_flux_change_weighted <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid_weighted.rds'))
avail_flux_change_best_performer_kg <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_best_performer_kg.rds'))


# Functions
build_hybrid_dataset <- function(entropy_threshold_value, weighted_entropy_all, best_performing_entropy_all) {
  
  weighted_entropy <- weighted_entropy_all[entropy <= entropy_threshold_value]
  best_performing_entropy <- best_performing_entropy_all[entropy > entropy_threshold_value]
  hybrid_dataset <- rbind(weighted_entropy, best_performing_entropy)
  
  return(hybrid_dataset)
}

compute_robin_residual <- function(hybrid_dataset, robin_coords, robin_meta, runoff_year, START_PERIOD_1, END_PERIOD_1, END_PERIOD_2) {

  water_avail_change_robin <- merge(hybrid_dataset, robin_coords, by = c('lon', 'lat'))
  water_avail_change_robin <- water_avail_change_robin[, .(
    water_avail_change_mean = mean(avail_change)
  ), by = robin_id]
  
  runoff_periods <- runoff_year[year >= year(START_PERIOD_1) & year <= year(END_PERIOD_2)]
  runoff_periods[, period := ordered('pre_2001')]
  runoff_periods[year > year(END_PERIOD_1), period := ordered('aft_2001')]
  
  runoff_change <- runoff_periods[, .(value = mean(flow)), .(robin_id, period)]
  runoff_change_wide <- dcast(runoff_change, robin_id ~ period)
  runoff_change_wide[, runoff_diff := aft_2001 - pre_2001]
  
  validation_dt <- merge(water_avail_change_robin, runoff_change_wide[, .(robin_id, runoff_diff)], by = 'robin_id')
  validation_dt <- merge(validation_dt, robin_meta[, .(robin_id = ROBIN_ID, hydrobelt = HYDROBELT)], by = "robin_id")
  validation_dt[, residual := water_avail_change_mean - runoff_diff]
  
  return(validation_dt[, .(robin_id, hydrobelt, water_avail_change_mean, runoff_diff, residual)])
}


# Analysis

# Step 1: Calculate IQR per grid cell
dataset_iqr <- avail_flux_change[, 
                                 .(iqr = IQR(avail_change, na.rm = TRUE)), 
                                 by = .(lon, lat)
]

# Step 2: Standardize the IQRs (z-score)
iqr_mean <- mean(dataset_iqr$iqr, na.rm = TRUE)
iqr_sd <- sd(dataset_iqr$iqr, na.rm = TRUE)
dataset_iqr[, iqr_std := (iqr - iqr_mean) / iqr_sd]

# Step 3: Merge with the two ensemble outputs
weighted_iqr <- merge(dataset_iqr, avail_flux_change_weighted, by = c("lon", "lat"))
best_performing_iqr <- merge(dataset_iqr, avail_flux_change_best_performer_kg, by = c("lon", "lat"))

# Step 4: Setup thresholds
n_sample <- 10000
iqr_probs <- seq(0.01, 0.99, by = 0.01)
iqr_values <- dataset_iqr$iqr_std
iqr_quantiles <- quantile(iqr_values, probs = iqr_probs, na.rm = TRUE)

# Step 5: Initialize RMSE table (or any metric table you want)
iqr_rmse <- data.table(quantile = iqr_probs, iqr_std = iqr_quantiles, rmse = NA_real_)

set.seed(1979)
for (i in seq_along(entropy_quantiles)) {
  
  threshold_value <- entropy_quantiles[i]
  
  hybrid_dataset <- build_hybrid_dataset(
    entropy_threshold_value = threshold_value, 
    weighted_entropy, 
    best_performing_entropy
  )
  
  robin_residual <- compute_robin_residual(
    hybrid_dataset, 
    robin_coords, 
    robin_meta, 
    runoff_year, 
    START_PERIOD_1, 
    END_PERIOD_1, 
    END_PERIOD_2
  )
  
  rmse_entropy_rmse <- robin_residual[, {
    sampled_residuals <- sample(residual, n_sample, replace = TRUE)
    rmse_value <- sqrt(mean(sampled_residuals^2, na.rm = TRUE))
    list(rmse = rmse_value)
  }, by = hydrobelt]
  
  entropy_rmse[i, rmse := mean(rmse_entropy_rmse$rmse)]
}

ENTROPY_THRESHOLD <- entropy_rmse[which.min(rmse)]$entropy
weighted_entropy <- merge(dataset_entropy, avail_flux_change_weighted)
weighted_entropy <- weighted_entropy[entropy <= ENTROPY_THRESHOLD]

best_performing_entropy <- merge(dataset_entropy, avail_flux_change_best_performer_kg)
best_performing_entropy <- best_performing_entropy[entropy > ENTROPY_THRESHOLD]

hybrid_dataset <- rbind(weighted_entropy, best_performing_entropy)

saveRDS(hybrid_dataset[, .(lon, lat, avail_change, flux_change)], file = paste0(PATH_OUTPUT, 'avail_flux_change_hybrid.rds'))


entropy_rmse_classification <- merge(dataset_entropy, masks[land_mask == 'land', .(lon, lat, KG_class)]) 
entropy_KG <- entropy_rmse_classification[entropy > ENTROPY_THRESHOLD, mean(entropy, na.rm=T), KG_class]
entropy_KG[order(V1),]

entropy_KG <- entropy_rmse_classification[entropy > ENTROPY_THRESHOLD]
entropy_KG[, table(KG_class)]



