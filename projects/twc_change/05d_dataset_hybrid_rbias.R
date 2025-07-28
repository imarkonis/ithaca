source('source/twc_change.R')

# Data
masks <- pRecipe::pRecipe_masks()
robin_meta <- fread('~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv')
robin_coords <- readRDS(paste0(PATH_OUTPUT_RAW, 'robin_coords.rds'))
avail_flux <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_grid.rds'))
runoff_year <- readRDS('~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_yearly.rds')
dataset_weights <- readRDS(paste0(PATH_OUTPUT, 'dataset_pair_weights.rds'))

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

compute_robin_residual <- function(hybrid_dataset, robin_coords, robin_meta, runoff_year,
                                   START_PERIOD_1, END_PERIOD_1, END_PERIOD_2) {
  
  # Join availability changes to robin coordinates
  water_avail_change_robin <- merge(hybrid_dataset, robin_coords, by = c('lon', 'lat'))
  water_avail_change_robin <- water_avail_change_robin[, .(
    water_avail_change_mean = mean(avail_change, na.rm = TRUE)
  ), by = robin_id]
  
  # Filter and define runoff periods
  runoff_periods <- runoff_year[year >= year(START_PERIOD_1) & year <= year(END_PERIOD_2)]
  runoff_periods[, period := ordered('pre_2001')]
  runoff_periods[year > year(END_PERIOD_1), period := ordered('aft_2001')]
  
  # Compute runoff change
  runoff_change <- runoff_periods[, .(value = mean(flow, na.rm = TRUE)), by = .(robin_id, period)]
  runoff_change_wide <- dcast(runoff_change, robin_id ~ period)
  runoff_change_wide[, runoff_diff := aft_2001 - pre_2001]
  
  # Merge everything
  validation_dt <- merge(water_avail_change_robin, runoff_change_wide[, .(robin_id, runoff_diff)], by = 'robin_id')
  validation_dt <- merge(validation_dt, robin_meta[, .(robin_id = ROBIN_ID, hydrobelt = HYDROBELT)], by = "robin_id")
  
  # Compute relative residual
  validation_dt[, rel_residual := fifelse(runoff_diff != 0,
                                          (water_avail_change_mean - runoff_diff) / runoff_diff,
                                          NA_real_)]
  
  return(validation_dt[, .(robin_id, hydrobelt, water_avail_change_mean, runoff_diff, rel_residual)])
}


# Analysis

dataset_entropy <- dataset_weights[, .(entropy = -sum(weight * log(weight))), .(lon, lat)]
weighted_entropy <- merge(dataset_entropy, avail_flux_change_weighted)
best_performing_entropy <- merge(dataset_entropy, avail_flux_change_best_performer_kg)

n_sample <- 10000
entropy_probs <- seq(0.01, 0.99, by = 0.01)
entropy_values <- dataset_entropy$entropy
entropy_quantiles <- quantile(entropy_values, probs = entropy_probs, na.rm = TRUE)

entropy_relbias <- data.table(quantile = entropy_probs, entropy = entropy_quantiles, rmse = NA_real_)

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
    sampled_residuals <- sample(rel_residual, n_sample, replace = TRUE)
    rmse_value <- sqrt(mean(sampled_residuals^2, na.rm = TRUE))
    list(rmse = rmse_value)
  }, by = hydrobelt]
  
  entropy_relbias[i, rmse := mean(rmse_entropy_rmse$rmse)]
}

ENTROPY_THRESHOLD <- entropy_relbias[which.min(rmse)]$entropy
weighted_entropy <- merge(dataset_entropy, avail_flux_change_weighted)
weighted_entropy <- weighted_entropy[entropy <= ENTROPY_THRESHOLD]

best_performing_entropy <- merge(dataset_entropy, avail_flux_change_best_performer_kg)
best_performing_entropy <- best_performing_entropy[entropy > ENTROPY_THRESHOLD]

hybrid_dataset <- rbind(weighted_entropy, best_performing_entropy)

saveRDS(hybrid_dataset[, .(lon, lat, avail_change, flux_change)], file = paste0(PATH_OUTPUT, 'avail_flux_change_hybrid_rel.rds'))


entropy_rmse_classification <- merge(dataset_entropy, masks[land_mask == 'land', .(lon, lat, KG_class)]) 
entropy_KG <- entropy_rmse_classification[entropy > ENTROPY_THRESHOLD, mean(entropy, na.rm=T), KG_class]
entropy_KG[order(V1),]

entropy_KG <- entropy_rmse_classification[entropy > ENTROPY_THRESHOLD]
entropy_KG[, table(KG_class)]
