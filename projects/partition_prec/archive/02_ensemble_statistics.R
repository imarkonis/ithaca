# Estimation of ensemble statistics
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Packages
library("gtools")

## Data
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

## Functions
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Analysis
cores <- N_CORES - 3
cl <- makeCluster(cores, type = "FORK")
prec_mean <- parLapply(cl, prec_2000_2019, calc, fun = mean, na.rm = TRUE)
stopCluster(cl)
prec_sd <- lapply(prec_2000_2019, calc, fun = function(x) {sd(x, na.rm = TRUE)}) #sd doesn't work with calc in parallel 

### Multi-source
registerDoParallel(cores = N_CORES - 1)
prec_mean_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- data.table(as.data.frame(rasterToPoints(prec_mean[[dataset_count]], spatial = TRUE)))
  colnames(dummy) <- c('prec_mean', 'lon', 'lat')
  dummy$dataset <- names(prec_mean[dataset_count])
  dummy[, lon := round(lon, 3)]
  dummy[, lat := round(lat, 3)]
  dummy
}

prec_sd_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- data.table(as.data.frame(rasterToPoints(prec_sd[[dataset_count]], spatial = TRUE)))
  colnames(dummy) <- c('prec_sd', 'lon', 'lat')
  dummy$dataset <- names(prec_sd[dataset_count])
  dummy[, lon := round(lon, 3)]
  dummy[, lat := round(lat, 3)]
  dummy
}
prec_mean_datasets[, n_datasets := .N, .(lon, lat)]
prec_mean_datasets <- prec_mean_datasets[n_datasets >= MIN_N_DATASETS]

prec_mean_datasets[dataset %in% PREC_DATASETS_OBS, dataset_type := 'ground stations']
prec_mean_datasets[dataset %in% PREC_DATASETS_REANAL, dataset_type := 'reanalysis']
prec_mean_datasets[dataset %in% PREC_DATASETS_REMOTE, dataset_type := 'remote sensing']
prec_mean_datasets[, prec_mean := round(prec_mean, 2)]

prec_datasets <- merge(prec_mean_datasets, prec_sd_datasets, by = c("lon", "lat", "dataset"))
prec_datasets <- prec_datasets[, .(lon, lat, dataset, dataset_type, prec_mean, prec_sd = round(prec_sd, 2))]

### Annual sums   
prec_annual <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  dummie_raster <- prec_2000_2019[[dataset_count]]
  dummie_weights <- area(dummie_raster, na.rm = TRUE, weights = TRUE)
  dummie_table <- dummie_raster * dummie_weights
  dummie_table <- cellStats(dummie_table, 'sum')
  dummie_table <- data.table(getZ(dummie_raster), dummie_table)
  setnames(dummie_table, c("date", "wavg_yearly"))
  dummie_table <- dummie_table[, .(year = year(date), prec_mean =  wavg_yearly)]
}
names(prec_annual) <- names(prec_2000_2019)
prec_annual$`gpm-imerg`[1] <- NA
prec_annual <- bind_rows(prec_annual, .id = "column_label")
colnames(prec_annual)[1] <- "dataset"

### Ensemble statistics
prec_ens_stats <- prec_mean_datasets[, .(ens_mean_mean = round(mean(prec_mean, na.rm = TRUE), 2)), .(lat, lon)]
prec_mean_datasets[, n_datasets := NULL]

#dummy <- prec_mean_datasets[, .(ens_mean_sd = round(sd(prec_mean, na.rm = TRUE), 0)), .(lat, lon)]
#prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- prec_mean_datasets[, .(ens_mean_median = round(median(prec_mean, na.rm = TRUE), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))
dummy <- prec_mean_datasets[, .(ens_mean_sd = round(sd(prec_mean, na.rm = TRUE), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))
dummy <- prec_mean_datasets[, .(ens_mean_q25 = round(estimate_q25(prec_mean), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))
dummy <- prec_mean_datasets[, .(ens_mean_q75 = round(estimate_q75(prec_mean), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

#### Bias measures
prec_ens_stats[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
prec_ens_stats[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

### Grid cell area
prec_area <- prec_ens_stats[, .(lon, lat)] %>% grid_area() # m2
prec_grid <- prec_area[prec_ens_stats[, .(lon, lat, prec_mean = ens_mean_mean)], on = .(lon, lat)]
prec_grid[, prec_volume_year := area * 10 ^ (-9) * prec_mean * 0.001][, prec_mean := NULL] # km3

## Save data 
saveRDS(prec_ens_stats, paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
saveRDS(prec_annual, paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_datasets.rds"))
saveRDS(prec_datasets, paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
saveRDS(prec_grid , paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.rds"))