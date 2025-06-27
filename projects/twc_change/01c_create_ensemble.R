source('source/twc_change.R')

files <- list.files(PATH_OUTPUT_RAW_TABULAR, pattern = "\\.Rds$", full.names = TRUE)

#Precipitation
prec_files <- files[grepl("_prec\\.Rds$", files)]
prec_files <- prec_files[!grepl("MSWEP", prec_files)] #Avoid using MSWEP in the ensemble as it merges data
prec_list <- lapply(prec_files, readRDS)
prec_datasets <- rbindlist(prec_list)
prec_datasets[, variable := NULL]

ensemble_stats <- prec_datasets[, .(
  ens_median = median(value, na.rm = TRUE),
  ens_cv     = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE)
), by = .(lon, lat)]

prec_datasets[, date_num := as.numeric(date)]
prec_slopes <- prec_datasets[
  , .(slope = if (.N > 1) coef(lm(value ~ date_num))[2] else NA_real_),
  by = .(lon, lat, dataset)
]

slope_summary <- prec_slopes[
  , .(ens_slope_median = median(slope, na.rm = TRUE)),
  by = .(lon, lat)
]

prec_ensemble_stats <- merge(
  ensemble_stats, 
  slope_summary, 
  by = c("lon", "lat"), 
  all.x = TRUE
)
prec_ensemble_stats <- prec_ensemble_stats[complete.cases(ensemble_stats)]
saveRDS(prec_ensemble_stats, file = paste0(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
saveRDS(prec_datasets, file = paste0(PATH_OUTPUT_DATA, 'prec_ensemble.Rds'))
saveRDS(prec_slopes, file = paste0(PATH_OUTPUT_DATA, 'prec_ensemble_slopes.Rds'))

#Evaporation
evap_files <- files[grepl("_evap\\.Rds$", files)]
evap_list <- lapply(evap_files, readRDS)
evap_datasets <- rbindlist(evap_list)
evap_datasets[, variable := NULL]
evap_datasets <- evap_datasets[date >= min(evap_datasets[, min(date), dataset]$V1)]

ensemble_stats <- evap_datasets[, .(
  ens_median = median(value, na.rm = TRUE),
  ens_cv     = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE)
), by = .(lon, lat)]

evap_datasets[, date_num := as.numeric(date)]
evap_slopes <- evap_datasets[
  , .(slope = if (.N > 1) coef(lm(value ~ date_num))[2] else NA_real_),
  by = .(lon, lat, dataset)
]

slope_summary <- evap_slopes[
  , .(ens_slope_median = median(slope, na.rm = TRUE)),
  by = .(lon, lat)
]

evap_ensemble_stats <- merge(
  ensemble_stats, 
  slope_summary, 
  by = c("lon", "lat"), 
  all.x = TRUE
)

evap_ensemble_stats <- evap_ensemble_stats[complete.cases(ensemble_stats)]
saveRDS(evap_ensemble_stats, file = paste0(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))
saveRDS(evap_datasets, file = paste0(PATH_OUTPUT_DATA, 'evap_ensemble.Rds'))
saveRDS(evap_slopes, file = paste0(PATH_OUTPUT_DATA, 'evap_ensemble_slopes.Rds'))
