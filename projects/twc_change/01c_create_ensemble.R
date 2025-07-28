install.packages('trend')

source('source/twc_change.R')
library(trend)

files <- list.files(PATH_OUTPUT_RAW_TABULAR, pattern = "\\.Rds$", full.names = TRUE)

#Precipitation
prec_files <- files[grepl("_prec\\.Rds$", files)]
prec_files <- prec_files[!grepl("MSWEP", prec_files)] #Avoid using MSWEP in the ensemble as it merges data
prec_list <- lapply(prec_files, readRDS)
prec_datasets <- rbindlist(prec_list)
prec_datasets[, variable := NULL]

dataset_stats <- prec_datasets[, .(
  dataset_mean = mean(value, na.rm = TRUE),
  dataset_sd     = sd(value, na.rm = TRUE) 
), by = .(lon, lat, dataset)]

ensemble_stats <- dataset_stats[, .(
  ens_median = median(dataset_mean, na.rm = TRUE),
  ens_sd     = median(dataset_sd, na.rm = TRUE) 
), by = .(lon, lat)]

prec_slopes <- prec_datasets[
  , {
    if (.N > 1) {
      s = sens.slope(x = value)
      mk = mk.test(x = value)
      list(
        sen_slope = s$estimates,
        p_value = mk$p.value
      )
    } else {
      list(
        sen_slope = NA_real_,
        p_value = NA_real_
      )
    }
  },
  by = .(lon, lat, dataset)
]

slope_summary <- copy(prec_slopes)
slope_summary <- slope_summary[, ens_slope_median := median(sen_slope, na.rm = TRUE), .(lon, lat)]
slope_summary[, significant := p_value < 0.1]
slope_summary[, slope_sign := sign(sen_slope)]

agreement_summary <- slope_summary[significant == TRUE, .(
  n_significant = .N,
  n_pos = sum(slope_sign > 0),
  n_neg = sum(slope_sign < 0)
), by = .(lon, lat)]
agreement_summary[, majority_significant := n_significant > 4] 
agreement_summary[majority_significant == TRUE, majority_agrees := abs(n_neg - n_pos) > 2] 

ens_slopes <- slope_summary[, .(lon, lat, ens_slope_median)]
ens_slopes <- ens_slopes[!duplicated(ens_slopes)]

prec_ensemble_stats <- merge(
  ensemble_stats, 
  ens_slopes, 
  by = c("lon", "lat"), 
  all.x = TRUE
)

prec_ensemble_stats <- merge(
  prec_ensemble_stats, 
  agreement_summary, 
  by = c("lon", "lat"), 
  all.x = TRUE
)

saveRDS(prec_ensemble_stats, file = paste0(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
saveRDS(prec_datasets, file = paste0(PATH_OUTPUT_DATA, 'prec_ensemble.Rds'))
saveRDS(prec_slopes, file = paste0(PATH_OUTPUT_DATA, 'prec_ensemble_slopes.Rds'))

#Evaporation
evap_files <- files[grepl("_evap\\.Rds$", files)]
evap_list <- lapply(evap_files, readRDS)
evap_datasets <- rbindlist(evap_list)
evap_datasets[, variable := NULL]

dataset_stats <- evap_datasets[, .(
  dataset_mean = mean(value, na.rm = TRUE),
  dataset_sd     = sd(value, na.rm = TRUE) 
), by = .(lon, lat, dataset)]

ensemble_stats <- dataset_stats[, .(
  ens_median = median(dataset_mean, na.rm = TRUE),
  ens_sd     = median(dataset_sd, na.rm = TRUE) 
), by = .(lon, lat)]

evap_slopes <- evap_datasets[
  , {
    if (.N > 3) {
      s = sens.slope(x = value)
      mk = mk.test(x = value)
      list(
        sen_slope = s$estimates,
        p_value = mk$p.value
      )
    } else {
      list(
        sen_slope = NA_real_,
        p_value = NA_real_
      )
    }
  },
  by = .(lon, lat, dataset)
]

slope_summary <- copy(evap_slopes)
slope_summary <- slope_summary[, ens_slope_median := median(sen_slope, na.rm = TRUE), .(lon, lat)]
slope_summary[, significant := p_value < 0.1]
slope_summary[, slope_sign := sign(sen_slope)]

agreement_summary <- slope_summary[significant == TRUE, .(
  n_significant = .N,
  n_pos = sum(slope_sign > 0),
  n_neg = sum(slope_sign < 0)
), by = .(lon, lat)]
agreement_summary[, majority_significant := n_significant > 4] 
agreement_summary[majority_significant == TRUE, majority_agrees := abs(n_neg - n_pos) > 2] 

ens_slopes <- slope_summary[, .(lon, lat, ens_slope_median)]
ens_slopes <- ens_slopes[!duplicated(ens_slopes)]

evap_ensemble_stats <- merge(
  ensemble_stats, 
  ens_slopes, 
  by = c("lon", "lat"), 
  all.x = TRUE
)

evap_ensemble_stats <- merge(
  evap_ensemble_stats, 
  agreement_summary, 
  by = c("lon", "lat"), 
  all.x = TRUE
)

saveRDS(evap_ensemble_stats, file = paste0(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))
saveRDS(evap_datasets, file = paste0(PATH_OUTPUT_DATA, 'evap_ensemble.Rds'))
saveRDS(evap_slopes, file = paste0(PATH_OUTPUT_DATA, 'evap_ensemble_slopes.Rds'))
