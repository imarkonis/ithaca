source('source/twc_change.R')

# --- Read in Data ---
prec_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
evap_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))
prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_evap.Rds'))


## ANALYSIS
# --- Calculate summary statistics per dataset/gridcell ---
prec_stats <- prec_evap[, .(prec_mean = mean(prec), prec_sd = sd(prec)), .(lon, lat, dataset)]
evap_stats <- prec_evap[year >= 2000, .(evap_mean = mean(evap), evap_sd = sd(evap)), .(lon, lat, dataset)]

dataset_stats <- merge(prec_stats, evap_stats, by = c('lon', 'lat', 'dataset'))

# --- Compare precipitation datasets ---
prec_comparison <- merge(prec_stats, prec_ensemble_stats)
prec_comparison[, bias_mean := abs(prec_mean - ens_median) / ens_median] #ens_median is the ensemble median of the dataset means
prec_comparison[, bias_sd := abs(prec_sd - ens_sd) / ens_sd]
prec_comparison[, rank_mean := frank(bias_mean, ties.method = "min"), by = .(lon, lat)]
prec_comparison[, rank_sd := frank(bias_sd, ties.method = "min"), by = .(lon, lat)]
prec_comparison[, prec_ranks := rank_mean + rank_sd]
prec_comparison[, best_dataset := dataset[which.min(prec_ranks)], by = .(lon, lat)]

# --- Compare evaporation datasets ---
evap_comparison <- merge(evap_stats, evap_ensemble_stats)
evap_comparison[, bias_mean := abs(evap_mean - ens_median) / ens_median]
evap_comparison[, bias_sd := abs(evap_sd - ens_sd) / ens_sd]
evap_comparison[, rank_mean := frank(bias_mean, ties.method = "min"), by = .(lon, lat)]
evap_comparison[, rank_sd := frank(bias_sd, ties.method = "min"), by = .(lon, lat)]

dataset_ranks <- merge(prec_comparison[, .(lon, lat, dataset, prec_mean_bias = bias_mean, prec_sd_bias = bias_sd,
                                          prec_mean_rank = rank_mean, prec_sd_rank = rank_sd)], 
      evap_comparison[, .(lon, lat, dataset, evap_mean_bias = bias_mean, evap_sd_bias = bias_sd,
                          evap_mean_rank = rank_mean, evap_sd_rank = rank_sd)],
      by = c("lon", "lat", "dataset"))

saveRDS(dataset_ranks, file = paste0(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))
saveRDS(dataset_stats, file.path(PATH_OUTPUT_DATA, 'prec_evap_stats.Rds'))

