source('source/twc_change.R')

prec_ensemble_stats <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
evap_ensemble_stats <- readRDS(paste0(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))
prec_slopes <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_ensemble_slopes.Rds'))
evap_slopes <- readRDS(paste0(PATH_OUTPUT_DATA, 'evap_ensemble_slopes.Rds'))

prec_slopes <- prec_slopes[dataset %in% PREC_NAMES_SHORT]
evap_slopes <- evap_slopes[dataset %in% EVAP_NAMES_SHORT]

prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
prec_stats <-  prec_evap[, .(prec_median = median(prec), prec_cv = sd(prec) / mean(prec)), .(lon, lat, dataset)]
evap_stats <-  prec_evap[year >= 2000, .(evap_median = median(evap), evap_cv = sd(evap) / mean(evap)), .(lon, lat, dataset)]

dataset_stats <- merge(prec_stats, evap_stats, by = c('lon', 'lat', 'dataset'))

prec_comparison <- merge(dataset_stats, prec_ensemble_stats)
evap_comparison <- merge(dataset_stats, evap_ensemble_stats)
evap_comparison <- merge(evap_comparison, evap_slopes, by = c('lon', 'lat', 'dataset'))

prec_comparison[, diff_median :=  abs(prec_median - ens_median)]
prec_comparison[, diff_cv :=  abs(prec_cv - ens_cv)]
prec_comparison[, rank_median := frank(diff_median, ties.method = "min"), by = .(lon, lat)]
prec_comparison[, rank_cv     := frank(diff_cv,     ties.method = "min"), by = .(lon, lat)]
prec_comparison[, prec_ranks := rank_median + rank_cv]
prec_comparison[, mean(prec_ranks), dataset]


evap_comparison[, prob_weight := dnorm(evap_median, mean = ens_median, sd = ens_cv * ens_median), .(lon, lat, dataset)]
evap_comparison[, prob_weight_norm := prob_weight / sum(prob_weight), by = .(lon, lat)]

evap_comparison[, diff_median :=  abs(evap_median - ens_median)]
evap_comparison[, diff_cv :=  abs(evap_cv - ens_cv)]
evap_comparison[, slope_penalty := 0]
evap_comparison[slope * ens_slope_median < 0, slope_penalty := 1]

evap_comparison[, rank_median := frank(diff_median, ties.method = "min"), by = .(lon, lat)]
evap_comparison[, rank_cv     := frank(diff_cv, ties.method = "min"), by = .(lon, lat)]
evap_comparison[, evap_ranks := rank_median + rank_cv + slope_penalty]
evap_comparison[, mean(evap_ranks), dataset]
