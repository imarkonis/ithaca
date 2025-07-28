source('source/twc_change.R')

prec_evap_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_evap_stats.Rds'))
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))

ARIDITY_THRES <- 0.4
WETNESS_THRES <- 3

prec_evap_stats[, pe_ratio := prec_mean / evap_mean]
prec_evap_stats[, pe_ratio_check := TRUE]
prec_evap_stats[pe_ratio < 0.4 | pe_ratio > 3, pe_ratio_check := FALSE]

prec_evap_stats[pe_ratio_check == T, .N, dataset]
prec_evap_stats[pe_ratio_check == F, .N, dataset]


dataset_ranks <- merge(dataset_ranks, prec_evap_stats[, .(lon, lat, dataset, pe_ratio_check)])

saveRDS(dataset_ranks, file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))    
