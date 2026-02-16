# --- Load libraries and sources ---
library(trend)
source('source/twc_change.R')

prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
prec_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
evap_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))
prec_slopes <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_ensemble_slopes.Rds'))
evap_slopes <- readRDS(file.path(PATH_OUTPUT_DATA, 'evap_ensemble_slopes.Rds'))
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))

extra_prec_datasets <- prec_evap[dataset == "GLEAM", .(lon, lat, prec)]
extra_prec_slopes <- extra_prec_datasets[
  , {
    if (.N > 1) {
      s = sens.slope(x = prec)
      mk = mk.test(x = prec)
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
  by = .(lon, lat)
]
extra_prec_slopes$dataset <- "GLEAM"
setcolorder(extra_prec_slopes, c('lon', 'lat', 'dataset', 'sen_slope', 'p_value'))
prec_comparison <-  merge(rbind(prec_slopes[dataset %in% PREC_NAMES_SHORT], extra_prec_slopes), prec_ensemble_stats)

prec_comparison[, check_significance := NA]
prec_comparison[, check_non_significance := NA]
prec_comparison[majority_significant == TRUE & majority_agrees == TRUE & p_value < 0.1, check_significance := TRUE]
prec_comparison[majority_significant == TRUE & majority_agrees == TRUE & p_value > 0.1, check_significance := FALSE]
prec_comparison[majority_significant == TRUE & majority_agrees == TRUE & p_value > 0.1, check_significance := FALSE]
prec_comparison[majority_significant == FALSE & p_value > 0.1, check_non_significance := TRUE]
prec_comparison[majority_significant == FALSE & p_value < 0.1, check_non_significance := FALSE]
prec_comparison[, diff_slope := abs(sen_slope - ens_slope_median)]
prec_comparison[, bias_slope := abs(sen_slope - ens_slope_median) / ens_slope_median]
prec_comparison[, rank_slope := frank(diff_slope, ties.method = "min"), by = .(lon, lat)]

dataset_ranks <- merge(dataset_ranks, prec_comparison[, .(lon, lat, dataset, prec_check_significance = check_significance, 
                                         prec_check_non_significance = check_non_significance, prec_bias_slope = bias_slope, prec_rank_slope = rank_slope)],
      by = c('lon', 'lat', 'dataset'))

evap_comparison <- merge(evap_slopes[dataset %in% EVAP_NAMES_SHORT], evap_ensemble_stats)
evap_comparison[, check_significance := NA]
evap_comparison[, check_non_significance := NA]
evap_comparison[majority_significant == TRUE & majority_agrees == TRUE & p_value < 0.1, check_significance := TRUE]
evap_comparison[majority_significant == TRUE & majority_agrees == TRUE & p_value > 0.1, check_significance := FALSE]
evap_comparison[majority_significant == TRUE & majority_agrees == TRUE & p_value > 0.1, check_significance := FALSE]
evap_comparison[majority_significant == FALSE & p_value > 0.1, check_non_significance := TRUE]
evap_comparison[majority_significant == FALSE & p_value < 0.1, check_non_significance := FALSE]
evap_comparison[, diff_slope := abs(sen_slope - ens_slope_median)]
evap_comparison[, bias_slope := abs(sen_slope - ens_slope_median) / ens_slope_median]
evap_comparison[, rank_slope := frank(diff_slope, ties.method = "min"), by = .(lon, lat)]

dataset_ranks <- merge(dataset_ranks, evap_comparison[, .(lon, lat, dataset, evap_check_significance = check_significance, 
                    evap_check_non_significance = check_non_significance, evap_bias_slope = bias_slope, evap_rank_slope = rank_slope)],
      by = c('lon', 'lat', 'dataset'))

saveRDS(dataset_ranks, file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))    
