# ============================================================================
# Estimate ensemble mean, variability, and trend diagnostics for precipitation
# and evaporation datasets in the TWC change workflow.
#
# This script:
#   1) selects the ensemble precipitation and evaporation datasets
#   2) estimates per-dataset grid-cell means and standard deviations
#   3) computes ensemble median mean and variability
#   4) estimates Sen slopes and Mann-Kendall p-values per dataset and grid cell
#   5) summarizes ensemble slope agreement
#   6) saves the ensemble datasets, slopes, and summary statistics
# ============================================================================

# Libraries ==================================================================

source("source/twc_change.R")

library(trend)

# Constants & Variables =======================================================

P_VALUE_THRESHOLD <- 0.1
MIN_YEARS_FOR_TREND <- diff(FULL_PERIOD) - 4

prec_evap_raw <- read_fst(
  file.path(PATH_OUTPUT_RAW, "other/prec_evap_raw.fst"),
  as.data.table = TRUE
)

# Functions ==================================================================

estimate_ensemble_products <- function(dt, dataset_names) {
  dt_use <- copy(dt[dataset %in% dataset_names])
  dt_use[, variable := NULL]
  
  dataset_stats <- dt_use[
    ,
    .(
      dataset_mean = mean(value, na.rm = TRUE),
      dataset_sd = sd(value, na.rm = TRUE)
    ),
    by = .(lon, lat, dataset)
  ]
  
  ensemble_stats <- dataset_stats[
    ,
    .(
      ens_median = median(dataset_mean, na.rm = TRUE),
      ens_sd = median(dataset_sd, na.rm = TRUE)
    ),
    by = .(lon, lat)
  ]
  
  dataset_slopes <- dt_use[
    ,
    {
      if (.N >= MIN_YEARS_FOR_TREND) {
        s <- sens.slope(x = value)
        mk <- mk.test(x = value)
        
        list(
          sen_slope = as.numeric(s$estimates),
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
  
  slope_summary <- copy(dataset_slopes)
  slope_summary[, significant := p_value < P_VALUE_THRESHOLD]
  slope_summary[, slope_sign := sign(sen_slope)]
  
  ens_slopes <- slope_summary[
    ,
    .(
      ens_slope_median = median(sen_slope, na.rm = TRUE)
    ),
    by = .(lon, lat)
  ]
  
  agreement_summary <- slope_summary[
    significant == TRUE,
    .(
      n_significant = .N,
      n_pos = sum(slope_sign > 0, na.rm = TRUE),
      n_neg = sum(slope_sign < 0, na.rm = TRUE)
    ),
    by = .(lon, lat)
  ]
  
  agreement_summary[
    ,
    majority_significant := n_significant > floor(length(dataset_names) / 2)
  ]
  
  agreement_summary[
    majority_significant == TRUE,
    majority_agrees := pmax(n_pos, n_neg) > floor(n_significant / 2)
  ]
  
  ensemble_products <- merge(
    ensemble_stats,
    ens_slopes,
    by = c("lon", "lat"),
    all.x = TRUE
  )
  
  ensemble_products <- merge(
    ensemble_products,
    agreement_summary,
    by = c("lon", "lat"),
    all.x = TRUE
  )
  
  setorder(dt_use, dataset, lon, lat, year)
  setorder(dataset_slopes, dataset, lon, lat)
  
  return(list(
    datasets = dt_use,
    slopes = dataset_slopes,
    stats = ensemble_products
  ))
}

# Analysis ===================================================================

## Precipitation ensemble
prec_ensemble <- estimate_ensemble_products(
  dt = prec_evap_raw[variable == "prec"],
  dataset_names = PREC_ENSEMBLE_NAMES_SHORT
)

## Evaporation ensemble 
evap_ensemble <- estimate_ensemble_products(
  dt = prec_evap_raw[variable == "evap"],
  dataset_names = EVAP_ENSEMBLE_NAMES_SHORT
)

# Outputs ===================================================================

saveRDS(
  prec_ensemble$stats,
  file.path(PATH_OUTPUT_DATA, "prec_ensemble_stats.Rds")
)

saveRDS(
  prec_ensemble$datasets,
  file.path(PATH_OUTPUT_DATA, "prec_ensemble.Rds")
)

saveRDS(
  prec_ensemble$slopes,
  file.path(PATH_OUTPUT_DATA, "prec_ensemble_slopes.Rds")
)
saveRDS(
  evap_ensemble$stats,
  file.path(PATH_OUTPUT_DATA, "evap_ensemble_stats.Rds")
)

saveRDS(
  evap_ensemble$datasets,
  file.path(PATH_OUTPUT_DATA, "evap_ensemble.Rds")
)

saveRDS(
  evap_ensemble$slopes,
  file.path(PATH_OUTPUT_DATA, "evap_ensemble_slopes.Rds")
)