# Libraries ====================================================================

source('source/twc_change.R')

# Inputs =======================================================================

dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))

# Helpers ======================================================================

normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & !is.na(x) & (x > 0)
  if (!any(ok)) return(out)
  s <- sum(x[ok])
  if (!is.finite(s) || s <= 0) {
    out[ok] <- 1 / sum(ok)
  } else {
    out[ok] <- x[ok] / s
  }
  out
}

loss_to_prob <- function(loss, eps = 1e-6) {
  q <- rep(NA_real_, length(loss))
  ok <- is.finite(loss) & !is.na(loss) & (loss >= 0)
  q[ok] <- 1 / (eps + loss[ok])
  normalize_prob(q)
}

pair_w <- function(a) c(a, 1 - a)
triple_w <- function(a, b, c) {
  s <- a + b + c
  c(a, b, c) / s
}

# Constants & Variables ========================================================

WEIGHT_SCENARIOS <- list(
  base = list(
    # inside-climate: mean vs sd
    PREC_CLIM_MEAN = 0.5,
    EVAP_CLIM_MEAN = 0.5,
    
    # inside-trend: sig vs slope
    PREC_TREND_SIG = 0.7,
    EVAP_TREND_SIG = 0.7,
    
    # internal per variable: clim vs trend
    PREC_INTERNAL_CLIM = 0.3,
    EVAP_INTERNAL_CLIM = 0.3,
    
    # final internal: P vs E vs PET-consistency
    INTERNAL_PREC = 0.35,
    INTERNAL_EVAP = 0.45,
    INTERNAL_PET  = 0.20
  ),
  
  trend_dominant = list(
    PREC_CLIM_MEAN = 0.5,
    EVAP_CLIM_MEAN = 0.5,
    PREC_TREND_SIG = 0.7,
    EVAP_TREND_SIG = 0.7,
    PREC_INTERNAL_CLIM = 0.1,  # 0.9 trend
    EVAP_INTERNAL_CLIM = 0.1,
    INTERNAL_PREC = 0.35,
    INTERNAL_EVAP = 0.45,
    INTERNAL_PET  = 0.20
  ),
  
  clim_dominant = list(
    PREC_CLIM_MEAN = 0.5,
    EVAP_CLIM_MEAN = 0.5,
    PREC_TREND_SIG = 0.7,
    EVAP_TREND_SIG = 0.7,
    PREC_INTERNAL_CLIM = 0.9,  
    EVAP_INTERNAL_CLIM = 0.9,
    INTERNAL_PREC = 0.35,
    INTERNAL_EVAP = 0.45,
    INTERNAL_PET  = 0.20
  ),
  
  evap_dominant = list(
    PREC_CLIM_MEAN = 0.5,
    EVAP_CLIM_MEAN = 0.5,
    PREC_TREND_SIG = 0.7,
    EVAP_TREND_SIG = 0.7,
    PREC_INTERNAL_CLIM = 0.3,
    EVAP_INTERNAL_CLIM = 0.3,
    INTERNAL_PREC = 0.10,
    INTERNAL_EVAP = 0.80,
    INTERNAL_PET  = 0.10
  ),
  
  prec_dominant = list(
    PREC_CLIM_MEAN = 0.5,
    EVAP_CLIM_MEAN = 0.5,
    PREC_TREND_SIG = 0.7,
    EVAP_TREND_SIG = 0.7,
    PREC_INTERNAL_CLIM = 0.3,
    EVAP_INTERNAL_CLIM = 0.3,
    INTERNAL_PREC = 0.80,
    INTERNAL_EVAP = 0.10,
    INTERNAL_PET  = 0.10
  )
)

# Functions ====================================================================

compute_weights <- function(dt, scenarios) {
  # hard physics gates
  dt <- dt[pe_ratio_check == TRUE]
  dt <- dt[n_below_pet > 0]
  
  # metric probabilities
  dt[, weight_prec_mean  := loss_to_prob(prec_mean_bias), by = .(lon, lat)]
  dt[, weight_prec_sd    := loss_to_prob(prec_sd_bias),   by = .(lon, lat)]
  dt[, weight_evap_mean  := loss_to_prob(evap_mean_bias), by = .(lon, lat)]
  dt[, weight_evap_sd    := loss_to_prob(evap_sd_bias),   by = .(lon, lat)]
  dt[, weight_prec_slope := loss_to_prob(prec_bias_slope), by = .(lon, lat)]
  dt[, weight_evap_slope := loss_to_prob(evap_bias_slope), by = .(lon, lat)]
  
  dt[, weight_prec_sig := {
    raw <- fcoalesce(
      fifelse(!is.na(prec_check_significance),
              fifelse(prec_check_significance, 1.0, 0.10), NA_real_),
      fifelse(!is.na(prec_check_non_significance),
              fifelse(prec_check_non_significance, 1.0, 0.10), NA_real_)
    )
    normalize_prob(raw)
  }, by = .(lon, lat)]
  
  dt[, weight_evap_sig := {
    raw <- fcoalesce(
      fifelse(!is.na(evap_check_significance),
              fifelse(evap_check_significance, 1.0, 0.10), NA_real_),
      fifelse(!is.na(evap_check_non_significance),
              fifelse(evap_check_non_significance, 1.0, 0.10), NA_real_)
    )
    normalize_prob(raw)
  }, by = .(lon, lat)]
  
  dt[, weight_pet := {
    raw <- fifelse(is.na(n_below_pet), NA_real_, pmax(n_below_pet, 0))
    normalize_prob(raw)
  }, by = .(lon, lat)]
  
  # aggregates with scenario weights
  dt[, weight_prec_clim := weighted_row_mean(weight_prec_mean, weight_prec_sd,
                                             w = pair_w(scenarios$PREC_CLIM_MEAN))]
  dt[, weight_evap_clim := weighted_row_mean(weight_evap_mean, weight_evap_sd,
                                             w = pair_w(scenarios$EVAP_CLIM_MEAN))]
  
  dt[, weight_prec_trend := weighted_row_mean(weight_prec_sig, weight_prec_slope,
                                              w = pair_w(scenarios$PREC_TREND_SIG))]
  dt[, weight_evap_trend := weighted_row_mean(weight_evap_sig, weight_evap_slope,
                                              w = pair_w(scenarios$EVAP_TREND_SIG))]
  
  dt[, weight_prec_internal := weighted_row_mean(weight_prec_clim, weight_prec_trend,
                                                 w = pair_w(scenarios$PREC_INTERNAL_CLIM))]
  dt[, weight_evap_internal := weighted_row_mean(weight_evap_clim, weight_evap_trend,
                                                 w = pair_w(scenarios$EVAP_INTERNAL_CLIM))]
  
  dt[, weight_internal := weighted_row_mean(
    weight_prec_internal, weight_evap_internal, weight_pet,
    w = triple_w(scenarios$INTERNAL_PREC, scenarios$INTERNAL_EVAP, scenarios$INTERNAL_PET)
  )]
  
  dt[]
}

run_scenarios <- function(dataset_ranks, scenarios,
                          cores = max(1, parallel::detectCores() - 3)) {
  nm <- names(scenarios)
  if (is.null(nm) || any(nm == "")) nm <- paste0("sc_", seq_along(scenarios))
  
  res <- parallel::mclapply(seq_along(scenarios), function(i) {
    dt_s <- compute_weights(dataset_ranks, scenarios[[i]])
    setDT(dt_s)
    dt_s[, scenario := nm[i]]
    dt_s
  }, mc.cores = cores)
  
  setNames(res, nm)
}

# Analysis =====================================================================

weights_by_scenario <- run_scenarios(dataset_ranks, SCENARIOS, 
                                     cores = length(WEIGHT_SCENARIOS) + 1)

weights_by_scenario_tidy <- 
  rbindlist(lapply(names(weights_by_scenario), \(s) 
                   weights_by_scenario[[s]][, .(lon, lat, dataset, 
                                                weight = weight_internal, 
                                                scenario = s)]), 
            use.names = TRUE, fill = TRUE)

weights_base <- compute_weights(dataset_ranks, WEIGHT_SCENARIOS$base)

# Outputs ======================================================================

saveRDS(weights_base[, c(1:3, 24:39)], 
        file.path(PATH_OUTPUT_DATA, 'dataset_agreement_weights_base.Rds'))
saveRDS(weights_by_scenario_tidy, 
        file.path(PATH_OUTPUT_DATA, 'dataset_agreement_weights.Rds'))

# Validation ===================================================================

lon_test_a <- 9.875 
lon_test_b <- 69.875 
lat_test <- 35.125

weights_base[lon == lon_test_a & lat == lat_test]
weights_base[lon == lon_test_b & lat == lat_test]

dcast(weights_by_scenario_tidy[lon == lon_test_a & lat == lat_test], 
      lon + lat + dataset ~ scenario, value.var = 'weight')
dcast(weights_by_scenario_tidy[lon == lon_test_b & lat == lat_test], 
      lon + lat + dataset ~ scenario, value.var = 'weight')

