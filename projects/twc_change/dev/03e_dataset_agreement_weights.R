# ============================================================================
# Compute dataset agreement weights for twc_change under multiple weighting
# scenarios.
#
# This script:
# 1. Applies hard physics filters
# 2. Converts loss metrics to probabilities within each grid cell
# 3. Combines climate and trend metrics for precipitation and evaporation
# 4. Builds scenario specific final weights
# 5. Exports the base scenario and the tidy scenario comparison table
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, "dataset_ranks.Rds"))

# Helpers =====================================================================

normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & !is.na(x) & (x > 0)
  
  if (!any(ok)) {
    return(out)
  }
  
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

weighted_row_mean <- function(..., w) {
  mat <- do.call(cbind, list(...))
  
  if (ncol(mat) != length(w)) {
    stop("Length of weights must match number of inputs.")
  }
  
  apply(
    mat,
    1,
    function(x) {
      ok <- is.finite(x) & !is.na(x)
      if (!any(ok)) {
        return(NA_real_)
      }
      stats::weighted.mean(x[ok], w[ok])
    }
  )
}

pair_w <- function(a) c(a, 1 - a)

run_weight_scenarios <- function(dt, scenarios) {
  out <- lapply(
    names(scenarios),
    function(s) {
      message("Running scenario: ", s)
      res <- compute_weights(copy(dt), scenarios[[s]])
      res[, scenario := s]
      res
    }
  )
  
  names(out) <- names(scenarios)
  out
}

# Constants ===================================================================

WEIGHT_SCENARIOS <- list(
  base = list(
    PREC_CLIM = 0.5,
    EVAP_CLIM = 0.5,
    PREC_TREND = 0.7,
    EVAP_TREND = 0.7,
    PREC = 0.3,
    EVAP = 0.3,
    FINAL_PREC = 0.45
  ),
  trend_dominant = list(
    PREC_CLIM = 0.5,
    EVAP_CLIM = 0.5,
    PREC_TREND = 0.7,
    EVAP_TREND = 0.7,
    PREC = 0.1,
    EVAP = 0.1,
    FINAL_PREC = 0.45
  ),
  clim_dominant = list(
    PREC_CLIM = 0.5,
    EVAP_CLIM = 0.5,
    PREC_TREND = 0.7,
    EVAP_TREND = 0.7,
    PREC = 0.9,
    EVAP = 0.9,
    FINAL_PREC = 0.45
  ),
  evap_dominant = list(
    PREC_CLIM = 0.5,
    EVAP_CLIM = 0.5,
    PREC_TREND = 0.7,
    EVAP_TREND = 0.7,
    PREC = 0.3,
    EVAP = 0.3,
    FINAL_PREC = 0.10
  ),
  prec_dominant = list(
    PREC_CLIM = 0.5,
    EVAP_CLIM = 0.5,
    PREC_TREND = 0.7,
    EVAP_TREND = 0.7,
    PREC = 0.3,
    EVAP = 0.3,
    FINAL_PREC = 0.90
  )
)

BASE_OUTPUT_COLS <- c(
  "lon", "lat", "dataset",
  "weight_prec_mean", "weight_prec_sd",
  "weight_evap_mean", "weight_evap_sd",
  "weight_prec_slope", "weight_evap_slope",
  "weight_prec_sig", "weight_evap_sig",
  "weight_prec_clim", "weight_evap_clim",
  "weight_prec_trend", "weight_evap_trend",
  "weight_prec", "weight_evap",
  "weight"
)

# Functions ===================================================================

compute_weights <- function(dt, scenario) {
  dt <- copy(dt)[pe_ratio_check == TRUE & n_below_pet > 6]
  
  dt[, weight_prec_mean  := loss_to_prob(prec_mean_bias),  by = .(lon, lat)]
  dt[, weight_prec_sd    := loss_to_prob(prec_sd_bias),    by = .(lon, lat)]
  dt[, weight_evap_mean  := loss_to_prob(evap_mean_bias),  by = .(lon, lat)]
  dt[, weight_evap_sd    := loss_to_prob(evap_sd_bias),    by = .(lon, lat)]
  dt[, weight_prec_slope := loss_to_prob(prec_bias_slope), by = .(lon, lat)]
  dt[, weight_evap_slope := loss_to_prob(evap_bias_slope), by = .(lon, lat)]
  
  dt[, weight_prec_sig := {
    raw <- fcoalesce(
      fifelse(
        !is.na(prec_check_significance),
        fifelse(prec_check_significance, 1, 0.10),
        NA_real_
      ),
      fifelse(
        !is.na(prec_check_non_significance),
        fifelse(prec_check_non_significance, 1, 0.10),
        NA_real_
      )
    )
    normalize_prob(raw)
  }, by = .(lon, lat)]
  
  dt[, weight_evap_sig := {
    raw <- fcoalesce(
      fifelse(
        !is.na(evap_check_significance),
        fifelse(evap_check_significance, 1, 0.10),
        NA_real_
      ),
      fifelse(
        !is.na(evap_check_non_significance),
        fifelse(evap_check_non_significance, 1, 0.10),
        NA_real_
      )
    )
    normalize_prob(raw)
  }, by = .(lon, lat)]
  
  dt[, weight_prec_clim := weighted_row_mean(
    weight_prec_mean,
    weight_prec_sd,
    w = pair_w(scenario$PREC_CLIM)
  )]
  
  dt[, weight_evap_clim := weighted_row_mean(
    weight_evap_mean,
    weight_evap_sd,
    w = pair_w(scenario$EVAP_CLIM)
  )]
  
  dt[, weight_prec_trend := weighted_row_mean(
    weight_prec_sig,
    weight_prec_slope,
    w = pair_w(scenario$PREC_TREND)
  )]
  
  dt[, weight_evap_trend := weighted_row_mean(
    weight_evap_sig,
    weight_evap_slope,
    w = pair_w(scenario$EVAP_TREND)
  )]
  
  dt[, weight_prec := weighted_row_mean(
    weight_prec_clim,
    weight_prec_trend,
    w = pair_w(scenario$PREC)
  )]
  
  dt[, weight_evap := weighted_row_mean(
    weight_evap_clim,
    weight_evap_trend,
    w = pair_w(scenario$EVAP)
  )]
  
  dt[, weight := weighted_row_mean(
    weight_prec,
    weight_evap,
    w = pair_w(scenario$FINAL_PREC)
  )]
  
  dt[]
}

# Analysis ====================================================================

weights_by_scenario <- run_weight_scenarios(
  dt = dataset_ranks,
  scenarios = WEIGHT_SCENARIOS
)

weights_by_scenario_tidy <- rbindlist(
  lapply(
    names(weights_by_scenario),
    \(s) weights_by_scenario[[s]][
      ,
      .(lon, lat, dataset, weight, scenario = s)
    ]
  ),
  use.names = TRUE
)

weights_base <- weights_by_scenario_tidy[scenario == 'base']

# Outputs =====================================================================

saveRDS(
  weights_by_scenario_tidy,
  file.path(PATH_OUTPUT_DATA, "dataset_weights.Rds")
)

# Validation ==================================================================

weights_by_scenario_tidy[, mean(weight), dataset]

top_weight_map <- weights_by_scenario_tidy[
  ,
  {
    max_weight <- max(weight, na.rm = TRUE)
    ii <- which(weight == max_weight)
    
    .(
      dataset_top = dataset[ii[1]],
      weight_top = max_weight,
      n_ties = length(ii)
    )
  },
  by = .(lon, lat)
]

top_weight_map[, tied := n_ties > 1]

ggplot(top_weight_map) +
  geom_raster(aes(x = lon, y = lat, fill = dataset_top)) +
  borders("world", colour = "grey20", linewidth = 0.2) +
  coord_equal(expand = FALSE) +
  labs(
    title = "Dominant dataset by grid cell",
    subtitle = "Dataset with the highest agreement weight",
    x = NULL,
    y = NULL,
    fill = "Dataset"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.4, "cm")
  )



lon_test_a <- 9.875
lon_test_b <- 69.875
lat_test <- 35.125

weights_base[lon == lon_test_a & lat == lat_test]
weights_base[lon == lon_test_b & lat == lat_test]

dcast(
  weights_by_scenario_tidy[lon == lon_test_a & lat == lat_test],
  lon + lat + dataset ~ scenario,
  value.var = "weight",
  fun.aggregate = ''
)

dcast(
  weights_by_scenario_tidy[lon == lon_test_b & lat == lat_test],
  lon + lat + dataset ~ scenario,
  value.var = "weight"
)

aa <- dataset_ranks[
  ,
  .(N = sum(n_below_pet > 6, na.rm = TRUE)),
  by = .(lon, lat)
]

table(aa$N)