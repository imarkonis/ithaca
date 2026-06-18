# ============================================================================
# Dataset agreement weights for twc_change under multiple weighting scenarios.
#   1. physics filter  2. per-cell loss -> probability  3. hierarchical blend
#   4. five scenarios  5. export tidy weights, base detail, sampling diagnostics
# Slope biases are non-negative magnitudes (fixed upstream); all six bias
# metrics are therefore treated identically as relative-bias losses.
# ============================================================================

source("source/twc_change.R")

dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, "dataset_ranks.Rds"))


# Constants ===================================================================

WEIGHT_SCENARIOS <- list(
  base           = list(PREC_CLIM_MEAN_SHARE = 0.5, EVAP_CLIM_MEAN_SHARE = 0.5,
                        PREC_TREND_SIG_SHARE = 0.7, EVAP_TREND_SIG_SHARE = 0.7,
                        PREC_CLIM_SHARE = 0.5, EVAP_CLIM_SHARE = 0.5, FINAL_PREC_SHARE = 0.5),
  trend_dominant = list(PREC_CLIM_MEAN_SHARE = 0.5, EVAP_CLIM_MEAN_SHARE = 0.5,
                        PREC_TREND_SIG_SHARE = 0.7, EVAP_TREND_SIG_SHARE = 0.7,
                        PREC_CLIM_SHARE = 0.1, EVAP_CLIM_SHARE = 0.1, FINAL_PREC_SHARE = 0.5),
  clim_dominant  = list(PREC_CLIM_MEAN_SHARE = 0.5, EVAP_CLIM_MEAN_SHARE = 0.5,
                        PREC_TREND_SIG_SHARE = 0.7, EVAP_TREND_SIG_SHARE = 0.7,
                        PREC_CLIM_SHARE = 0.9, EVAP_CLIM_SHARE = 0.9, FINAL_PREC_SHARE = 0.5),
  evap_dominant  = list(PREC_CLIM_MEAN_SHARE = 0.5, EVAP_CLIM_MEAN_SHARE = 0.5,
                        PREC_TREND_SIG_SHARE = 0.7, EVAP_TREND_SIG_SHARE = 0.7,
                        PREC_CLIM_SHARE = 0.3, EVAP_CLIM_SHARE = 0.3, FINAL_PREC_SHARE = 0.10),
  prec_dominant  = list(PREC_CLIM_MEAN_SHARE = 0.5, EVAP_CLIM_MEAN_SHARE = 0.5,
                        PREC_TREND_SIG_SHARE = 0.7, EVAP_TREND_SIG_SHARE = 0.7,
                        PREC_CLIM_SHARE = 0.3, EVAP_CLIM_SHARE = 0.3, FINAL_PREC_SHARE = 0.90)
)

# loss-weight column  <-  source bias column (all treated as non-negative loss)
LOSS_WEIGHT_MAP <- c(
  weight_prec_mean  = "prec_mean_bias",  weight_prec_sd    = "prec_sd_bias",
  weight_evap_mean  = "evap_mean_bias",  weight_evap_sd    = "evap_sd_bias",
  weight_prec_slope = "prec_bias_slope", weight_evap_slope = "evap_bias_slope"
)

# ordered hierarchy: out <- weighted_pair(x, y, scenario[[share]]); later rows depend on earlier
COMBINE_SPEC <- list(
  list(out = "weight_prec_clim",  x = "weight_prec_mean", y = "weight_prec_sd",    share = "PREC_CLIM_MEAN_SHARE"),
  list(out = "weight_evap_clim",  x = "weight_evap_mean", y = "weight_evap_sd",    share = "EVAP_CLIM_MEAN_SHARE"),
  list(out = "weight_prec_trend", x = "weight_prec_sig",  y = "weight_prec_slope", share = "PREC_TREND_SIG_SHARE"),
  list(out = "weight_evap_trend", x = "weight_evap_sig",  y = "weight_evap_slope", share = "EVAP_TREND_SIG_SHARE"),
  list(out = "weight_prec",       x = "weight_prec_clim", y = "weight_prec_trend", share = "PREC_CLIM_SHARE"),
  list(out = "weight_evap",       x = "weight_evap_clim", y = "weight_evap_trend", share = "EVAP_CLIM_SHARE"),
  list(out = "weight",            x = "weight_prec",      y = "weight_evap",       share = "FINAL_PREC_SHARE")
)

REQUIRED_COLS <- c(
  "lon", "lat", "dataset", "pe_ratio_check", "n_below_pet", unname(LOSS_WEIGHT_MAP),
  "prec_check_significance", "prec_check_non_significance",
  "evap_check_significance", "evap_check_non_significance"
)

BASE_OUTPUT_COLS <- c(
  "lon", "lat", "dataset", names(LOSS_WEIGHT_MAP), "weight_prec_sig", "weight_evap_sig",
  "weight_prec_clim", "weight_evap_clim", "weight_prec_trend", "weight_evap_trend",
  "weight_prec", "weight_evap", "weight"
)


# Functions ===================================================================

validate_input <- function(dt) {
  missing <- setdiff(REQUIRED_COLS, names(dt))
  if (length(missing)) stop("Missing required columns: ", toString(missing))
  
  # Fail loud if any bias metric is signed: a silent drop here was the original bug.
  neg <- names(LOSS_WEIGHT_MAP)[vapply(
    unname(LOSS_WEIGHT_MAP), \(c) any(dt[[c]] < 0, na.rm = TRUE), logical(1))]
  if (length(neg)) stop("Negative values in loss metric(s): ", toString(LOSS_WEIGHT_MAP[neg]))
  invisible(TRUE)
}

normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & x > 0
  if (!any(ok)) return(out)
  s <- sum(x[ok])
  out[ok] <- if (is.finite(s) && s > 0) x[ok] / s else 1 / sum(ok)
  out
}

loss_to_prob <- function(loss, eps = 1e-6) {
  q <- rep(NA_real_, length(loss))
  ok <- is.finite(loss)
  q[ok] <- 1 / (eps + loss[ok])
  normalize_prob(q)
}

significance_to_prob <- function(sig, non_sig, fail = 0.10) {
  raw <- fcoalesce(
    fifelse(!is.na(sig),     fifelse(sig, 1, fail),     NA_real_),
    fifelse(!is.na(non_sig), fifelse(non_sig, 1, fail), NA_real_)
  )
  normalize_prob(raw)
}

# convex combo a*x + (1-a)*y, NA-aware (drops the missing side and renormalizes a)
weighted_pair_mean <- function(x, y, a) {
  ox <- is.finite(x); oy <- is.finite(y)
  num <- fifelse(ox, a * x, 0) + fifelse(oy, (1 - a) * y, 0)
  den <- fifelse(ox, a, 0)     + fifelse(oy, 1 - a, 0)
  fifelse(den > 0, num / den, NA_real_)
}

combine_pair_prob <- function(dt, out, x, y, a) {
  dt[, (out) := weighted_pair_mean(get(x), get(y), a)]
  dt[, (out) := normalize_prob(get(out)), by = .(lon, lat)]
  invisible(dt)
}

# Scenario-independent components: physics filter + per-cell loss/sig probabilities (computed once)
prep_components <- function(dt) {
  validate_input(dt)
  dt <- copy(dt)[pe_ratio_check == TRUE & n_below_pet > 6]
  for (out_col in names(LOSS_WEIGHT_MAP)) {
    dt[, (out_col) := loss_to_prob(get(LOSS_WEIGHT_MAP[[out_col]])), by = .(lon, lat)]
  }
  dt[, weight_prec_sig := significance_to_prob(prec_check_significance, prec_check_non_significance), by = .(lon, lat)]
  dt[, weight_evap_sig := significance_to_prob(evap_check_significance, evap_check_non_significance), by = .(lon, lat)]
  dt[]
}

# Scenario-dependent: walk the hierarchy
compute_weights <- function(components, scenario) {
  dt <- copy(components)
  for (s in COMBINE_SPEC) combine_pair_prob(dt, s$out, s$x, s$y, scenario[[s$share]])
  dt[]
}

run_weight_scenarios <- function(components, scenarios) {
  rbindlist(lapply(names(scenarios), function(nm) {
    message("Running scenario: ", nm)
    compute_weights(components, scenarios[[nm]])[, scenario := nm]
  }), use.names = TRUE)
}

# Per-cell sampling concentration. eff_n = inverse-Simpson effective n of datasets:
# a DIVERSITY measure, not confidence (a cell where all products are equally bad also reads high).
make_diagnostics <- function(dt) {
  dt[is.finite(weight),
     {
       top <- which.max(weight)
       .(n_candidates = .N, dataset_top = dataset[top],
         weight_top = weight[top], eff_n = 1 / sum(weight^2))
     },
     by = .(scenario, lon, lat)]
}


# Analysis ====================================================================

weights_all      <- run_weight_scenarios(prep_components(dataset_ranks), WEIGHT_SCENARIOS)
weights_tidy     <- weights_all[, .(lon, lat, dataset, weight, scenario)]
weights_base     <- weights_all[scenario == "base", ..BASE_OUTPUT_COLS]
weight_diag      <- make_diagnostics(weights_all)


# Outputs =====================================================================

saveRDS(weights_tidy, file.path(PATH_OUTPUT_DATA, "dataset_weights.Rds"))
saveRDS(weights_base, file.path(PATH_OUTPUT_DATA, "dataset_weights_base_detailed.Rds"))
saveRDS(weight_diag,  file.path(PATH_OUTPUT_DATA, "dataset_weight_diagnostics.Rds"))


# Validation ==================================================================

# weights must form a per-cell distribution
sums <- weights_tidy[is.finite(weight), .(s = sum(weight)), by = .(scenario, lon, lat)]
stopifnot(all(abs(sums$s - 1) < 1e-6))

print(weights_tidy[, .(mean_weight = mean(weight, na.rm = TRUE)),
                   by = .(scenario, dataset)][order(scenario, -mean_weight)])

if (interactive()) {
  library(ggplot2); library(maps); library(grid)
  
  top_base <- weight_diag[scenario == "prec_dominant"]
  print(
    ggplot(top_base) +
      geom_raster(aes(lon, lat, fill = dataset_top)) +
      borders("world", colour = "grey20", linewidth = 0.2) +
      coord_equal(expand = FALSE) +
      labs(title = "Dominant dataset by grid cell", subtitle = "Base scenario",
           x = NULL, y = NULL, fill = "Dataset") +
      theme(legend.position = "bottom", legend.key.width = unit(1.4, "cm"))
  )
}

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