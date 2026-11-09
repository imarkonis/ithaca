# ============================================================================
# Compare regional Monte Carlo slopes between 1982-2001 and 2002-2021
# ============================================================================

# Libraries ===================================================================

library(data.table)

# Inputs ======================================================================

mc_region_slopes_1982_2001 <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_1982_2001.Rds")
)

mc_region_slopes_2002_2021 <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_2002_2021.Rds")
)

# Constants & Variables =======================================================

P_THRES <- 0.05

slope_vars <- c("prec", "evap", "avail", "flux")

# Functions ===================================================================

sign_class <- function(x) {
  fifelse(
    is.na(x), NA_character_,
    fifelse(
      x > 0, "positive",
      fifelse(x < 0, "negative", "zero")
    )
  )
}

trend_change_class <- function(slope_old, slope_new) {
  fifelse(
    is.na(slope_old) | is.na(slope_new), NA_character_,
    fifelse(
      slope_old > 0 & slope_new > 0 & slope_new > slope_old, "positive_strengthening",
      fifelse(
        slope_old > 0 & slope_new > 0 & slope_new < slope_old, "positive_weakening",
        fifelse(
          slope_old < 0 & slope_new < 0 & slope_new < slope_old, "negative_strengthening",
          fifelse(
            slope_old < 0 & slope_new < 0 & slope_new > slope_old, "negative_weakening",
            fifelse(
              slope_old <= 0 & slope_new > 0, "shift_to_positive",
              fifelse(
                slope_old >= 0 & slope_new < 0, "shift_to_negative",
                "no_change"
              )
            )
          )
        )
      )
    )
  )
}

prepare_period_slopes <- function(dt, period_label) {
  
  dt <- copy(dt)
  
  keep_cols <- c(
    "scenario", "sim_id", "region",
    paste0(slope_vars, "_slope"),
    paste0(slope_vars, "_mk_p"),
    paste0(slope_vars, "_sig")
  )
  
  dt <- dt[, ..keep_cols]
  
  setnames(
    dt,
    old = paste0(slope_vars, "_slope"),
    new = paste0(slope_vars, "_slope_", period_label)
  )
  
  setnames(
    dt,
    old = paste0(slope_vars, "_mk_p"),
    new = paste0(slope_vars, "_mk_p_", period_label)
  )
  
  setnames(
    dt,
    old = paste0(slope_vars, "_sig"),
    new = paste0(slope_vars, "_sig_", period_label)
  )
  
  dt
}

# Analysis ====================================================================

## Prepare period-specific tables

slopes_1982_2001 <- prepare_period_slopes(
  mc_region_slopes_1982_2001,
  "1982_2001"
)

slopes_2002_2021 <- prepare_period_slopes(
  mc_region_slopes_2002_2021,
  "2002_2021"
)

## Join periods

mc_region_slope_comparison <- merge(
  slopes_1982_2001,
  slopes_2002_2021,
  by = c("scenario", "sim_id", "region"),
  all = TRUE
)

## Add slope differences and trend-change classes

for (v in slope_vars) {
  
  slope_old <- paste0(v, "_slope_1982_2001")
  slope_new <- paste0(v, "_slope_2002_2021")
  
  sig_old <- paste0(v, "_sig_1982_2001")
  sig_new <- paste0(v, "_sig_2002_2021")
  
  diff_col <- paste0(v, "_slope_diff")
  absdiff_col <- paste0(v, "_slope_absdiff")
  sign_old_col <- paste0(v, "_sign_1982_2001")
  sign_new_col <- paste0(v, "_sign_2002_2021")
  sign_agree_col <- paste0(v, "_sign_agree")
  class_col <- paste0(v, "_trend_change")
  sig_both_col <- paste0(v, "_sig_both")
  sig_any_col <- paste0(v, "_sig_any")
  
  mc_region_slope_comparison[
    ,
    c(
      diff_col,
      absdiff_col,
      sign_old_col,
      sign_new_col,
      sign_agree_col,
      class_col,
      sig_both_col,
      sig_any_col
    ) := list(
      get(slope_new) - get(slope_old),
      abs(get(slope_new) - get(slope_old)),
      sign_class(get(slope_old)),
      sign_class(get(slope_new)),
      sign(get(slope_old)) == sign(get(slope_new)),
      trend_change_class(get(slope_old), get(slope_new)),
      get(sig_old) & get(sig_new),
      get(sig_old) | get(sig_new)
    )
  ]
}

setorder(mc_region_slope_comparison, scenario, sim_id, region)

## Regional summary across Monte Carlo simulations

mc_region_slope_comparison_summary <- mc_region_slope_comparison[
  ,
  .(
    n_sim = .N,
    
    prec_slope_1982_2001_mean = mean(prec_slope_1982_2001, na.rm = TRUE),
    prec_slope_2002_2021_mean = mean(prec_slope_2002_2021, na.rm = TRUE),
    prec_slope_diff_mean = mean(prec_slope_diff, na.rm = TRUE),
    prec_pr_sign_agree = mean(prec_sign_agree, na.rm = TRUE),
    prec_pr_sig_both = mean(prec_sig_both, na.rm = TRUE),
    prec_pr_sig_any = mean(prec_sig_any, na.rm = TRUE),
    
    evap_slope_1982_2001_mean = mean(evap_slope_1982_2001, na.rm = TRUE),
    evap_slope_2002_2021_mean = mean(evap_slope_2002_2021, na.rm = TRUE),
    evap_slope_diff_mean = mean(evap_slope_diff, na.rm = TRUE),
    evap_pr_sign_agree = mean(evap_sign_agree, na.rm = TRUE),
    evap_pr_sig_both = mean(evap_sig_both, na.rm = TRUE),
    evap_pr_sig_any = mean(evap_sig_any, na.rm = TRUE),
    
    avail_slope_1982_2001_mean = mean(avail_slope_1982_2001, na.rm = TRUE),
    avail_slope_2002_2021_mean = mean(avail_slope_2002_2021, na.rm = TRUE),
    avail_slope_diff_mean = mean(avail_slope_diff, na.rm = TRUE),
    avail_pr_sign_agree = mean(avail_sign_agree, na.rm = TRUE),
    avail_pr_sig_both = mean(avail_sig_both, na.rm = TRUE),
    avail_pr_sig_any = mean(avail_sig_any, na.rm = TRUE),
    
    flux_slope_1982_2001_mean = mean(flux_slope_1982_2001, na.rm = TRUE),
    flux_slope_2002_2021_mean = mean(flux_slope_2002_2021, na.rm = TRUE),
    flux_slope_diff_mean = mean(flux_slope_diff, na.rm = TRUE),
    flux_pr_sign_agree = mean(flux_sign_agree, na.rm = TRUE),
    flux_pr_sig_both = mean(flux_sig_both, na.rm = TRUE),
    flux_pr_sig_any = mean(flux_sig_any, na.rm = TRUE)
  ),
  by = .(scenario, region)
]

setorder(mc_region_slope_comparison_summary, scenario, region)

## Long-format class summary

trend_change_long <- melt(
  mc_region_slope_comparison,
  id.vars = c("scenario", "sim_id", "region"),
  measure.vars = patterns(
    trend_change = "_trend_change$"
  ),
  variable.name = "variable",
  value.name = "trend_change"
)

trend_change_long[
  ,
  variable := gsub("_trend_change", "", variable)
]

trend_change_summary <- trend_change_long[
  !is.na(trend_change),
  .(
    n = .N
  ),
  by = .(scenario, region, variable, trend_change)
]

trend_change_summary[
  ,
  prop := n / sum(n),
  by = .(scenario, region, variable)
]

setorder(trend_change_summary, scenario, region, variable, -prop)

## Dominant trend-change class per region

dominant_trend_change <- trend_change_summary[
  order(-prop),
  .SD[1],
  by = .(scenario, region, variable)
]

setorder(dominant_trend_change, scenario, region, variable)

# Outputs =====================================================================

saveRDS(
  mc_region_slope_comparison,
  file.path(PATH_OUTPUT_DATA, "mc_region_slope_comparison_1982_2001_vs_2002_2021.Rds")
)

saveRDS(
  mc_region_slope_comparison_summary,
  file.path(PATH_OUTPUT_DATA, "mc_region_slope_comparison_summary_1982_2001_vs_2002_2021.Rds")
)

saveRDS(
  trend_change_summary,
  file.path(PATH_OUTPUT_DATA, "mc_region_trend_change_summary_1982_2001_vs_2002_2021.Rds")
)

saveRDS(
  dominant_trend_change,
  file.path(PATH_OUTPUT_DATA, "mc_region_dominant_trend_change_1982_2001_vs_2002_2021.Rds")
)

# Validation ==================================================================

print(mc_region_slope_comparison)

print(mc_region_slope_comparison_summary)

print(dominant_trend_change)
