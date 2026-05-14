# ============================================================================
# Compare availability trends with GRACE
#
# Metrics:
#   1. Overall sign agreement
#   2. Sign agreement where both trends are significant at p <= 0.05
#   3. Fraction of statistically significant availability trends
#
# Period:
#   2002-2021
#
# Applies to:
#   1. Monte Carlo ensemble members
#   2. Individual P/E datasets
# ============================================================================

source("source/twc_change.R")

library(data.table)
library(trend)

# Inputs ======================================================================

mc_region_slopes_2002_2021 <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_2002_2021.Rds")
)

grace_region_slopes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "grace_region_slopes.Rds")
)

dataset_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_yearly_prec_evap.Rds")
)

mc_region_slopes_2002_2021 <- as.data.table(mc_region_slopes_2002_2021)
grace_region_slopes <- as.data.table(grace_region_slopes)
dataset_region_yearly <- as.data.table(dataset_region_yearly)

# Constants & Variables =======================================================

period_start <- 2002
period_end <- 2021
P_THRES <- 0.05

# Functions ===================================================================

safe_sen <- function(x) {
  if (sum(is.finite(x)) < 5) return(NA_real_)
  as.numeric(sens.slope(x)$estimates)
}

safe_mk_p <- function(x) {
  if (sum(is.finite(x)) < 5) return(NA_real_)
  mk.test(x)$p.value
}

safe_sign <- function(x) {
  fifelse(x > 0, 1L, fifelse(x < 0, -1L, 0L))
}

summarize_grace_agreement <- function(dt, by_cols) {
  dt[
    ,
    .(
      n_regions = .N,
      overall_sign_agreement = mean(sign_agree, na.rm = TRUE),
      n_both_sig_95 = sum(both_sig_95, na.rm = TRUE),
      significant_sign_agreement_95 = fifelse(
        sum(both_sig_95, na.rm = TRUE) > 0,
        mean(sign_agree[both_sig_95], na.rm = TRUE),
        NA_real_
      ),
      avail_sig_95_fraction = mean(avail_sig_95, na.rm = TRUE)
    ),
    by = by_cols
  ]
}

# Analysis ====================================================================

## Prepare GRACE slopes --------------------------------------------------------

grace_compare <- grace_region_slopes[
  ,
  .(
    region,
    grace_slope = slope,
    grace_mk_p = mk_p_value
  )
][
  is.finite(grace_slope)
]

grace_compare[
  ,
  `:=`(
    grace_sign = safe_sign(grace_slope),
    grace_sig_95 = grace_mk_p <= P_THRES
  )
]

## Monte Carlo comparison ------------------------------------------------------

mc_grace_avail <- merge(
  mc_region_slopes_2002_2021[
    ,
    .(
      scenario,
      sim_id,
      region,
      avail_slope,
      avail_mk_p
    )
  ],
  grace_compare,
  by = "region"
)

mc_grace_avail[
  ,
  `:=`(
    avail_sign = safe_sign(avail_slope),
    avail_sig_95 = avail_mk_p <= P_THRES
  )
]

mc_grace_avail[
  ,
  `:=`(
    sign_agree = avail_sign == grace_sign,
    both_sig_95 = avail_sig_95 & grace_sig_95
  )
]

mc_member_grace_agreement <- summarize_grace_agreement(
  mc_grace_avail,
  by_cols = c("scenario", "sim_id")
)

setorder(
  mc_member_grace_agreement,
  -overall_sign_agreement,
  -significant_sign_agreement_95
)

mc_scenario_grace_agreement <- mc_member_grace_agreement[
  ,
  .(
    n_members = .N,
    mean_overall_sign_agreement = mean(overall_sign_agreement, na.rm = TRUE),
    mean_significant_sign_agreement_95 = mean(
      significant_sign_agreement_95,
      na.rm = TRUE
    ),
    mean_n_both_sig_95 = mean(n_both_sig_95, na.rm = TRUE),
    mean_avail_sig_95_fraction = mean(avail_sig_95_fraction, na.rm = TRUE)
  ),
  by = scenario
]

setorder(
  mc_scenario_grace_agreement,
  -mean_overall_sign_agreement
)

mc_region_grace_agreement <- mc_grace_avail[
  ,
  .(
    n_members = .N,
    grace_slope = first(grace_slope),
    grace_mk_p = first(grace_mk_p),
    grace_sig_95 = first(grace_sig_95),
    grace_sign = first(grace_sign),
    overall_sign_agreement = mean(sign_agree, na.rm = TRUE),
    n_both_sig_95 = sum(both_sig_95, na.rm = TRUE),
    significant_sign_agreement_95 = fifelse(
      sum(both_sig_95, na.rm = TRUE) > 0,
      mean(sign_agree[both_sig_95], na.rm = TRUE),
      NA_real_
    ),
    avail_sig_95_fraction = mean(avail_sig_95, na.rm = TRUE)
  ),
  by = region
]

setorder(
  mc_region_grace_agreement,
  -overall_sign_agreement
)

## Dataset comparison ----------------------------------------------------------

dataset_region_yearly[
  ,
  avail := prec - evap
]

dataset_region_avail_slopes <- dataset_region_yearly[
  year >= period_start & year <= period_end,
  .(
    avail_slope = safe_sen(avail),
    avail_mk_p = safe_mk_p(avail),
    n_years = sum(is.finite(avail))
  ),
  by = .(dataset, region)
]

dataset_region_avail_slopes[
  ,
  `:=`(
    avail_sign = safe_sign(avail_slope),
    avail_sig_95 = avail_mk_p <= P_THRES
  )
]

dataset_grace_avail <- merge(
  dataset_region_avail_slopes,
  grace_compare,
  by = "region"
)

dataset_grace_avail[
  ,
  `:=`(
    sign_agree = avail_sign == grace_sign,
    both_sig_95 = avail_sig_95 & grace_sig_95
  )
]

dataset_grace_agreement <- summarize_grace_agreement(
  dataset_grace_avail,
  by_cols = "dataset"
)

setorder(
  dataset_grace_agreement,
  -overall_sign_agreement,
  -significant_sign_agreement_95
)

dataset_region_grace_agreement <- dataset_grace_avail[
  ,
  .(
    n_datasets = .N,
    grace_slope = first(grace_slope),
    grace_mk_p = first(grace_mk_p),
    grace_sig_95 = first(grace_sig_95),
    grace_sign = first(grace_sign),
    overall_sign_agreement = mean(sign_agree, na.rm = TRUE),
    n_both_sig_95 = sum(both_sig_95, na.rm = TRUE),
    significant_sign_agreement_95 = fifelse(
      sum(both_sig_95, na.rm = TRUE) > 0,
      mean(sign_agree[both_sig_95], na.rm = TRUE),
      NA_real_
    ),
    avail_sig_95_fraction = mean(avail_sig_95, na.rm = TRUE)
  ),
  by = region
]

setorder(
  dataset_region_grace_agreement,
  -overall_sign_agreement
)

# Outputs =====================================================================

saveRDS(
  mc_member_grace_agreement,
  file.path(PATH_OUTPUT_DATA, "mc_member_grace_availability_agreement_2002_2021.Rds")
)

saveRDS(
  mc_scenario_grace_agreement,
  file.path(PATH_OUTPUT_DATA, "mc_scenario_grace_availability_agreement_2002_2021.Rds")
)

saveRDS(
  mc_region_grace_agreement,
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_availability_agreement_2002_2021.Rds")
)

saveRDS(
  dataset_region_avail_slopes,
  file.path(PATH_OUTPUT_DATA, "dataset_region_availability_slopes_2002_2021.Rds")
)

saveRDS(
  dataset_grace_agreement,
  file.path(PATH_OUTPUT_DATA, "dataset_grace_availability_agreement_2002_2021.Rds")
)

saveRDS(
  dataset_region_grace_agreement,
  file.path(PATH_OUTPUT_DATA, "dataset_region_grace_availability_agreement_2002_2021.Rds")
)

# Validation ==================================================================

mc_scenario_grace_agreement
dataset_grace_agreement
mc_region_grace_agreement
dataset_region_grace_agreement