# ============================================================================
# Estimate regional Monte Carlo slopes by simulation
#
# This script:
# 1. Estimates regional Monte Carlo trends for each scenario, simulation,
#    region, and period using:
#    a) Sen's slope
#    b) Mann-Kendall significance test
# 2. Flags statistically significant trends for precipitation, evaporation,
#    availability, and flux
# 3. Saves regional Monte Carlo slope summaries separately for:
#    a) 1982-2001
#    b) 2002-2021
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

library(trend)

# Inputs ======================================================================

mc_region_yearly_by_sim <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_yearly_prec_evap.Rds")
)

# Constants & Variables =======================================================

periods <- list(
  "1982_2001" = c(1982L, 2001L),
  "2002_2021" = c(2002L, 2021L)
)

P_THRES <- 0.05

# Functions ===================================================================

estimate_slopes <- function(dt) {
  
  dt <- dt[order(year)]
  
  if (nrow(dt) < 5) {
    return(data.table(
      prec_slope = NA_real_,
      evap_slope = NA_real_,
      avail_slope = NA_real_,
      flux_slope = NA_real_,
      prec_mk_p = NA_real_,
      evap_mk_p = NA_real_,
      avail_mk_p = NA_real_,
      flux_mk_p = NA_real_,
      n_years = nrow(dt),
      year_start = ifelse(nrow(dt) > 0, min(dt$year), NA_integer_),
      year_end = ifelse(nrow(dt) > 0, max(dt$year), NA_integer_)
    ))
  }
  
  safe_sen <- function(x) {
    if (sum(!is.na(x)) < 5) return(NA_real_)
    as.numeric(sens.slope(x)$estimates)
  }
  
  safe_mk <- function(x) {
    if (sum(!is.na(x)) < 5) return(NA_real_)
    mk.test(x)$p.value
  }
  
  data.table(
    prec_slope = safe_sen(dt$prec),
    evap_slope = safe_sen(dt$evap),
    avail_slope = safe_sen(dt$avail),
    flux_slope = safe_sen(dt$flux),
    
    prec_mk_p = safe_mk(dt$prec),
    evap_mk_p = safe_mk(dt$evap),
    avail_mk_p = safe_mk(dt$avail),
    flux_mk_p = safe_mk(dt$flux),
    
    n_years = nrow(dt),
    year_start = min(dt$year),
    year_end = max(dt$year)
  )
}

estimate_mc_region_slopes <- function(dt, year_start, year_end) {
  
  dt_period <- dt[
    year >= year_start & year <= year_end
  ]
  
  slopes <- dt_period[
    ,
    estimate_slopes(.SD),
    by = .(scenario, sim_id, region)
  ]
  
  slopes[
    ,
    `:=`(
      period = paste0(year_start, "_", year_end),
      prec_sig = prec_mk_p < P_THRES,
      evap_sig = evap_mk_p < P_THRES,
      avail_sig = avail_mk_p < P_THRES,
      flux_sig = flux_mk_p < P_THRES
    )
  ]
  
  setcolorder(
    slopes,
    c(
      "period", "scenario", "sim_id", "region",
      "prec_slope", "evap_slope", "avail_slope", "flux_slope",
      "prec_mk_p", "evap_mk_p", "avail_mk_p", "flux_mk_p",
      "prec_sig", "evap_sig", "avail_sig", "flux_sig",
      "n_years", "year_start", "year_end"
    )
  )
  
  setorder(slopes, period, scenario, sim_id, region)
  
  slopes
}

# Analysis ====================================================================

## Add derived variables

mc_region_yearly_by_sim[
  ,
  `:=`(
    avail = prec - evap,
    flux = (prec + evap) / 2
  )
]

## Estimate Monte Carlo regional slopes for both periods

mc_region_slopes_1982_2001 <- estimate_mc_region_slopes(
  dt = mc_region_yearly_by_sim,
  year_start = 1982L,
  year_end = 2001L
)

mc_region_slopes_2002_2021 <- estimate_mc_region_slopes(
  dt = mc_region_yearly_by_sim,
  year_start = 2002L,
  year_end = 2021L
)

# Outputs =====================================================================

saveRDS(
  mc_region_slopes_1982_2001,
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_1982_2001.Rds")
)

saveRDS(
  mc_region_slopes_2002_2021,
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_2002_2021.Rds")
)


# Validation ==================================================================

print(mc_region_slopes_1982_2001)
print(mc_region_slopes_2002_2021)