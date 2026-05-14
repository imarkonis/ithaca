# ============================================================================
# Estimate regional Monte Carlo slopes by simulation, 2002-2021
# ============================================================================

library(data.table)
library(trend)

dataset_region_yearly <- readRDS(file.path(PATH_OUTPUT_DATA,  #DO ALSO THIS!
                                           "dataset_region_yearly_prec_evap.Rds"))

mc_region_yearly_by_sim <- readRDS(file.path(PATH_OUTPUT_DATA, 
                                             "mc_region_yearly_prec_evap.Rds"))

# Add derived variables =======================================================

mc_region_yearly_by_sim[
  ,
  `:=`(
    avail = prec - evap,
    flux = (prec + evap) / 2
  )
]

# Keep target period ==========================================================

mc_region_yearly_2002_2021 <- mc_region_yearly_by_sim[
  year >= 2002 & year <= 2021
]

# Slope helper ================================================================

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

# Estimate slopes per scenario, simulation, and region ========================

mc_region_slopes_2002_2021 <- mc_region_yearly_2002_2021[
  ,
  estimate_slopes(.SD),
  by = .(scenario, sim_id, region)
]

# Add significance flags ======================================================

mc_region_slopes_2002_2021[
  ,
  `:=`(
    prec_sig = prec_mk_p < 0.05,
    evap_sig = evap_mk_p < 0.05,
    avail_sig = avail_mk_p < 0.05,
    flux_sig = flux_mk_p < 0.05
  )
]

setorder(mc_region_slopes_2002_2021, scenario, sim_id, region)

# Save output =================================================================

saveRDS(
  mc_region_slopes_2002_2021,
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_2002_2021.Rds")
)

# Quick check =================================================================

print(mc_region_slopes_2002_2021)