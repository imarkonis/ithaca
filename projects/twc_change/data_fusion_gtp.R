prec_evap_change <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap_change.Rds'))[, .(
  lon, lat, dataset, prec_change, evap_change, prec, evap, prec_evap)]

# grid metadata from dt (one row per lon-lat)
grid_meta <- unique(dt[, .(lon, lat, ipcc_short_region, biome, area_w)])

# merge into change table
chg <- merge(
  prec_evap_change,
  grid_meta,
  by = c("lon", "lat"),
  all.x = TRUE
)

# sanity
stopifnot(all(c("ipcc_short_region","biome","area_w") %in% names(chg)))

wmean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w = w[ok], na.rm = TRUE)
}

rb_change <- chg[
  ,
  .(
    dP_biome = wmean_safe(prec_change, area_w),
    dE_biome = wmean_safe(evap_change, area_w),
    n_cells  = .N
  ),
  by = .(ipcc_short_region, biome, dataset)
]

mc_reg_change <- merge(
  mc_choices[, .(ipcc_short_region, biome, sim, dataset, A_region_biome)],
  rb_change,
  by = c("ipcc_short_region","biome","dataset"),
  all.x = TRUE
)

# region totals per simulation (biome mixture)
mc_reg_change <- mc_reg_change[
  ,
  .(
    dP = sum(A_region_biome * dP_biome, na.rm = TRUE),
    dE = sum(A_region_biome * dE_biome, na.rm = TRUE),
    cov_biomes = mean(is.finite(dP_biome) & is.finite(dE_biome))
  ),
  by = .(ipcc_short_region, sim)
]

# derived
mc_reg_change[, `:=`(
  dP_minus_E = dP - dE,
  dP_plus_E  = dP + dE
)]

mc_reg_change[]

reg_change_summary <- mc_reg_change[
  ,
  .(
    dP_med = median(dP, na.rm = TRUE),
    dP_q05 = quantile(dP, 0.05, na.rm = TRUE),
    dP_q95 = quantile(dP, 0.95, na.rm = TRUE),
    
    dE_med = median(dE, na.rm = TRUE),
    dE_q05 = quantile(dE, 0.05, na.rm = TRUE),
    dE_q95 = quantile(dE, 0.95, na.rm = TRUE),
    
    dPmE_med = median(dP_minus_E, na.rm = TRUE),
    dPmE_q05 = quantile(dP_minus_E, 0.05, na.rm = TRUE),
    dPmE_q95 = quantile(dP_minus_E, 0.95, na.rm = TRUE),
    
    dPpE_med = median(dP_plus_E, na.rm = TRUE),
    dPpE_q05 = quantile(dP_plus_E, 0.05, na.rm = TRUE),
    dPpE_q95 = quantile(dP_plus_E, 0.95, na.rm = TRUE)
  ),
  by = ipcc_short_region
]

reg_change_summary[]
