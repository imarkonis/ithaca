# Libraries ====================================================================

library(data.table)
library(parallel)
source("source/twc_change.R")

# Inputs =======================================================================

prec_evap_change <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap_change.Rds"))[, .(
  lon, lat, dataset, prec_change, evap_change, prec, evap, prec_evap
)]

avail_flux <- readRDS(file.path(PATH_OUTPUT_DATA, "avail_flux_change.rds"))

weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds"))

masks <- pRecipe::pRecipe_masks()

# Constants ====================================================================

set.seed(42)
n_sims <- 50

# Data prep ====================================================================

weights_region <- weights_dt[,
  .(scenario, region, biome, dataset, w_region_biome, biome_fraction)
][
  is.finite(w_region_biome) & !is.na(w_region_biome) & w_region_biome > 0
]

twc_change <- merge(
  prec_evap_change,
  avail_flux,
  by = c("lon", "lat", "dataset"),
  all = FALSE
)

twc_change <- merge(
  twc_change,
  masks[, .(lon, lat, region = ipcc_short_region, biome = biome_short_class)],
  by = c("lon", "lat"),
  all = FALSE
)

# Keep only valid base-scenario combinations
twc_change <- twc_change[
  weights_region,
  on = .(dataset, region, biome),
  allow.cartesian=TRUE
]

# Monte Carlo dataset selection =================================================

mc_ensemble <- rbindlist(
  mclapply(seq_len(n_sims), function(s) {
    
    sel <- weights_region[, {
      idx <- sample(.N, size = 1, prob = w_region_biome)
      .(
        dataset        = dataset[idx],
        biome_fraction = biome_fraction[idx]
      )
    }, by = .(scenario, region, biome)]
    
    sel[, sim := s]
    sel[]
  }, mc.cores = max(1L, detectCores() - 4L))
)

# Aggregate to region-biome per simulation =====================================

pb <- txtProgressBar(min = 0, max = n_sims, style = 3)

out_list <- vector("list", n_sims)

for (s in seq_len(n_sims)) {
  
  setTxtProgressBar(pb, s)
  
  dummy <- mc_ensemble[
    sim == s,
    .(scenario, sim, region, biome, dataset, biome_fraction)
  ]
  
  out_list[[s]] <- twc_change[
    dummy,
    on = .(dataset, region, biome),
    nomatch = 0L,
    allow.cartesian = TRUE,
    .(
      scenario       = i.scenario,
      sim            = i.sim,
      region         = i.region,
      biome          = i.biome,
      dataset        = i.dataset,
      biome_fraction = first(i.biome_fraction),
      
      prec_change    = mean(prec_change,  na.rm = TRUE),
      evap_change    = mean(evap_change,  na.rm = TRUE),
      avail_change   = mean(avail_change, na.rm = TRUE),
      flux_change    = mean(flux_change,  na.rm = TRUE)
    ),
    by = .EACHI
  ]
}

close(pb)

mc_world <- rbindlist(out_list, use.names = TRUE)

# Main analysis dataset: region x sim ==========================================

region_sim_means <- mc_world[, .(
  prec_change  = weighted.mean(prec_change,  w = biome_fraction, na.rm = TRUE),
  evap_change  = weighted.mean(evap_change,  w = biome_fraction, na.rm = TRUE),
  avail_change = weighted.mean(avail_change, w = biome_fraction, na.rm = TRUE),
  flux_change  = weighted.mean(flux_change,  w = biome_fraction, na.rm = TRUE),
  n_biomes     = .N
), by = .(scenario, sim, region)]

setorder(region_sim_means, sim, region)

# Plot-ready summary ============================================================

twc_dataset <- region_sim_means[, .(
  prec_med   = median(prec_change,  na.rm = TRUE),
  evap_med   = median(evap_change,  na.rm = TRUE),
  avail_med  = median(avail_change, na.rm = TRUE),
  flux_med   = median(flux_change,  na.rm = TRUE),
  
  prec_lo    = quantile(prec_change, 0.10, na.rm = TRUE),
  prec_hi    = quantile(prec_change, 0.90, na.rm = TRUE),
  evap_lo    = quantile(evap_change, 0.10, na.rm = TRUE),
  evap_hi    = quantile(evap_change, 0.90, na.rm = TRUE),
  avail_lo   = quantile(avail_change, 0.10, na.rm = TRUE),
  avail_hi   = quantile(avail_change, 0.90, na.rm = TRUE),
  flux_lo    = quantile(flux_change, 0.10, na.rm = TRUE),
  flux_hi    = quantile(flux_change, 0.90, na.rm = TRUE),
  
  n_sims     = .N
), by = .(scenario, region)]

# Outputs ======================================================================

saveRDS(mc_world, 
        file.path(PATH_OUTPUT_DATA, 'mc_ensemble.Rds'))
saveRDS(region_sim_means, 
        file.path(PATH_OUTPUT_DATA, 'mc_ensemble_means.Rds'))
saveRDS(twc_dataset, 
        file.path(PATH_OUTPUT_DATA, 'twc_dataset.Rds'))

