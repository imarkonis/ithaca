# Libraries ====================================================================

library(parallel)
source('source/twc_change.R')

# Inputs =======================================================================

prec_evap_change <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap_change.Rds'))[, .(
  lon, lat, dataset, prec_change, evap_change, prec, evap, prec_evap)]
avail_flux <-  readRDS(file = file.path(PATH_OUTPUT_DATA, 
                                        'avail_flux_change.rds'))
weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, 
                                'weights_region_biome.Rds'))
masks <- pRecipe::pRecipe_masks()

# Constants & Variables ========================================================

set.seed(42)
n_sims <- 1000

weights_region <- weights_dt[, .(scenario, region, biome, dataset, w_region_biome, biome_fraction)]
weights_region <- weights_region[is.finite(w_region_biome) & !is.na(w_region_biome) & w_region_biome > 0]

twc_change <- merge(prec_evap_change, avail_flux)
twc_change <- merge(twc_change, masks[, .(lon, lat, region = ipcc_short_region, biome = biome_short_class)], 
      by = c('lon', 'lat'))

# Analysis =====================================================================

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
    sel
  }, mc.cores = max(1L, detectCores() - 4L))
)


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
      
      prec_change    = mean(prec_change,  na.rm = TRUE),
      evap_change    = mean(evap_change,  na.rm = TRUE),
      avail_change   = mean(avail_change, na.rm = TRUE),
      flux_change    = mean(flux_change,  na.rm = TRUE),
      
      biome_fraction = first(i.biome_fraction)
    ),
    by = .EACHI
  ]
}

close(pb)

mc_world <- rbindlist(out_list, use.names = TRUE)

region_sim_means <- mc_world[, .(
  prec_change  = weighted.mean(prec_change,  w = biome_fraction, na.rm = TRUE),
  evap_change  = weighted.mean(evap_change,  w = biome_fraction, na.rm = TRUE),
  avail_change = weighted.mean(avail_change, w = biome_fraction, na.rm = TRUE),
  flux_change  = weighted.mean(flux_change,  w = biome_fraction, na.rm = TRUE),
  n_biomes     = .N
), by = .(scenario, sim, region)]
setorder(region_sim_means, sim, region, scenario)

twc_dataset <- mc_world[, .(
  prec_change  = weighted.mean(prec_change,  w = biome_fraction, na.rm = TRUE),
  evap_change  = weighted.mean(evap_change,  w = biome_fraction, na.rm = TRUE),
  avail_change = weighted.mean(avail_change, w = biome_fraction, na.rm = TRUE),
  flux_change  = weighted.mean(flux_change,  w = biome_fraction, na.rm = TRUE),
  n_biomes     = .N
), by = .(scenario, region)]
setorder(twc_dataset, region, scenario)


# Outputs ======================================================================

saveRDS(mc_world, 
        file.path(PATH_OUTPUT_DATA, 'mc_ensemble.Rds'))
saveRDS(region_sim_means, 
        file.path(PATH_OUTPUT_DATA, 'mc_ensemble_means.Rds'))
saveRDS(twc_dataset, 
        file.path(PATH_OUTPUT_DATA, 'twc_dataset.Rds'))

