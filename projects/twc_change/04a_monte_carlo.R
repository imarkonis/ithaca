# Libraries ====================================================================
install.packages('doSNOW')

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

weights_region <- weights_dt[, .(scenario, region, biome, dataset, prob = w_region_biome)]
weights_region <- weights_region[is.finite(prob) & !is.na(prob) & prob > 0]

twc_change <- merge(prec_evap_change, avail_flux)
twc_change <- merge(twc_change, masks[, .(lon, lat, region = ipcc_short_region, biome = biome_short_class)], 
      by = c('lon', 'lat'))

# Analysis =====================================================================

biome_frac <- twc_change[, .(n_cells = .N), by = .(region, biome, dataset)]
biome_frac[, n_region := sum(n_cells), by = .(region, dataset)]
biome_frac[, area_region_biome := n_cells / n_region]

mc_ensemble <- rbindlist(mclapply(seq_len(n_sims), function(s) {
  sel <- weights_region[, .(dataset = sample(dataset, size = 1, prob = prob)),
                        by = .(scenario, region, biome)]
  sel[, sim := s]
  sel
}, mc.cores = max(1L, detectCores() - 4L)))

twc_change_ensemble <- twc_change[mc_ensemble, on = .(dataset, region, biome), 
                                  nomatch = 0L, allow.cartesian = TRUE]

test <- mc_ensemble[sim == 1]

twc_change_ensemble <- twc_change[test, on = .(dataset, region, biome), 
                                  nomatch = 0L, allow.cartesian = TRUE]

pb <- txtProgressBar(min = 0, max = n_sims, style = 3)

out_list <- vector("list", n_sims)

for (s in seq_len(n_sims)) {
  
  setTxtProgressBar(pb, s) # progress
  dummy <- mc_ensemble[sim == s, .(scenario, sim, region, biome, dataset)]
  
  out_list[[s]] <- twc_change[dummy,
                              on = .(dataset, region, biome),
                              nomatch = 0L,
                              allow.cartesian = TRUE,
                              .(
                                scenario = i.scenario,
                                sim      = i.sim,
                                
                                prec_change     = mean(prec_change, na.rm = TRUE),
                                evap_change     = mean(evap_change, na.rm = TRUE),
                                avail_change    = mean(avail_change, na.rm = TRUE),
                                flux_change     = mean(flux_change, na.rm = TRUE),
                                n_cells_rb = .N
                              ),
                              by = .EACHI]
}
close(pb)

mc_world <- rbindlist(out_list, use.names = TRUE)
mc_world <- mc_world[biome_frac, on = .(region, biome, dataset), nomatch = 0L]

region_sim_means <- mc_world[, .(
  prec_change  = weighted.mean(prec_change,  w = area_region_biome, na.rm = TRUE),
  evap_change  = weighted.mean(evap_change,  w = area_region_biome, na.rm = TRUE),
  avail_change = weighted.mean(avail_change, w = area_region_biome, na.rm = TRUE),
  flux_change  = weighted.mean(flux_change,  w = area_region_biome, na.rm = TRUE),
  n_biomes     = .N
), by = .(scenario, sim, region)]
setorder(region_sim_means, sim, region, scenario)

# Outputs ======================================================================

saveRDS(region_sim_means, 
        file.path(PATH_OUTPUT_DATA, 'mc_ensemble.Rds'))
