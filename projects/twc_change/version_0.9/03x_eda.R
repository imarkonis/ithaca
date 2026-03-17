# Libraries ====================================================================

source('source/twc_change.R')

# Inputs =======================================================================

prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))

weights_region_biome <- readRDS(file.path(PATH_OUTPUT_DATA, 
                                          'weights_region_biome.Rds'))
masks <- pRecipe::pRecipe_masks()

# Constants & Variables ========================================================

prec_evap_regions <- merge(prec_evap, masks[land_mask == 'land', 
                                            .(lon, lat, region = ipcc_short_region, 
                                              biome = biome_short_class)], 
                           by = c('lon', 'lat')) 
prec_evap_regions_means <- prec_evap_regions[, .(prec = mean(prec, na.rm = FALSE),
                                                 evap = mean(evap, na.rm = FALSE)),
                                             .(year, region, biome, dataset)]

# Helpers ======================================================================

# Analysis =====================================================================

to_plot <- prec_evap_regions_means[region == 'CAU']

ggplot(to_plot) +
  geom_line(aes(year, prec, col = dataset)) +
  facet_wrap(~biome) +
  theme_light()

ggplot(to_plot) +
  geom_line(aes(year, evap, col = dataset)) +
  facet_wrap(~biome) +
  theme_light()

to_plot <- prec_evap_regions_means[region == 'MED']

ggplot(to_plot) +
  geom_line(aes(year, prec, col = dataset)) +
  facet_wrap(~biome) +
  theme_light()

ggplot(to_plot) +
  geom_line(aes(year, evap, col = dataset)) +
  facet_wrap(~biome) +
  theme_light()










test <- make_prob_wide(weights_region_biome, reg = 'CAU')
