source('source/twc_change.R')

dataset_weights <- readRDS(paste0(PATH_OUTPUT, 'dataset_pair_weights.rds'))
avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))

avail_flux_change <- merge(dataset_weights, avail_flux_change, by = c("lon", "lat", "dataset_pair"))

avail_flux_change_weighted <- avail_flux_change[, .(avail_change = sum(avail_change * weight), 
                                                    flux_change = sum(flux_change * weight)), 
                                                .(lon, lat)]
avail_flux_change_weighted <- avail_flux_change_weighted[complete.cases(avail_flux_change_weighted)]

masks <- pRecipe::pRecipe_masks()
results_classification <- merge(avail_flux_change_weighted, masks[land_mask == 'land']) 



#Plots
to_plot <- copy(results_classification)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, water_avail_flux)

ggplot(to_plot[KG_class_1 != "Ocean"]) +
  geom_bar(aes(KG_class_1, fill = Conditions), position="fill") +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(to_plot) +
  geom_bar(aes(biome_short_class , fill = Conditions), position="fill") +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

