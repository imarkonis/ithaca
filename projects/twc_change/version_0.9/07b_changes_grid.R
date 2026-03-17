source('source/avail_flux_change.R')
avail_flux <-  readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_period_mean.Rds'))

water_avail <- avail_flux[, .(water_avail = mean(avail)), .(lon, lat, dataset_pair, period)]
water_avail_wide <- dcast(water_avail, lon + lat + dataset_pair ~ period )
water_avail_wide[, avail_change := aft_2001 - pre_2001]

water_flux <- avail_flux[, .(water_flux = mean(flux)), .(lon, lat, dataset_pair, period)]
water_flux_wide <- dcast(water_flux, lon + lat + dataset_pair ~ period )
water_flux_wide[, flux_change := aft_2001 - pre_2001]

water_avail_flux  <- merge(water_avail, water_flux, 
                           by = c("lon", "lat", "dataset_pair", "period"), allow.cartesian = TRUE)
avail_flux_change <- merge(water_avail_wide[, .(lon, lat, dataset_pair, avail_change)], 
                    water_flux_wide[, .(lon, lat, dataset_pair, flux_change)], 
                    by = c("lon", "lat", "dataset_pair"), allow.cartesian = TRUE)

saveRDS(water_avail_flux, file = paste0(PATH_OUTPUT, 'avail_flux_grid.rds'))
saveRDS(avail_flux_change, file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))

#Plots
to_plot <- copy(avail_flux_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Drier - Accelerated', 'Wetter - Deccelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, water_avail_flux)

ggplot(to_plot) +
  geom_bar(aes(dataset_pair, fill = Conditions), position="fill") +
  scale_fill_manual(values = WATER_CYCLE_CHANGE_PALETTE[c(1, 3, 2, 4)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

