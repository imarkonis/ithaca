source('source/twc_change.R')

ipcc_change_mean <- readRDS(file = paste0(PATH_OUTPUT, 'ipcc_change_mean.rds'))[
  , .(dataset, region, prec, evap, prec_evap, flux, avail, flux_avail)]

ipcc_change_mean <- 


#Plots 
to_plot <- copy(avail_flux_change)

to_plot <- merge(to_plot, water_avail_flux)
levels(to_plot$period) <-  c("1981-2000", "2001-2020")
setnames(to_plot, 'period', "Period")
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = dataset_pair, col = Conditions), 
            alpha = 0.5) +
  facet_wrap(~region, scales = 'free') +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = PALETTES$water_cycle_change) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/year]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/year]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  geom_text(data = to_plot[Period == "1981-2000"], aes(y = water_flux, x = water_avail, label = dataset_pair), 
            vjust = -3.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

ggplot(to_plot, aes(x = "", fill = Conditions)) +
  geom_bar(width = 1, position = "fill") +
  coord_polar(theta = "y") +
  facet_wrap(~region) +
  scale_fill_manual(values = PALETTES$water_cycle_change) +
  theme_void() +
  theme(strip.text = element_text(size = 10),
        legend.position = "right")


ipcc_hexagon <- data.table(read.csv("/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv"))[
  , .(region = Acronym, x = CENTROIX, y = CENTROIY)]
to_plot <- merge(to_plot, ipcc_hexagon, by = 'region', allow.cartesian = TRUE)
to_plot <- to_plot[complete.cases(to_plot)]

to_plot_pie <- merge(data.table(table(to_plot[, .(region, Conditions)])), unique(to_plot[, .(region, x, y)]), by = 'region')
to_plot_pie <- dcast(to_plot_pie, region + x + y ~ Conditions, value.var = 'N')


world <- map_data('world')
base_map <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()

base_map + 
  geom_scatterpie(data = to_plot_pie, aes(x, y, group = region), 
                  cols = c('Wetter - Accelerated', 'Wetter - Deccelerated',
                           'Drier - Accelerated', 'Drier - Deccelerated')) +
  scale_fill_manual(values = PALETTES$water_cycle_change) +
  theme_void() 

#AS ABOVE BUT FOR GRID CELLS
#Analysis
water_avail <- avail_flux[, .(water_avail = mean(avail)), .(lon, lat, dataset_pair, period, region)]
water_avail_wide <- dcast(water_avail, lon + lat + dataset_pair + region ~ period )
water_avail_wide[, avail_change := aft_2001 - pre_2001]

water_flux <- avail_flux[, .(water_flux = mean(flux)), .(lon, lat, dataset_pair, period, region)]
water_flux_wide <- dcast(water_flux, lon + lat + dataset_pair + region ~ period )
water_flux_wide[, flux_change := aft_2001 - pre_2001]

water_avail_flux  <- merge(water_avail, water_flux, 
                           by = c("lon", "lat", "dataset_pair", "period", "region"), allow.cartesian = TRUE)
avail_flux_change <- merge(water_avail_wide[, .(lon, lat, dataset_pair, avail_change, region)], 
                           water_flux_wide[, .(lon, lat, dataset_pair, flux_change, region)], 
                           by = c("lon", "lat", "dataset_pair", "region"), allow.cartesian = TRUE)

#Plots
to_plot <- copy(avail_flux_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, water_avail_flux)
to_plot <- merge(to_plot, ipcc_hexagon, by = 'region', allow.cartesian = TRUE)
to_plot <- to_plot[complete.cases(to_plot)]

to_plot_pie <- merge(data.table(table(to_plot[, .(region, Conditions)])), unique(to_plot[, .(region, x, y)]), by = 'region')
to_plot_pie <- dcast(to_plot_pie, region + x + y ~ Conditions, value.var = 'N')


world <- map_data('world')
base_map <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()

base_map + 
  geom_scatterpie(data = to_plot_pie, aes(x, y, group = region), 
                  cols = c('Wetter - Accelerated', 'Wetter - Deccelerated',
                           'Drier - Accelerated', 'Drier - Deccelerated')) +
  scale_fill_manual(values = PALETTES$water_cycle_change) +
  theme_void() 
