### Estimation of monthly evapotranspiration mean, standard deviation (sd), and coefficient of variance
### for the dataset ensemble

source('source/example_kenya.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
evap_era5_kenya <- raster(paste0(path_save_kenya, "evap_era5.nc"))
evap_terra_kenya <- raster(paste0(path_save_kenya, "evap_terra.nc"))

## Variables
period_months_dates <- seq(PERIOD_START, by = "month", length.out = period_months)

## Main estimations
evap_mean_month <- foreach(month_count = 1:period_months) %dopar% {
   calc(stack(evap_era5_kenya[[month_count]], 
             evap_terra_kenya[[month_count]]), 
       fun = mean, 
       na.rm = F)
}

evap_mean_month <- brick(evap_mean_month)
evap_mean_month <- setZ(evap_mean_month, period_months_dates)
names(evap_mean_month) <- as.Date(period_months_dates)

evap_sd_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(evap_era5_kenya[[month_count]], 
             evap_terra_kenya[[month_count]]), 
       fun = sd, 
       na.rm = F)
}

evap_sd_month <- brick(evap_sd_month)
evap_sd_month <- setZ(evap_sd_month, period_months_dates)
names(evap_sd_month) <- as.Date(period_months_dates)

evap_cv_month <- foreach(month_count = 1:period_months) %dopar% {
  evap_sd_month[[month_count]]/evap_mean_month[[month_count]]
}

evap_cv_month <- brick(evap_cv_month)
evap_cv_month <- setZ(evap_cv_month, period_months_dates)
names(evap_cv_month) <- as.Date(period_months_dates)

## Quick validation
plot(mean(evap_era5_kenya))
plot(mean(evap_terra_kenya))
plot(mean(evap_mean_month))
plot(mean(evap_sd_month))
plot(mean(evap_cv_month))

## Transform to data.table 
evap_mean_month_dt <- brick_to_dt(evap_mean_month) 
evap_sd_month_dt <- brick_to_dt(evap_sd_month)
evap_cv_month_dt <- brick_to_dt(evap_cv_month)

evap_stats <- merge(evap_mean_month_dt, evap_sd_month_dt, by = c('x', 'y', 'time'))
evap_stats <- merge(evap_stats, evap_cv_month_dt, by = c('x', 'y', 'time'))
colnames(evap_stats) <- c('lon', 'lat', 'time', 'mean', 'sd', 'cv')

## Save data for further use
saveRDS(evap_stats, paste0(path_save_kenya, "ensemble_evap_stats.rds"))

## Plot results
to_plot <- evap_stats[, .(value = mean(mean)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = evap_name_short) +
  scale_fill_gradient2(low = main_cols[3], 
                       mid = "white", 
                       high = "tomato", 
                       midpoint = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- evap_stats[, .(value = mean(sd)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = evap_name_short) +
  scale_fill_gradient2(low = main_cols[3], 
                       mid = "white", 
                       high = "tomato", 
                       midpoint = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- evap_stats[, .(value = mean(cv)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = "CV") +
  scale_fill_gradient2(low = main_cols[1], 
                       mid = "white", 
                       high = main_cols[3], 
                       midpoint = 0.6) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

