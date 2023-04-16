### Estimation of monthly precipitation mean, standard deviation (sd), and coefficient of variance
### for the dataset ensemble

source('source/example_kenya.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_era5_kenya <- raster(paste0(path_save_kenya, "prec_era5.nc"))
prec_terra_kenya <- raster(paste0(path_save_kenya, "prec_terra.nc"))

## Set variables
period_months_dates <- seq(PERIOD_START, by = "month", length.out = period_months)

## Main estimations
# Version 1: Parallel computing
prec_mean_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]], 
             prec_terra_kenya[[month_count]]),
       fun = mean, 
       na.rm = T)
}

prec_mean_month <- brick(prec_mean_month)
prec_mean_month <- setZ(prec_mean_month, period_months_dates)
names(prec_mean_month) <- as.Date(period_months_dates)

prec_sd_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]], 
             prec_terra_kenya[[month_count]]), 
       fun = sd, 
       na.rm = T)
}

prec_sd_month <- brick(prec_sd_month)
prec_sd_month <- setZ(prec_sd_month, period_months_dates)
names(prec_sd_month) <- as.Date(period_months_dates)

prec_cv_month <- foreach(month_count = 1:period_months) %dopar% {
  prec_sd_month[[month_count]]/prec_mean_month[[month_count]]
}

prec_cv_month <- brick(prec_cv_month)
prec_cv_month <- setZ(prec_cv_month, period_months_dates)
names(prec_cv_month) <- as.Date(period_months_dates)

# Version 2: Alternative example with stackApply 
# [Not sure which of two versions is faster with big rasters]

prec_mean_month_alt <- stackApply(x = stack(prec_era5_kenya, prec_terra_kenya), 
                                 indices = rep(1:period_months, n_datasets), 
                                 fun = mean, 
                                 na.rm = F)
prec_mean_month_alt <- setZ(prec_mean_month_alt, period_months_dates)
names(prec_mean_month_alt) <- as.Date(period_months_dates)
prec_mean_month_alt_dt <- brick_to_dt(prec_mean_month_alt)

## Quick validation
plot(mean(prec_era5_kenya))
plot(mean(prec_terra_kenya))
plot(mean(prec_mean_month))
plot(mean(prec_sd_month))
plot(mean(prec_cv_month))

## Transform to data.table 
prec_mean_month_dt <- brick_to_dt(prec_mean_month) 
prec_sd_month_dt <- brick_to_dt(prec_sd_month)
prec_cv_month_dt <- brick_to_dt(prec_cv_month)

prec_stats <- merge(prec_mean_month_dt, prec_sd_month_dt, by = c('x', 'y', 'time'))
prec_stats <- merge(prec_stats, prec_cv_month_dt, by = c('x', 'y', 'time'))
colnames(prec_stats) <- c('lon', 'lat', 'time', 'mean', 'sd', 'cv')

## Save data for further use
saveRDS(prec_stats, paste0(path_save_kenya, "ensemble_prec_stats.rds"))

## Plot results
to_plot <- prec_stats[, .(value = mean(mean)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = prec_name_short) +
  scale_fill_gradient2(low = main_cols[3], 
                       mid = "white", 
                       high = "dark blue", 
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

to_plot <- prec_stats[, .(value = mean(sd)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = prec_name_short) +
  scale_fill_gradient2(low = main_cols[3], 
                       mid = "white", 
                       high = "dark blue", 
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

to_plot <- prec_stats[, .(value = mean(cv)), .(lon, lat)]
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

