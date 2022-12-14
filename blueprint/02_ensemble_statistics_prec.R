### Estimation of monthly precipitation mean, standard deviation (sd), and coefficient of variance
### for the dataset ensemble

source('source/blueprint.R')
source('source/masks.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_era5_kenya <- readRDS(paste0(path_save_blueprint, "prec_era5.rds"))
prec_gpcc_kenya <- readRDS(paste0(path_save_blueprint, "prec_gpcc.rds"))
prec_em_kenya <- readRDS(paste0(path_save_blueprint, "prec_em.rds"))
prec_gpcp_kenya <- readRDS(paste0(path_save_blueprint, "prec_gpcp.rds"))
prec_mswep_kenya <- readRDS(paste0(path_save_blueprint, "prec_mswep.rds"))
prec_gpm_kenya <- readRDS(paste0(path_save_blueprint, "prec_gpm.rds"))

## Set variables
period_months_dates <- seq(period_start, by = "month", length.out = period_months)

## Crop over land <--- Check if works and prepare a function
fname_shape <- list.files(path = masks_dir_landsea, full.names = TRUE, pattern = "land_ocean")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_crop <- st_crop(shape_mask, study_area)
shape_mask_raster <- rasterize(shape_mask_crop, prec_era5_kenya[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'land_ocean')
prec_stats_mean <- merge(prec_stats_mean, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_mean <- prec_stats_mean[land_ocean == "land"]
prec_stats_mean <- prec_stats_mean[, land_ocean := NULL]

## Main estimations
# Version 1: Parallel computing
prec_mean_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]],
             prec_gpcc_kenya[[month_count]],
             prec_em_kenya[[month_count]],
             prec_gpcp_kenya[[month_count]],
             prec_mswep_kenya[[month_count]],
             prec_gpm_kenya[[month_count]]), 
       fun = mean, 
       na.rm = T)
}

prec_mean_month <- brick(prec_mean_month)
prec_mean_month <- setZ(prec_mean_month, period_months_dates)
names(prec_mean_month) <- as.Date(period_months_dates)

prec_sd_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]],
             prec_gpcc_kenya[[month_count]],
             prec_em_kenya[[month_count]],
             prec_gpcp_kenya[[month_count]],
             prec_mswep_kenya[[month_count]],
             prec_gpm_kenya[[month_count]]), 
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

## Quick validation
plot(mean(prec_era5_kenya))
plot(mean(prec_gpcc_kenya))
plot(mean(prec_em_kenya))
plot(mean(prec_gpcp_kenya))
plot(mean(prec_mswep_kenya))
plot(mean(prec_gpm_kenya))
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
prec_stats[, month := month(time)]
prec_stats[, year := year(time)]
prec_stats <- prec_stats[, .(lon, lat, time, month, year, mean, sd, cv)]

## Save data for further use
saveRDS(prec_stats, paste0(path_save_blueprint, "ensemble_prec_stats.rds"))

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

