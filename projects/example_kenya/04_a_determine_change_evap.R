source('source/main.R')
source('source/graphics.R')
source('source/example_kenya.R')

### Assessing % change in ensemble mean between two 30 years time periods (t1=1960 to 1989 & t2=1990 to 2019)

## Read data 
evap_stats <- readRDS(paste0(path_save_kenya, "ensemble_evap_stats.rds"))
all_stats_low_bias <- readRDS(paste0(path_save_kenya, "ensemble_stats_low_bias.rds"))

## Set variables
evap_mean_period_1 <- evap_stats[time >= PERIOD_1_START & time <= PERIOD_1_END, mean(mean), .(lon, lat)]
evap_mean_period_2 <- evap_stats[time >= PERIOD_2_START & time <= PERIOD_2_END, mean(mean), .(lon, lat)]
evap_low_bias <- all_stats_low_bias[variable == 'evap', .(lon, lat)]

## Merge data
evap_mean_change <- merge(evap_mean_period_1, evap_mean_period_2, by = c("lon", "lat"))
setnames(evap_mean_change, c('V1.x', 'V1.y'), c('mean_period_1', 'mean_period_2'))

## Main estimations
evap_mean_change[, mean_diff := round(mean_period_2 - mean_period_1, 1)]
evap_mean_change[, mean_perc_change := round((mean_period_2 - mean_period_1)/mean_period_1 * 100, 2)]

## Save for further use
saveRDS(evap_mean_change, paste0(path_save_kenya, "evap_mean_change.rds"))

## Plot results
to_plot <- evap_mean_change[, .(lat, lon, value = mean_perc_change)]
p00 <- ggplot() +
  geom_raster(data = to_plot, aes(x = lon, y = lat, fill = value)) +
  geom_point(data = evap_low_bias, aes(x = lon, y = lat)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = "Change [%]") +
  scale_fill_gradient2(low = main_cols[1], 
                       mid = "white", 
                       high = main_cols[3], 
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


