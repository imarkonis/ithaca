### Determine the regions that have lower observation bias according to the 
### coefficient of variability of the datasets

source('source/example_kenya.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_stats <- readRDS(paste0(path_save_kenya, "ensemble_prec_stats.rds"))
evap_stats <- readRDS(paste0(path_save_kenya, "ensemble_evap_stats.rds"))

## Set variables
quantiles <- seq(0.1, 1, 0.1)
bias_thres_cv <- 0.5

## Merge data 
prec_stats[, variable := 'prec']
evap_stats[, variable := 'evap']
all_stats <- rbind(prec_stats, evap_stats)

## Main estimations
all_stats_mean <- all_stats[, lapply(.SD, mean), .SDcols = c('mean', 'sd', 'cv'), by = c('lon', 'lat', 'variable')]
all_stats_low_bias <- all_stats_mean[cv < bias_thres_cv,  .(lon, lat, variable)]
all_stats_low_bias[, all_low_bias := .N == n_variables, by =  c('lon', 'lat')]
all_stats_low_bias <- merge(all_stats_mean, all_stats_low_bias, by = c('lon', 'lat', 'variable'))

grid_cell_n <- nrow(all_stats_mean[variable == 'prec'])
     
cv_quantiles <- all_stats_mean[, .(cv = quantile(cv, quantiles)), variable]
cv_values_n <- data.table(quantile = quantiles, 
                          variable = cv_quantiles$variable,
                          cv = cv_quantiles$cv, 
                          size = floor(quantile(1:grid_cell_n, quantiles)))

## Save data for further use
saveRDS(all_stats, paste0(path_save_kenya, "ensemble_stats.rds"))
saveRDS(all_stats_mean, paste0(path_save_kenya, "ensemble_stats_mean.rds"))
saveRDS(all_stats_low_bias, paste0(path_save_kenya, "ensemble_stats_low_bias.rds"))

## Plot results
to_plot <- cv_values_n
ggplot(to_plot, aes(x = size, y = cv, col = variable)) +
  geom_point() +
  geom_line() +
  labs(x = "Size", y = "CV") +
  scale_color_manual(values = c(main_cols[3], main_cols[1])) +
  theme_light() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 

to_plot <- all_stats_low_bias[variable == 'prec']
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = "")) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = 'Low bias') +
  scale_x_continuous(expand = c(0.015, 0.015)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 

y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0.015, 0.015), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- all_stats_low_bias[variable == 'evap']
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = "")) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = 'Low bias') +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- all_stats_low_bias[all_low_bias == TRUE]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = "")) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = 'Low bias') +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01




