# Determine the regions that have lower observation bias according to the 
# coefficient of variation of the datasets

source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data 
prec_stats <- readRDS(paste0(PATH_SAVE_BLUEPRINT, "prec_ensemble_stats.rds"))
prec_stats_month <- readRDS(paste0(PATH_SAVE_BLUEPRINT, "prec_ensemble_stats_month.rds"))
ghcn_stations <- fread('~/shared/data_review/ghcn_stations_now.csv')[, .(lat, lon)]

## Variables
quantiles <- seq(0.1, 1, 0.1)
bias_thres_cv <- 0.3

## Analysis
prec_stats_low_bias <- prec_stats[ens_mean_cv <= bias_thres_cv,  .(lon, lat)]

cv_quantiles <- prec_stats[, quantile(ens_mean_cv, quantiles)]
cv_values_n <- data.table(quantile = quantiles, 
                          cv = cv_quantiles, 
                          size = floor(quantile(1:nrow(prec_stats), quantiles)))

ghcn_stations_kenya <- ghcn_stations[lat <= PILOT_LAT_MAX & lat >= PILOT_LAT_MIN & 
                                       lon <= PILOT_LON_MAX & lon >= PILOT_LON_MIN]

## Figures
to_plot <- cv_values_n
ggplot(to_plot, aes(y = size, x = cv)) +
  geom_point() +
  geom_line() +
  labs(y = "Number of grid cells", x = "CV") +
  theme_light() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 

to_plot <- prec_stats_low_bias
p00 <- ggplot() +
  geom_raster(data = to_plot, aes(x = lon, y = lat, fill = "")) +
  geom_point(data = ghcn_stations_kenya, aes(x = lon, y = lat), col = 'dark red', size = 3) +
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



