# Estimation of ensemble statistics
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Packages
library("gtools")

## Data
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

## Variables
period_months_dates <- seq(period_start, by = "month", length.out = period_months)

## Functions
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Analysis
cores <- N_CORES - 3
cl <- makeCluster(cores, type = "FORK")
prec_mean <- parLapply(cl, prec_2000_2019, calc, fun = mean, na.rm = TRUE)
stopCluster(cl)

#prec_mean <- lapply(prec_2000_2019, calc, fun = mean, na.rm = TRUE)
#prec_sd <- lapply(prec_2000_2019, calc, fun = sd, na.rm = TRUE)
#prec_mean <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_mean.rds"))

### Multi-source
prec_mean_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- data.table(as.data.frame(rasterToPoints(prec_mean[[dataset_count]], spatial = TRUE)))
  colnames(dummy) <- c('prec_mean', 'lon', 'lat')
  dummy$dataset <- names(prec_mean[dataset_count])
  dummy[, lon := round(lon, 3)]
  dummy[, lat := round(lat, 3)]
  dummy
}
prec_mean_datasets[, n_datasets := .N, .(lon, lat)]
prec_mean_datasets <- prec_mean_datasets[n_datasets >= MIN_N_DATASETS]

prec_mean_datasets[dataset %in% PREC_DATASETS_OBS, dataset_type := 'ground stations']
prec_mean_datasets[dataset %in% PREC_DATASETS_REANAL, dataset_type := 'reanalysis']
prec_mean_datasets[dataset %in% PREC_DATASETS_REMOTE, dataset_type := 'remote sensing']

### Ensemble statistics
prec_ens_stats <- prec_mean_datasets[, .(ens_mean_mean = round(mean(prec_mean, na.rm = TRUE), 0)), .(lat, lon)]
prec_mean_datasets[, n_datasets := NULL]

#dummy <- prec_mean_datasets[, .(ens_mean_sd = round(sd(prec_mean, na.rm = TRUE), 0)), .(lat, lon)]
#prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- prec_mean_datasets[, .(ens_mean_median = round(median(prec_mean, na.rm = TRUE), 0)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))
dummy <- prec_mean_datasets[, .(ens_mean_sd = round(sd(prec_mean, na.rm = TRUE), 0)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))
dummy <- prec_mean_datasets[, .(ens_mean_q25 = round(estimate_q25(prec_mean), 0)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))
dummy <- prec_mean_datasets[, .(ens_mean_q75 = round(estimate_q75(prec_mean), 0)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

#### Bias measures
prec_ens_stats[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
prec_ens_stats[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

### Grid cell area
prec_area <- prec_ens_stats[, .(lon, lat)] %>% grid_area() # m2
prec_grid <- prec_area[prec_ens_stats[, .(lon, lat, prec_mean = ens_mean_mean)], on = .(lon, lat)]
prec_grid[, prec_volume_year := 12 * area * 10 ^ (-9) * prec_mean * 0.001][, prec_mean := NULL] # km3

## Validation
prec_mean_datasets[, table(n_datasets)]

plot(mean(prec_2000_2019$cpc))
plot(mean(prec_2000_2019$persiann))
plot(mean(prec_2000_2019$`em-earth`))
plot(mean(prec_2000_2019$gpcc))
plot(mean(prec_2000_2019$`gpm-imerg`))

## Save data 
saveRDS(prec_ens_stats, paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
saveRDS(prec_mean_datasets, paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
saveRDS(prec_grid , paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.rds"))

## Figures
to_plot <- prec_ens_stats[, .(value = mean(ens_mean_mean)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = PREC_NAME_SHORT) +
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

to_plot <- prec_ens_stats[, .(value = mean(ens_sd_mean)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = PREC_NAME_SHORT) +
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

to_plot <- prec_ens_stats[, .(value = mean(ens_mean_cv)), .(lon, lat)]
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

to_plot <- prec_ens_stats[, .(value = mean(std_quant_range)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = "Std. 0.25-0.75 quantile range") +
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

