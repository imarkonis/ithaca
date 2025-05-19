# Maps
source("source/uncertainty_prec.R")

install.packages(setdiff("Metrics",  rownames(installed.packages())))

library(Metrics)

## Data
prec_ensemble <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                "pRecipe_ensemble.rds"))

e_obs <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "e-obs_monthly.rds"))
e_obs <- e_obs[year(date) >= 2000 & year(date) <= 2019]


stations <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                         "others/stations_info_rr_v30.0e.txt"))

stations[, `:=`(START = year(START), STOP = year(STOP))]
stations <- stations[START <= 2000 & STOP >= 2019]

prec_masks <- pRecipe_masks()
prec_masks <- prec_masks[(country == "Germany" | country == "Netherlands" |
                            country == "Luxembourg" | country == "Czechia" |
                            country == "Austria" | country == "Belgium" |
                            country == "France" | country == "Switzerland") &
                           lon >= -10 & lon <= 40 & lat >= 35 & lat <= 70]

e_obs <- e_obs[prec_masks[, .(lon, lat)], on = .(lon, lat)]
e_obs <- e_obs[complete.cases(e_obs)]

prec_ensemble <- prec_ensemble[prec_masks[, .(lon, lat)], on = .(lon, lat)]

prec_data <- merge(prec_ensemble, e_obs, by = c("lon", "lat", "date"))

dummie_cor <- prec_data[, .(coeff = cor(mean_prec, value,
                                          use = "pairwise.complete.obs")),
                        .(lon, lat)]

dummie_nse <- copy(prec_data)
dummie_nse <- dummie_nse[, dummie_mean := mean(value, na.rm = TRUE),
                         .(lon, lat)
                         ][, `:=`(t1 = (value - mean_prec)^2,
         t2 = (value - dummie_mean)^2)]
dummie_nse <- dummie_nse[, .(coeff = 1 - (sum(t1)/sum(t2))), .(lon, lat)]

dummie_rmse <- prec_data[, .(coeff = rmse(value, mean_prec)), .(lon, lat)]

dummie_kge <- copy(prec_data)
dummie_kge <- dummie_kge[, .(r_coeff = cor(mean_prec, value,
                                           use = "pairwise.complete.obs"),
                             mean_obs = mean(value, na.rm = TRUE),
                             mean_sim = mean(mean_prec, na.rm = TRUE),
                             var_obs = sd(value, na.rm = TRUE),
                             var_sim = sd(mean_prec, na.rm = TRUE)),
                         .(lon, lat)]
dummie_kge <- dummie_kge[, .(coeff = 1 - sqrt(((r_coeff - 1)^2) + (((var_sim/var_obs) - 1)^2) + (((mean_sim/mean_obs) - 1)^2))),
                         .(lon, lat)]

dummie_t <- copy(prec_data)
dummie_t <- dummie_t[, .(mse_prec = mean((mean_prec - value)^2, na.rm = TRUE),
                         r_prec = cor(value, mean_prec, use = "pairwise.complete.obs"),
                         bias_prec = mean(value, na.rm = TRUE) - mean(mean_prec, na.rm = TRUE),
                         var_prec = sd(value, na.rm = TRUE)^2,
                         mean_var = sd(mean_prec, na.rm = TRUE)^2),
                     .(lon, lat)]

dummie_t <- dummie_t[, .(coeff = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + mean_var)))),
                     .(lon, lat)]

dummie_bias <- prec_data[, .(coeff = bias(value, mean_prec)), .(lon, lat)]

e_obs <- e_obs[, .(prec = sum(value, na.rm = TRUE)), .(lon, lat, year(date))]
e_obs <- e_obs[, .(prec = mean(prec, na.rm = TRUE)), .(lon, lat)]

prec_ensemble <- prec_ensemble[, .(prec = sum(mean_prec, na.rm = TRUE)),
                               .(lon, lat, year(date))]
prec_ensemble <- prec_ensemble[, .(prec = mean(prec, na.rm = TRUE)),
                               .(lon, lat)]
prec_ensemble <- prec_ensemble[prec > 0]
###

ggplot(stations) +
  borders(colour = "gray23", fill = "gray69") +
  geom_point(aes(x = LON, y = LAT), size = 1, colour = "#1A237E", shape = 20) +
  borders(colour = "gray23") +
  geom_rect(xmin = -5, xmax = 19.5, ymin = 41, ymax = 55.5, fill = NA, color = "#800020") +
  coord_cartesian(xlim = c(-10, 40), ylim = c(35, 70)) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

ggsave(paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "stations_eruope.png"),
       width = 4.5*GOLDEN_RATIO, height = 4.5)

p01 <- ggplot(prec_ensemble) +
  geom_tile(aes(x = lon, y = lat, fill = prec)) +
  borders(colour = "gray23") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1,
                       limits = c(450, 1950)) +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "[mm/yr]", tag = "a)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p02 <- ggplot(e_obs) +
  geom_tile(aes(x = lon, y = lat, fill = prec)) +
  borders(colour = "gray23") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1,
                       limits = c(450, 1950)) +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "[mm/yr]", tag = "b)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 18),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p03 <- ggplot(dummie_t) +
  geom_tile(aes(x = lon, y = lat, fill = coeff)) +
  borders(colour = "gray23") +
  scale_fill_viridis_c(limits = c(0, 1)) +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "T-metric", tag = "c)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 18),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p04 <- ggplot(dummie_cor) +
  geom_tile(aes(x = lon, y = lat, fill = coeff)) +
  borders(colour = "gray23") +
  scale_fill_viridis_c(limits = c(0, 1)) +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "R", tag = "d)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 18),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p05 <- ggplot(dummie_rmse) +
  geom_tile(aes(x = lon, y = lat, fill = coeff)) +
  borders(colour = "gray23") +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "RMSE", tag = "e)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 18),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p06 <- ggplot(dummie_bias) +
  geom_tile(aes(x = lon, y = lat, fill = coeff)) +
  borders(colour = "gray23") +
  scale_fill_gradient2(high = "#e66101", low = "#542788", mid = "#F7F7F7") +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "Bias", tag = "f)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20),
        plot.tag = element_text(size = 18),
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p07 <- ggplot(dummie_nse) +
  geom_tile(aes(x = lon, y = lat, fill = coeff)) +
  borders(colour = "gray23") +
  scale_fill_distiller(palette = "PuBuGn", direction = 1, limits = c(0, 1)) +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "NSE", tag = "g)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 18),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p08 <- ggplot(dummie_kge) +
  geom_tile(aes(x = lon, y = lat, fill = coeff)) +
  borders(colour = "gray23") +
  coord_cartesian(xlim = c(-5, 19.5), ylim = c(41, 55.5), expand = FALSE) +
  scale_fill_distiller(palette = "PuBuGn", direction = 1, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "KGE", tag = "h)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20),
        plot.tag = element_text(size = 18),
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))

p00 <- ggarrange(p01, p02, p03, p04, p05, p06, p07, p08,
                 ncol = 2, nrow = 4, align = "hv")

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "demo_ensemble.png"),
       width = 4.5*GOLDEN_RATIO*2, height = 4.5*4)

