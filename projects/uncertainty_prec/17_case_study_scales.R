# Maps
source("source/uncertainty_prec.R")

## Data
load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_scales.rda"))

dummie_t_250 <- copy(prec_data_250)
dummie_t_250 <- dummie_t_250[, .(mse_prec = mean((value - gpcc)^2, na.rm = TRUE),
                                 r_prec = cor(gpcc, value, use = "pairwise.complete.obs"),
                                 bias_prec = mean(gpcc, na.rm = TRUE) - mean(value, na.rm = TRUE),
                                 var_prec = sd(gpcc, na.rm = TRUE)^2,
                                 mean_var = sd(value, na.rm = TRUE)^2),
                             .(lon, lat, dataset)]

dummie_t_250 <- dummie_t_250[, .(coeff = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + mean_var)))),
                             .(lon, lat, dataset)]

dummie_t_100 <- copy(prec_data_100)
dummie_t_100 <- dummie_t_100[, .(mse_prec = mean((value - gpcc)^2, na.rm = TRUE),
                                 r_prec = cor(gpcc, value, use = "pairwise.complete.obs"),
                                 bias_prec = mean(gpcc, na.rm = TRUE) - mean(value, na.rm = TRUE),
                                 var_prec = sd(gpcc, na.rm = TRUE)^2,
                                 mean_var = sd(value, na.rm = TRUE)^2),
                             .(lon, lat, dataset)]

dummie_t_100 <- dummie_t_100[, .(coeff = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + mean_var)))),
                             .(lon, lat, dataset)]

dummie_t_050 <- copy(prec_data_050)
dummie_t_050 <- dummie_t_050[, .(mse_prec = mean((value - gpcc)^2, na.rm = TRUE),
                                 r_prec = cor(gpcc, value, use = "pairwise.complete.obs"),
                                 bias_prec = mean(gpcc, na.rm = TRUE) - mean(value, na.rm = TRUE),
                                 var_prec = sd(gpcc, na.rm = TRUE)^2,
                                 mean_var = sd(value, na.rm = TRUE)^2),
                             .(lon, lat, dataset)]

dummie_t_050 <- dummie_t_050[, .(coeff = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + mean_var)))),
                             .(lon, lat, dataset)]

dummie_t_025 <- copy(prec_data_025)
dummie_t_025 <- dummie_t_025[, .(mse_prec = mean((value - gpcc)^2, na.rm = TRUE),
                                 r_prec = cor(gpcc, value, use = "pairwise.complete.obs"),
                                 bias_prec = mean(gpcc, na.rm = TRUE) - mean(value, na.rm = TRUE),
                                 var_prec = sd(gpcc, na.rm = TRUE)^2,
                                 mean_var = sd(value, na.rm = TRUE)^2),
                             .(lon, lat, dataset)]

dummie_t_025 <- dummie_t_025[, .(coeff = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + mean_var)))),
                             .(lon, lat, dataset)]


dummie_t_250$res <- 2.50
dummie_t_100$res <- 1.00
dummie_t_050$res <- 0.50
dummie_t_025$res <- 0.25

prec_data <- rbind(dummie_t_250, dummie_t_100, dummie_t_050, dummie_t_025)

prec_data$res <- format(prec_data$res, nsmall = 2)
prec_data[dataset == "cmap", dataset := "CMAP"
          ][dataset == "persiann", dataset := "PERSIANN CDR"]

###
ggplot(prec_data) +
  geom_raster(aes(x = lon, y = lat, fill = coeff, color = coeff)) +
  facet_grid(res ~ dataset) +
  borders(colour = "gray23") +
  scale_fill_viridis_c(limits = c(0, 1)) +
  coord_cartesian(xlim = c(-126, -67), ylim = c(25, 50), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  scale_y_continuous(breaks = seq(-180, 180, 30)) +
  labs(x = NULL, y = NULL, fill = "T-metric\nwrt\nGPCC FD v2022\n") +
  guides(color = "none") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray23", linewidth = 2),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20),
        plot.tag = element_text(size = 18),
        axis.text = element_blank(),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_rect(fill = NA, color = "gray23",
                                        linewidth = 1))

ggsave(paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "demo_scales.png"),
       width = 4.5*GOLDEN_RATIO*2, height = 4.5*4)

