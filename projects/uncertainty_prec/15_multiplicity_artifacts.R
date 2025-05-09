source("source/uncertainty_prec.R")

load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "multiplicity_ensemble.rda"))

cmap_range <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "multiplicity_cmap.rds"))
reas_range <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "multiplicity_reas.rds"))

bootstrap_all <- bootstrap_all[, .(date = trunc(date), prec, idx)]
multiplicity_range <- bootstrap_all[, .(upper = max(prec), lower = min(prec)), .(date)]
multiplicity_range <- unique(multiplicity_range[, .(upper, lower, date)])

bootstrap_rep[, date := year(date)]
bootstrap_rep <- bootstrap_rep[, .(prec = sum(prec)), .(date, idx)]
rep_ensemble_range <- bootstrap_rep[, .(upper = quantile(prec, 0.95),
                                        lower = quantile(prec, 0.05),
                                        prec = median(prec)), .(date)]

cmap_range[, date := year(date)]
cmap_range <- cmap_range[, .(prec = sum(prec)), .(date, idx)]
cmap_range <- cmap_range[, .(upper = quantile(prec, 0.95),
                             lower = quantile(prec, 0.05),
                             prec = median(prec)), .(date)]

reas_range[, date := year(date)]
reas_range <- reas_range[, .(prec = sum(prec)), .(date, idx)]
reas_range <- reas_range[, .(upper = quantile(prec, 0.95),
                             lower = quantile(prec, 0.05),
                             prec = median(prec)), .(date)]

#reas_range[, prec := mean(c(upper, lower)), date]
#cmap_range[, prec := mean(c(upper, lower)), date]
#rep_ensemble_range[, prec := mean(c(upper, lower)), date]
mean(cmap_range$prec)
mean(reas_range$prec)
mean(rep_ensemble_range$prec)

p01 <- ggplot() +
  geom_segment(aes(x = 2000, xend = 2019, y = 809.3442, yend = 809.3442),
               color = "#e6ab02", linetype = "dashed", alpha = 0.5) +
  geom_ribbon(data = cmap_range,
              aes(ymax = upper, ymin = lower, x = date), fill = "#e6ab02",
              alpha = 0.5) +
  geom_line(data = cmap_range, aes(x = date, y = prec), color = "#e6ab02",
            linewidth = 1) +
  geom_segment(aes(x = 2000, xend = 2019, y = 903.4825, yend = 903.4825),
               color = "#8B008B", linetype = "dashed", alpha = 0.5) +
  geom_ribbon(data = reas_range,
              aes(ymax = upper, ymin = lower, x = date), fill = "#8B008B",
              alpha = 0.5) +
  geom_line(data = reas_range, aes(x = date, y = prec), color = "#8B008B",
            linewidth = 1) +
  geom_segment(aes(x = 2019.5, xend = 2019.5, y = 809.3442, yend = 903.4825),
               arrow = arrow(length = unit(0.25, "cm"),
                             ends = "both", type = "open"),
               linetype = "dashed", linewidth = 1, color = "#e31a1c") +
  labs(x = NULL, y = "Precipitation [mm/year]") +
  scale_y_continuous(limits = c(720, 980), expand = c(0, 0),
                     breaks = seq(750, 1000, 50)) +
  scale_x_continuous(limits = c(2000, 2020), expand = c(0, 0),
                     breaks = seq(2000, 2019, 5)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.25, "cm"))

p02 <- ggplot() +
  geom_segment(aes(x = 2000, xend = 2019, y = 879.702, yend = 879.702),
               color = "#377eb8", linetype = "dashed", alpha = 0.5) +
  geom_ribbon(data = rep_ensemble_range,
              aes(ymax = upper, ymin = lower, x = date), fill = "#377eb8",
              alpha = 0.5) +
  geom_line(data = rep_ensemble_range, aes(x = date, y = prec),
            color = "#377eb8", linewidth = 1) +
  labs(x = NULL, y = "Precipitation [mm/year]") +
  scale_y_continuous(limits = c(720, 980), expand = c(0, 0),
                     breaks = seq(750, 1000, 50)) +
  scale_x_continuous(limits = c(2000, 2020), expand = c(0, 0),
                     breaks = seq(2000, 2019, 5)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.25, "cm"))

p00 <- ggarrange(p01, p02, ncol = 1, nrow = 2, labels = c("a)", "b)"), align = "hv")

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "ensemble_artifacts.png"),
       width = 5*GOLDEN_RATIO, height = 5*2)
