source("source/uncertainty_prec.R")

load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "multiplicity_ensemble.rda"))

cmap_range <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "multiplicity_cmap.rds"))
reas_range <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "multiplicity_reas.rds"))

bootstrap_all <- bootstrap_all[, .(date = trunc(date), prec, idx)]
multiplicity_range <- bootstrap_all[, .(upper = max(prec), lower = min(prec)), .(date)]
multiplicity_range <- unique(multiplicity_range[, .(upper, lower, date)])

rep_ensemble_range <- bootstrap_rep[, .(upper = max(prec), lower = min(prec)), .(date)]

cmap_range <- cmap_range[, .(upper = max(prec), lower = min(prec)), .(date)]

reas_range <- reas_range[, .(upper = max(prec), lower = min(prec)), .(date)]

p01 <- ggplot(multiplicity_range) +
  #geom_ribbon(aes(ymax = upper, ymin = lower, x = date), fill = "gray69", color = "gray23") +
  geom_ribbon(data = cmap_range,
              aes(ymax = upper, ymin = lower, x = date), fill = "#e6ab02",
              color = "#e6ab02", alpha = 0.5) +
  geom_ribbon(data = reas_range,
              aes(ymax = upper, ymin = lower, x = date), fill = "#8B008B",
              color = "#8B008B", alpha = 0.5) +
  labs(x = NULL, y = "Precipitation [mm/month]") +
  scale_y_continuous(limits = c(50,100)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_line(colour = "gray23", linetype = "dashed"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.25, "cm"))

p02 <- ggplot(rep_ensemble_range) +
  geom_ribbon(aes(ymax = upper, ymin = lower, x = date), fill = "#377eb8",
              color = "#377eb8", alpha = 0.5) +
  labs(x = NULL, y = "Precipitation [mm/month]") +
  scale_y_continuous(limits = c(50,100)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_line(colour = "gray23", linetype = "dashed"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.25, "cm"))

p00 <- ggarrange(p01, p02, ncol = 1, nrow = 2, labels = c("a)", "b)"), align = "hv")

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "ensemble_artifacts.png"),
       width = 5*GOLDEN_RATIO, height = 5*2)
