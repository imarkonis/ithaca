source('source/exeves_ind.R')

region <- 'india'

exeves_drivers <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))

to_plot <- exeves_drivers[month(date) %in% c(3, 6, 9, 12), .(evap = mean(evap),
                                                             swrad = mean(swrad), 
                                                             lwrad = mean(lwrad), 
                                                             prec = mean(prec),
                                                             sensible = mean(sensible), 
                                                             temp = mean(temp)), 
                          .(grid_id, month(date), conditions, KG_class_2)] 

to_plot[, month := month(month, label = TRUE)]
# to_plot <- melt(to_plot, id.vars = c("grid_id", "month", "conditions", "evap"), variable.name = "variable")
to_plot <- melt(to_plot, id.vars = c("grid_id", "month", "conditions", "KG_class_2", "evap"), variable.name = "variable", value.name = "value")

gg_swrad <- ggplot(to_plot[variable == 'swrad']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) + 
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("SW radiation "(~W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col = guide_legend(title = "Conditions")) +
  coord_cartesian(xlim = c(0, 8)) +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
    # panel.grid.major = element_line(colour = "grey90"),
    # panel.grid.minor = element_line(colour = "grey95"),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)))


gg_lwrad <- ggplot(to_plot[variable == 'lwrad']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) + 
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("LW radiation "(W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col= guide_legend(title = "Conditions")) +
  coord_cartesian(xlim = c(0, 8)) +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
    # panel.grid.major = element_line(colour = "grey90"),
    # panel.grid.minor = element_line(colour = "grey95"),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)))

gg_sensible <- ggplot(to_plot[variable == 'sensible']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) + 
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("Sensible heat "(W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col= guide_legend(title = "Conditions")) +
  coord_cartesian(xlim = c(0, 8)) +
  scale_x_continuous(breaks = seq(0, 8, 1))+
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
    # panel.grid.major = element_line(colour = "grey90"),
    # panel.grid.minor = element_line(colour = "grey95"),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)))

gg_prec <- ggplot(to_plot[variable == 'prec']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) + 
  facet_wrap(~month, scales = 'free', ncol = 4) +
  ylab("Precip. (mm/day)") +
  xlab("") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col= guide_legend(title = "Conditions")) +
  coord_cartesian(xlim = c(0, 8)) +
  scale_x_continuous(breaks = seq(0, 8, 1))+
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
    # panel.grid.major = element_line(colour = "grey90"),
    # panel.grid.minor = element_line(colour = "grey95"),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)))

gg_temp <- ggplot(to_plot[variable == 'temp']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) + 
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("Evaporation (mm/day)") +
  ylab(expression(atop("Temperature (°C)"))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col= guide_legend(title = "Conditions")) +
  coord_cartesian(xlim = c(0, 8)) +
  scale_x_continuous(breaks = seq(0, 8, 1))+
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
    # panel.grid.major = element_line(colour = "grey90"),
    # panel.grid.minor = element_line(colour = "grey95"),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)))

# gg_lwrad  <- gg_lwrad + coord_cartesian(xlim = c(0, 8)) + scale_x_continuous(breaks = seq(0, 8, 1))
# gg_prec  <- gg_prec + coord_cartesian(xlim = c(0, 8)) + scale_x_continuous(breaks = seq(0, 8, 1))
# gg_sensible <- gg_sensible + coord_cartesian(xlim = c(0, 8)) + scale_x_continuous(breaks = seq(0, 8, 1))
# gg_temp <- gg_temp + coord_cartesian(xlim = c(0, 8)) + scale_x_continuous(breaks = seq(0, 8, 1))

ggarrange(gg_swrad, gg_lwrad, gg_prec, gg_sensible,  gg_temp, 
          ncol = 1, labels = c("A", "B", "C", "D", "E"),
          legend = 'right', common.legend = TRUE) 

ggsave(paste0(PATH_OUTPUT_FIGURES, "drivers_ind_gleam4.1.pdf"), width = 10, height = 12)


#### IMD India 
source('source/exeves_ind.R')

library(data.table)
library(lubridate)
# library(ggpubr)
# library(ggplot2)
# 
# region <- 'india'
# 
# exeves_drivers <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))


exeves_drivers[, season := fifelse(
  month(date) %in% c(12, 1, 2, 3), "Winter",
  fifelse(month(date) %in% c(4, 5), "Summer",
          fifelse(month(date) %in% c(6, 7, 8, 9), "Monsoon",
                  "Post-monsoon")))
]

to_plot <- exeves_drivers[
  , .(
    evap = mean(evap, na.rm = TRUE),
    swrad = mean(swrad, na.rm = TRUE),
    lwrad = mean(lwrad, na.rm = TRUE),
    prec = mean(prec, na.rm = TRUE),
    sensible = mean(sensible, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE)
  ),
  by = .(grid_id, season, conditions, KG_class_2)
]

# Order seasons
to_plot[, season := factor(
  season,
  levels = c("Winter", "Summer", "Monsoon", "Post-monsoon")
)]


to_plot <- melt(
  to_plot,
  id.vars = c("grid_id", "season", "conditions", "KG_class_2", "evap"),
  variable.name = "variable",
  value.name = "value"
)

base_theme <- theme_linedraw() +
  theme(
    axis.title = element_text(size = 12),
    strip.background = element_rect(fill = 'grey30'),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05))
  )

gg_swrad <- ggplot(to_plot[variable == 'swrad']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) +
  facet_wrap(~season, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("SW radiation "(W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col = guide_legend(title = "Conditions")) +
  base_theme

gg_lwrad <- ggplot(to_plot[variable == 'lwrad']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) +
  facet_wrap(~season, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("LW radiation "(W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col = guide_legend(title = "Conditions")) +
  base_theme

gg_sensible <- ggplot(to_plot[variable == 'sensible']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) +
  facet_wrap(~season, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("Sensible heat "(W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col = guide_legend(title = "Conditions")) +
  base_theme

gg_prec <- ggplot(to_plot[variable == 'prec']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) +
  facet_wrap(~season, scales = 'free', ncol = 4) +
  ylab("Precip. (mm/day)") +
  xlab("") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col = guide_legend(title = "Conditions")) +
  base_theme

gg_temp <- ggplot(to_plot[variable == 'temp']) +
  geom_point(aes(x = evap, y = value, col = conditions), alpha = 0.5) +
  facet_wrap(~season, scales = 'free', ncol = 4) +
  xlab("Evaporation (mm/day)") +
  ylab(expression(atop("Temperature (°C)"))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  guides(col = guide_legend(title = "Conditions")) +
  base_theme

ggarrange(gg_swrad, gg_lwrad, gg_prec, gg_sensible, gg_temp,
  ncol = 1,
  labels = c("A", "B", "C", "D", "E"),
  legend = 'right',
  common.legend = TRUE)

ggsave(paste0(PATH_OUTPUT_FIGURES, "drivers_ind_gleam4.1_seasonal.pdf"), width = 10, height = 12)

#


####
# Extra plots
to_plot <- exeves_drivers[, .(lwrad = mean(lwrad), swrad = mean(swrad)), 
                          .(grid_id, month(date), conditions)]
gg_rad <- ggplot(to_plot) +
  geom_point(aes(x = lwrad, y = swrad, col = conditions), alpha = 0.5) + 
  facet_wrap(~month, scales = 'free') +
  xlab("Longwave radiation (W/m2)") +
  ylab("Shortwave radiation (W/m2)") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(angle = 90, vjust = -0.5, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

to_plot <- exeves_drivers[, .(lwrad = mean(std_lwrad), swrad = mean(std_swrad)), 
                          .(grid_id, month(date), conditions)]
gg_rad_std <- ggplot(to_plot) +
  geom_point(aes(x = lwrad, y = swrad, col = conditions), alpha = 0.5) + 
  geom_hline(yintercept = 0, col = colset_subdued_prof[3]) +
  geom_vline(xintercept = 0, col = colset_subdued_prof[3]) +
  facet_wrap(~month, scales = "free") +
  xlab("Longwave radiation (z-score)") +
  ylab("Shortwave radiation (z-score)") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(angle = 90, vjust = -0.5, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

ggarrange(gg_rad, NULL, gg_rad_std,
          nrow = 3, 
          labels = c("A", "", "B"), heights = c(1, 0.05, 1),
          legend = 'bottom', common.legend = TRUE) 

ggsave(paste0(PATH_OUTPUT_FIGURES, "short_long_rad_ind_gleam4.pdf"), width = 8, height = 9)

# to_plot <- exeves_drivers[, .(evap = mean(std_value), swrad = mean(std_swrad)), 
#                           .(grid_id, month(date), conditions)] 
to_plot <- exeves_drivers[, .(evap = mean(evap), swrad = mean(std_swrad)), 
                          .(grid_id, month(date), conditions)] 

gg_evap_swrad_std <- ggplot(to_plot) +
  geom_point(aes(x = evap, y = swrad, col = conditions), alpha = 0.7) +
  geom_hline(yintercept = 0, col = colset_subdued_prof[3]) +
  geom_vline(xintercept = 0, col = colset_subdued_prof[3]) +
  facet_wrap(~month, scales = 'free') +
  xlab("Evaporation (z-score)") +
  ylab("Shortwave radiation (z-score)") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(angle = 90, vjust = -0.5, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

# to_plot <- exeves_drivers[, .(evap = mean(std_value), lwrad = mean(std_lwrad)), 
#                           .(grid_id, month(date), conditions)] 
to_plot <- exeves_drivers[, .(evap = mean(evap), lwrad = mean(std_lwrad)), 
                          .(grid_id, month(date), conditions)] 

gg_evap_lwrad_std <- ggplot(to_plot) +
  geom_point(aes(x = evap, y = lwrad, col = conditions), alpha = 0.7) +
  geom_hline(yintercept = 0, col = colset_subdued_prof[3]) +
  geom_vline(xintercept = 0, col = colset_subdued_prof[3]) +
  facet_wrap(~month, scales = 'free') +
  xlab("Evaporation (z-score)") +
  ylab("Longwave radiation (z-score)") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(angle = 90, vjust = -0.5, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

ggarrange(gg_evap_swrad_std, NULL, gg_evap_lwrad_std,
          nrow = 3, 
          labels = c("A", "", "B"), heights = c(1, 0.05, 1),
          legend = 'bottom', common.legend = TRUE)

ggsave(paste0(PATH_OUTPUT_FIGURES, "std_rad_ind_gleam4.pdf"), width = 8, height = 9)
