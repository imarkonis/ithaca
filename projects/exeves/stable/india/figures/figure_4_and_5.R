source("~/ithaca_sp/projects/exeves/stable/india/00_initialize.R")
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
                  "Post-monsoon")))]

to_plot <- exeves_drivers[
  , .(
    evap = mean(evap, na.rm = TRUE),
    swrad = mean(swrad, na.rm = TRUE),
    lwrad = mean(lwrad, na.rm = TRUE),
    prec = mean(prec, na.rm = TRUE),
    sensible = mean(sensible, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE)
  ), by = .(grid_id, season, conditions, KG_class_2)]

# Order seasons
to_plot[, season := factor(
  season,
  levels = c("Winter", "Summer", "Monsoon", "Post-monsoon"))]


to_plot <- melt(
  to_plot,
  id.vars = c("grid_id", "season", "conditions", "KG_class_2", "evap"),
  variable.name = "variable",
  value.name = "value")

base_theme <- theme_linedraw() +
  theme(
    axis.title = element_text(size = 12),
    strip.background = element_rect(fill = 'grey30'),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)))

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
