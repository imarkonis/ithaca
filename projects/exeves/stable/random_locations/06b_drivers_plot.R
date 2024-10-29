source('source/exeves.R')

region <- 'random_locations'
exeves_drivers <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))

to_plot <- exeves_drivers[month(date) %in% c(3, 6, 9, 12), .(evap = mean(evap), 
                                                             swrad = mean(swrad), 
                                                             lwrad = mean(lwrad), 
                                                             prec = mean(prec),
                                                             sensible = mean(sensible), 
                                                             temp = mean(temp)), 
                          .(KG_class_2, month(date), conditions)] 
to_plot[, month := month(month, label = TRUE)]
to_plot <- melt(to_plot, id.vars = c("KG_class_2", "month", "conditions", "evap"), variable.name = "variable")

gg_swrad <- ggplot(to_plot[variable == 'swrad']) +
  geom_point(aes(x = evap, y = value, col = conditions, shape = KG_class_2)) +
  geom_point(data = to_plot[variable == 'swrad' & KG_class_2 == 'Cf'], 
             aes(x = evap, y = value, col = conditions), shape = 6, size = 3, col = 'black') +
  geom_line(aes(x = evap, y = value, group = KG_class_2), col = 'grey') +
  geom_line(data = to_plot[variable == 'swrad' & KG_class_2 == 'Cf'],
            aes(x = evap, y = value, group = KG_class_2), col = 'black') +
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("SW radiation "(~W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  scale_shape_manual(values = 1:nlevels(to_plot$KG_class_2)) +
  guides(shape = guide_legend(title = "KG class", ncol = 2)) +
  guides(col = guide_legend(title = "Conditions")) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

gg_lwrad <- ggplot(to_plot[variable == 'lwrad']) +
  geom_point(aes(x = evap, y = value, col = conditions, shape = KG_class_2)) +
  geom_point(data = to_plot[variable == 'lwrad' & KG_class_2 == 'Cf'], 
             aes(x = evap, y = value, col = conditions), shape = 6, size = 3, col = 'black') +
  geom_line(aes(x = evap, y = value, group = KG_class_2), col = 'grey') +
  geom_line(data = to_plot[variable == 'lwrad' & KG_class_2 == 'Cf'],
            aes(x = evap, y = value, group = KG_class_2), col = 'black') +
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("LW radiation "(~W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  scale_shape_manual(values = 1:nlevels(to_plot$KG_class_2)) +
  guides(shape = guide_legend(title = "KG class", ncol = 2)) +
  guides(col = guide_legend(title = "Conditions")) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

gg_temp <- ggplot(to_plot[variable == 'temp']) +
  geom_point(aes(x = evap, y = value, col = conditions, shape = KG_class_2)) +
  geom_point(data = to_plot[variable == 'temp' & KG_class_2 == 'Cf'], 
             aes(x = evap, y = value, col = conditions), shape = 6, size = 3, col = 'black') +
  geom_line(aes(x = evap, y = value, group = KG_class_2), col = 'grey') +
  geom_line(data = to_plot[variable == 'temp' & KG_class_2 == 'Cf'],
            aes(x = evap, y = value, group = KG_class_2), col = 'black') +
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("Temperature (°C)"))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  scale_shape_manual(values = 1:nlevels(to_plot$KG_class_2)) +
  guides(shape = guide_legend(title = "KG class", ncol = 2)) +
  guides(col = guide_legend(title = "Conditions")) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

gg_sensible <- ggplot(to_plot[variable == 'sensible']) +
  geom_point(aes(x = evap, y = value, col = conditions, shape = KG_class_2)) +
  geom_point(data = to_plot[variable == 'sensible' & KG_class_2 == 'Cf'], 
             aes(x = evap, y = value, col = conditions), shape = 6, size = 3, col = 'black') +
  geom_line(aes(x = evap, y = value, group = KG_class_2), col = 'grey') +
  geom_line(data = to_plot[variable == 'sensible' & KG_class_2 == 'Cf'],
            aes(x = evap, y = value, group = KG_class_2), col = 'black') +
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("") +
  ylab(expression(atop("Sensible Heat "(W/m^2)))) +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  scale_shape_manual(values = 1:nlevels(to_plot$KG_class_2)) +
  guides(shape = guide_legend(title = "KG class", ncol = 2)) +
  guides(col = guide_legend(title = "Conditions")) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

gg_prec <- ggplot(to_plot[variable == 'prec']) +
  geom_point(aes(x = evap, y = value, col = conditions, shape = KG_class_2)) +
  geom_point(data = to_plot[variable == 'prec' & KG_class_2 == 'Cf'], 
             aes(x = evap, y = value, col = conditions), shape = 6, size = 3, col = 'black') +
  geom_line(aes(x = evap, y = value, group = KG_class_2), col = 'grey') +
  geom_line(data = to_plot[variable == 'prec' & KG_class_2 == 'Cf'],
            aes(x = evap, y = value, group = KG_class_2), col = 'black') +
  facet_wrap(~month, scales = 'free', ncol = 4) +
  xlab("Evaporation (mm/day)") +
  ylab("Precip. (mm/day)") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  scale_shape_manual(values = 1:nlevels(to_plot$KG_class_2)) +
  guides(shape = guide_legend(title = "KG class", ncol = 2)) +
  guides(col = guide_legend(title = "Conditions")) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        strip.background = element_rect(fill = 'grey30'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

ggarrange(gg_swrad, gg_lwrad, gg_temp, gg_sensible, gg_prec, 
          ncol = 1, labels = c("A", "B", "C", "D", "E"),
          legend = 'right', common.legend = TRUE)
ggsave(paste0(PATH_OUTPUT_FIGURES, "drivers_rnd_loc.png"), width = 10, height = 12)


# Extra plots
to_plot <- exeves_drivers[, .(lwrad = mean(lwrad), swrad = mean(swrad)), 
                          .(KG_class_2, month(date), conditions)]
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
                          .(KG_class_2, month(date), conditions)]
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
ggsave(paste0(PATH_OUTPUT_FIGURES, "short_long_rad.png"), width = 8, height = 9)

to_plot <- exeves_drivers[, .(evap = mean(std_value), swrad = mean(std_swrad)), 
                          .(KG_class_2, month(date), conditions)] 
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

to_plot <- exeves_drivers[, .(evap = mean(std_value), lwrad = mean(std_lwrad)), 
                          .(KG_class_2, month(date), conditions)] 

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
ggsave(paste0(PATH_OUTPUT_FIGURES, "std_rad.png"), width = 8, height = 9)
