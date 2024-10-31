install.packages("waffle")

source('source/exeves.R')
library(waffle)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))
evap_grid <- readRDS(paste0(PATH_OUTPUT_DATA, 'grid_', region, '.rds'))
borders <- read_sf('../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp')

axis_decimal <- function(x) sprintf("%.1f", x)

names(prec)[3] <- "prec"
exeves_prec <- merge(exeves, prec, by = c('grid_id', 'date'), all.x = TRUE)
names(exeves_prec)[5] <- 'evap'

# All days
exeves_prec_sums <- unique(exeves_prec[month(date) == 1, .(evap = sum(evap) / (SUB_PERIOD_YEARS * 31), 
                                           prec = sum(prec) / (SUB_PERIOD_YEARS * 31), 
                                           diff_pe = (sum(prec) - sum(evap)) / (SUB_PERIOD_YEARS * 31)), 
                                       by = .(grid_id, period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]
exeves_prec_sums <- merge(evap_grid, exeves_prec_sums, by = "grid_id")

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]

levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[9] <- "Period"
to_plot[grid_id == 2 & Period == "Up to 2001", Conditions := factor('Wetter - Deccelerated')] # needed for plotting purposes

gg_all <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/day]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/day]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

to_plot_nested <- to_plot[, .N, Conditions]
to_plot_nested$N <- round(100 * to_plot_nested$N/sum(to_plot_nested$N), 0)
gg_waffle <- 
  ggplot(to_plot_nested, aes(fill = Conditions, values = N)) +
  geom_waffle(alpha = 0.5, size = 1, colour = "white", n_rows = 10) +
  scale_fill_manual(values = WATER_CYCLE_CHANGE_PALETTE[c(1, 3, 2, 4)]) + 
  xlab("") +
  ylab("") +
  theme_linedraw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") 

sub_grob <- ggplotGrob(gg_waffle)
gg_all <- gg_all + annotation_custom(sub_grob, xmin = 1.25, xmax = 1.75, ymin = 1.15, ymax = 1.75)

# Exeve days
exeves_prec_sums <- unique(exeves_prec[!is.na(event_80_95_id) & month(date) == 1, .(evap = sum(evap) / (SUB_PERIOD_YEARS * 31), 
                                                                 prec = sum(prec) / (SUB_PERIOD_YEARS * 31), 
                                                                 diff_pe = (sum(prec) - sum(evap)) / (SUB_PERIOD_YEARS * 31)), 
                                       by = .(grid_id, period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]
exeves_prec_sums <- merge(evap_grid, exeves_prec_sums, by = "grid_id")

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[9] <- "Period"

gg_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE[c(1, 3, 4, 2)]) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/day]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/day]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

to_plot_nested <- to_plot[, .N, Conditions]
to_plot_nested$N <- round(100 * to_plot_nested$N/sum(to_plot_nested$N), 0)
gg_waffle <- 
  ggplot(to_plot_nested, aes(fill = Conditions, values = N)) +
  geom_waffle(alpha = 0.5, size = 1, colour = "white", n_rows = 10) +
  scale_fill_manual(values =  WATER_CYCLE_CHANGE_PALETTE[c(1, 3, 2, 4)]) + 
  xlab("") +
  ylab("") +
  theme_linedraw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") 

sub_grob <- ggplotGrob(gg_waffle)
gg_event <- gg_event + annotation_custom(sub_grob, xmin  = 0.425, xmax = 0.655, ymin = -0.01, ymax = 0.3)

# Non-exeve days
exeves_prec_sums <- unique(exeves_prec[is.na(event_80_95_id) & month(date) == 1, .(evap = sum(evap) / (SUB_PERIOD_YEARS * 31), 
                                                                prec = sum(prec) / (SUB_PERIOD_YEARS * 31), 
                                                                diff_pe = (sum(prec) - sum(evap)) / (SUB_PERIOD_YEARS * 31)), 
                                       by = .(grid_id, period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]
exeves_prec_sums <- merge(evap_grid, exeves_prec_sums, by = "grid_id")

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[9] <- "Period"

gg_not_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values =  WATER_CYCLE_CHANGE_PALETTE[c(2, 4, 1, 3)]) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/day]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/day]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

to_plot_nested <- to_plot[, .N, Conditions]
to_plot_nested$N <- round(100 * to_plot_nested$N/sum(to_plot_nested$N), 0)
gg_waffle <- 
  ggplot(to_plot_nested, aes(fill = Conditions, values = N)) +
  geom_waffle(alpha = 0.5, size = 1, colour = "white", n_rows = 10) +
  scale_fill_manual(values = WATER_CYCLE_CHANGE_PALETTE[c(2, 4, 1, 3)]) + 
  xlab("") +
  ylab("") +
  theme_linedraw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") 

sub_grob <- ggplotGrob(gg_waffle)
gg_not_event <- gg_not_event + annotation_custom(sub_grob, xmin  = 0.91, xmax = 1.33, ymin = 0.96, ymax = 1.4)

ggarrange(gg_all, gg_event, gg_not_event, 
          ncol = 1, nrow = 3,
          labels = c("A", "B", "C"),
          legend = 'right', common.legend = TRUE) 
ggsave(paste0(PATH_OUTPUT_FIGURES, "implications.png"), width = 8, height = 12)





