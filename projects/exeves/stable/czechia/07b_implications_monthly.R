source('source/exeves.R')

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
exeves_prec_sums <- unique(exeves_prec[, .(evap = sum(evap) / (SUB_PERIOD_YEARS * 161), 
                                           prec = sum(prec) / (SUB_PERIOD_YEARS * 161), 
                                           diff_pe = (sum(prec) - sum(evap)) / (SUB_PERIOD_YEARS* 161)), 
                                       by = .(period, month(date))])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(month)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]

levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[7] <- "Period"
to_plot[month == 2 & Period == "Up to 2001", Conditions := factor('Wetter - Deccelerated')] # needed for plotting purposes
to_plot <- to_plot[order(month)]

month_names <- data.frame(
  x = to_plot[Period == "Up to 2001", diff_pe],
  y = to_plot[Period == "Up to 2001", mean_flux],
  text = month.abb
)

gg_all <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = factor(month), col = Conditions), alpha = 0.5) +
  geom_text(data = month_names, aes(x, y, label = text), cex = 3.5, nudge_x = 2, nudge_y = 2, col = 'grey40') + 
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/month]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/month]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

# Exeve days
exeves_prec_sums <- unique(exeves_prec[!is.na(event_80_95_id), .(evap = sum(evap) / (SUB_PERIOD_YEARS * 161), 
                                                                 prec = sum(prec) / (SUB_PERIOD_YEARS * 161), 
                                                                 diff_pe = (sum(prec) - sum(evap)) / (SUB_PERIOD_YEARS * 161)), 
                                       by = .(month(date), period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(month)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[7] <- "Period"
to_plot <- to_plot[order(month)]

month_names <- data.frame(
  x = to_plot[Period == "Up to 2001", diff_pe],
  y = to_plot[Period == "Up to 2001", mean_flux],
  text = month.abb
)

gg_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = month, col = Conditions), alpha = 0.5) +
  geom_text(data = month_names, aes(x, y, label = text), cex = 3.5, nudge_x = -0.5, nudge_y = 0.5, col = 'grey40') + 
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE[c(1, 3, 4, 2)]) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/month]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/month]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

# Non-exeve days
exeves_prec_sums <- unique(exeves_prec[is.na(event_80_95_id), .(evap = sum(evap) / (SUB_PERIOD_YEARS* 161), 
                                                                prec = sum(prec) / (SUB_PERIOD_YEARS* 161), 
                                                                diff_pe = (sum(prec) - sum(evap)) / (SUB_PERIOD_YEARS* 161)), 
                                       by = .(month(date), period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(month)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[7] <- "Period"
to_plot <- to_plot[order(month)]

month_names <- data.frame(
  x = to_plot[Period == "Up to 2001", diff_pe],
  y = to_plot[Period == "Up to 2001", mean_flux],
  text = month.abb
)

gg_not_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = month, col = Conditions), alpha = 0.5) +
  geom_text(data = month_names, aes(x, y, label = text), cex = 3.5, nudge_x = 1.5, nudge_y = 1.5, col = 'grey40') + 
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values =  WATER_CYCLE_CHANGE_PALETTE[c(1, 3, 4, 2)]) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/month]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/month]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

ggarrange(gg_all, gg_event, gg_not_event, 
          ncol = 1, nrow = 3,
          labels = c("A", "B", "C"),
          legend = 'right', common.legend = TRUE) 
ggsave(paste0(PATH_OUTPUT_FIGURES, "implications_monthly.png"), width = 8, height = 12)





