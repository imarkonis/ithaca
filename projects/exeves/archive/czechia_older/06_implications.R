install.packages("waffle")

source('source/exeves.R')
install.packages('waffle', repos="http://cloud.r-project.org")
library(pRecipe)
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

exeves_prec_means <- unique(exeves_prec[is.na(event_id), .(evap = mean(evap), 
                                                           prec = mean(prec), 
                                                           diff_pe = mean(prec) - mean(evap)), 
                                        by = .(grid_id, period)])
exeves_prec_means <- exeves_prec_means[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_means[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_means[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_means[, mean_flux := (prec + evap) / 2]
exeves_prec_means <- merge(evap_grid, exeves_prec_means, by = "grid_id")

to_plot <- copy(exeves_prec_means)
to_plot[, Conditions := factor("Uknown")]
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[9] <- "Period"
to_plot[grid_id == 2 & Period == "Up to 2001", Conditions := factor('Drier - Accelerated')] # needed for plotting purposes

gg_not_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = c('steelblue3','darkgreen' ,  'darkorange', 'darkred')) +
  scale_shape_manual(values = c(22, 21)) +
  xlab("Water residual over land (mm/day)") +
  ylab("Mean land-atmosphere water exchange (mm/day)") +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw()

to_plot_nested <- to_plot[, .N, Conditions]
to_plot_nested$N <- round(100 * to_plot_nested$N/sum(to_plot_nested$N), 0)
to_plot_nested$N[1] <- 70 # For plotting
gg_waffle <- 
  ggplot(to_plot_nested, aes(fill = Conditions, values = N)) +
  geom_waffle(alpha = 0.5, size = 1, colour = "white", n_rows = 10) +
  scale_fill_manual(values = c('steelblue3','darkgreen' ,  'darkorange', 'darkred')) + 
  xlab("") +
  ylab("") +
  theme_linedraw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") 

sub_grob <- ggplotGrob(gg_waffle)
gg_not_event <- gg_not_event + annotation_custom(sub_grob, xmin  = 1, xmax = 1.5, ymin = 1.05, ymax = 1.4)

gg_not_event_map <- ggplot(to_plot) + 
  geom_tile(
    aes(
      lon, 
      lat, 
      fill = Conditions
    ), alpha = 0.4
  ) +
  geom_sf(
    data = borders, 
    alpha = 0.1, 
    col = 'black', 
    lwd = 0.4
  ) +
  scale_x_continuous(
    breaks = seq(CZECHIA_LON_MIN, CZECHIA_LON_MAX, 2)
  ) +
  scale_y_continuous(
    breaks = seq(CZECHIA_LAT_MIN - 0.5, CZECHIA_LAT_MAX, 1)
  ) +
  scale_fill_manual(
    values = c('steelblue3','darkgreen' ,  'darkorange', 'darkred')
  ) +
  xlab('') +
  ylab('') +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(color = colset_subdued_prof[3]),
    legend.position = 'none'
  )


exeves_prec_means <- unique(exeves_prec[!is.na(event_id), .(evap = mean(evap), 
                                                            prec = mean(prec), 
                                                            diff_pe = mean(prec) - mean(evap)), 
                                        by = .(grid_id, period)])
exeves_prec_means <- exeves_prec_means[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_means[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_means[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_means[, mean_flux := (prec + evap) / 2]
exeves_prec_means <- merge(evap_grid, exeves_prec_means, by = "grid_id")

to_plot <- copy(exeves_prec_means)
to_plot[, Conditions := factor("Uknown")]
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[9] <- "Period"

gg_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux, x = diff_pe, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux, x = diff_pe, group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = c('steelblue3', 'darkred', 'darkorange',  'darkgreen')) +
  scale_shape_manual(values = c(22, 21)) +
  xlab("Water residual over land (mm/day)") +
  ylab("Mean land-atmosphere water exchange (mm/day)") +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw()

to_plot_nested <- to_plot[, .N, Conditions]
to_plot_nested$N <- round(100 * to_plot_nested$N/sum(to_plot_nested$N), 0)
to_plot_nested$N[3] <- 30 # For plotting
gg_waffle <- 
  ggplot(to_plot_nested, aes(fill = Conditions, values = N)) +
  geom_waffle(alpha = 0.5, size = 1, colour = "white", n_rows = 10) +
  scale_fill_manual(values = c('steelblue3', 'darkred', 'darkorange', 'darkgreen' )) + 
  xlab("") +
  ylab("") +
  theme_linedraw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") 

sub_grob <- ggplotGrob(gg_waffle)
gg_event <- gg_event + annotation_custom(sub_grob, xmin  = 2.6, xmax = 4.2, ymin = 1.1, ymax = 2.5)

gg_event_map <- ggplot(to_plot) + 
  geom_tile(
    aes(
      lon, 
      lat, 
      fill = Conditions
    ), alpha = 0.4
  ) +
  geom_sf(
    data = borders, 
    alpha = 0.1, 
    col = 'black', 
    lwd = 0.4
  ) +
  scale_x_continuous(
    breaks = seq(CZECHIA_LON_MIN, CZECHIA_LON_MAX, 2)
  ) +
  scale_y_continuous(
    breaks = seq(CZECHIA_LAT_MIN - 0.5, CZECHIA_LAT_MAX, 1)
  ) +
  scale_fill_manual(
    values = c('darkgreen', 'darkred', 'darkorange', 'steelblue3')
  ) +
  xlab('') +
  ylab('') +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(color = colset_subdued_prof[3]),
    legend.position = 'none'
  )

ggarrange(gg_not_event, gg_event,
          ncol = 1, nrow = 2,
          labels = c("A", "B", "C", "D"),
          legend = 'right', common.legend = TRUE) 
ggsave(paste0(PATH_OUTPUT_FIGURES, "implications.png"), width = 9, height = 12)

