source('source/exeves.R')
library(pRecipe)
library(lubridate)

region <- 'czechia'

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

exeves[, month := factor(month(date, label = TRUE))]
rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves, rad, by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        prec[, .(grid_id, date, prec = value)], by = c('grid_id', 'date'))

exeves_drivers <- exeves_drivers[, .(grid_id, period, month, event_id, evap = value, swrad, lwrad, prec)]

exeves_drivers[, conditions := ordered('ExEvE')]
exeves_drivers[is.na(event_id), conditions :=  ordered('non-ExEvE')][, event_id := NULL]
exeves_drivers_summary <- exeves_drivers[, .(evap = sum(evap) / PERIOD_YEARS, 
                                             swrad = sum(swrad) / PERIOD_YEARS, 
                                             lwrad = sum(lwrad) / PERIOD_YEARS,
                                             prec = sum(prec) / PERIOD_YEARS), by = .(month, period, conditions)]

exeves_drivers_summary <- exeves_drivers[, .(evap = sum(evap), 
                                             prec = sum(prec), 
                                             swrad = sum(swrad), 
                                             lwrad = sum(lwrad)),
                                         by = .(grid_id, month, period, conditions)]

saveRDS(exeves_drivers_summary, file = paste0(PATH_OUTPUT, 'monthly_changes.rds'))



# For means
exeves_drivers_summary <- exeves_drivers[, .(evap = mean(evap), 
                                             swrad = mean(swrad), 
                                             lwrad = mean(lwrad),
                                             prec = mean(prec)), by = .(grid_id, month, period, conditions)]

dummy <- melt(exeves_drivers_summary,  id.vars = c('grid_id', 'month', 'period', 'conditions'))

dummy <- dcast(dummy, grid_id + month + conditions + variable ~ period)

dummy[, total_value_up_to_2001 := sum(up_to_2001), .(grid_id, variable, month)]
dummy[, total_value_after_2001 := sum(after_2001), .(grid_id, variable, month)]
dummy[, ratio_exeves := after_2001 / up_to_2001]
dummy[, ratio_total := total_value_after_2001 / total_value_up_to_2001]
dummy <- dummy[conditions == "ExEvE", .(grid_id, month, variable, ratio_total, ratio_exeves)]

to_plot_1 <- dummy[, .(median = median(ratio_total),
                       q95 = quantile(ratio_total, 0.75),
                       q05 = quantile(ratio_total, 0.25), Conditions = factor('All days')), .(month, variable)]

to_plot_2 <- dummy[, .(median = median(ratio_exeves),
                       q95 = quantile(ratio_exeves, 0.75),
                       q05 = quantile(ratio_exeves, 0.25), Conditions = factor('ExEvEs')), .(month, variable)]

to_plot <- rbind(to_plot_1, to_plot_2)
to_plot <- melt(to_plot,  id.vars = c('month', 'variable', 'Conditions'), variable.name = 'stat')

variable_names <- c('Evaporation', 'SW Radiation', 'LW Radiation', 'Precipitation')
variable_labeller <- function(variable, value){
  return(variable_names[value])
}

ggplot() +
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = 1),
    color = "lightgrey"
  ) + 
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0, 2)),
    color = '#536878', alpha = .9
  ) + 
  geom_col(data = to_plot[stat == 'median' & Conditions == 'ExEvEs'],
           aes(
             x = month,
             y = value,
             fill = value
           ),
           position = "dodge2",
           show.legend = TRUE,
           alpha = .8
  ) +
  geom_point(data = to_plot[stat == 'median' & Conditions == 'All days'],
             aes(
               x = month,
               y = value
             ),
             size = 1.5,
             color =  '#536878'
  ) +
  geom_segment(data = to_plot[stat == 'median' & Conditions == 'ExEvEs'],
               aes(
                 x = month,
                 y = 0,
                 xend = month,
                 yend = 4
               ),
               color =  '#536878',
               linetype = 'dotted'
  ) + 
  facet_wrap(~variable, nrow = 1, labeller = variable_labeller) + 
  coord_polar() +
  annotate(
    x = 11.5, 
    y = 1.3, 
    label = "1", 
    geom = "text", 
    color = 'grey50', 
    family = "Bell MT",
    size = 2.6
  ) +
  annotate(
    x = 11.5, 
    y = 2.3, 
    label = "2", 
    geom = "text", 
    color = 'grey50', 
    family = "Bell MT",
    size = 2.6
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.2, 0.2),
    limits = c(-1.5, 2),
  ) + 
  scale_fill_gradientn(
    "Ratio",
    colours =  c('grey97', colset_subdued_prof[2])
  ) +
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = colset_subdued_prof[3], size = 10, vjust = 0.1),
    legend.position = "bottom",
    text = element_text(color = colset_subdued_prof[3], family = "Bell MT"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 12, color = colset_subdued_prof[3])
  )





