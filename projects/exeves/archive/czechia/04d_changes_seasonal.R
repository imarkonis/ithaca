source('source/exeves.R')
library(pRecipe)

region <- 'czechia'

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves, rad, by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        prec[, .(grid_id, date, prec = value)], by = c('grid_id', 'date'))

exeves_drivers <- exeves_drivers[, .(grid_id, period, season, event_id, evap = value, swrad, lwrad, prec)]

exeves_drivers[, conditions := ordered('ExEvE')]
exeves_drivers[is.na(event_id), conditions :=  ordered('non-ExEvE')][, event_id := NULL]
exeves_drivers_summary <- exeves_drivers[, .(evap = sum(evap) / PERIOD_YEARS, 
                           swrad = sum(swrad) / PERIOD_YEARS, 
                           lwrad = sum(lwrad) / PERIOD_YEARS,
                           prec = sum(prec) / PERIOD_YEARS), by = .(season, period, conditions)]

to_plot <- melt(exeves_drivers_summary,  id.vars = c('season', 'period', 'conditions'))
to_plot <- dcast(to_plot, season + conditions + variable ~ period)
to_plot[, total_value_up_to_2001 := sum(up_to_2001), .(variable, season)]
to_plot[, total_value_after_2001 := sum(after_2001), .(variable, season)]
to_plot[, ratio_exeves := after_2001 / up_to_2001]
to_plot[, ratio_total := total_value_after_2001 / total_value_up_to_2001]
to_plot <- to_plot[conditions == "ExEvE", .(season, variable, ratio_total, ratio_exeves)]

ggplot(to_plot) +
  geom_point(aes(x = season, y = ratio_exeves), col = agu_palette[1]) +
  geom_point(aes(x = season, y = ratio_total), col = agu_palette[3]) +
  facet_wrap(~variable, nrow = 1) +
  theme_linedraw() + 
  theme(panel.spacing = unit(0.3, "lines"))

exeves_drivers_summary <- exeves_drivers[, .(evap = sum(evap), 
                                             swrad = sum(swrad), 
                                             lwrad = sum(lwrad),
                                             prec = sum(prec)), by = .(grid_id, season, period, conditions)]

dummy <- melt(exeves_drivers_summary,  id.vars = c('grid_id', 'season', 'period', 'conditions'))

dummy <- dcast(dummy, grid_id + season + conditions + variable ~ period)

dummy[, total_value_up_to_2001 := sum(up_to_2001), .(grid_id, variable, season)]
dummy[, total_value_after_2001 := sum(after_2001), .(grid_id, variable, season)]
dummy[, ratio_exeves := after_2001 / up_to_2001]
dummy[, ratio_total := total_value_after_2001 / total_value_up_to_2001]
dummy <- dummy[conditions == "ExEvE", .(grid_id, season, variable, ratio_total, ratio_exeves)]

to_plot_1 <- dummy[, .(median = median(ratio_total),
                     q95 = quantile(ratio_total, 0.75),
                     q05 = quantile(ratio_total, 0.25), Conditions = factor('All days')), .(season, variable)]

to_plot_2 <- dummy[, .(median = median(ratio_exeves),
                     q95 = quantile(ratio_exeves, 0.75),
                     q05 = quantile(ratio_exeves, 0.25), Conditions = factor('ExEvEs')), .(season, variable)]

to_plot <- rbind(to_plot_1, to_plot_2)
to_plot <- melt(to_plot,  id.vars = c('season', 'variable', 'Conditions'), variable.name = 'stat')

variable_names <- c('Evaporation', 'SW Radiation', 'LW Radiation', 'Precipitation')
variable_labeller <- function(variable, value){
  return(variable_names[value])
}

ggplot() +
  geom_hline(yintercept = 1, col = 'grey60') +
  geom_point(data = to_plot[stat == 'median'], aes(x = season, y = value, col = Conditions)) +
  geom_line(data = to_plot, aes(x = season, y = value, col = Conditions)) +
  facet_wrap(~variable, nrow = 1, labeller = variable_labeller) +
  xlab('') +
  ylab('Ratio') +
  scale_color_manual(values = colset_subdued_prof[c(2, 1)]) +  
  theme_linedraw() + 
  theme(panel.spacing = unit(0.3, "lines")) +
  theme(strip.background = element_rect(fill = colset_subdued_prof[3]))

to_plot <- melt(to_plot_1,  id.vars = c('season', 'variable', 'Conditions'), variable.name = 'stat')
ggplot() +
  geom_hline(yintercept = 1, col = 'grey60') +
  geom_point(data = to_plot[stat == 'median'], aes(x = season, y = value, col = Conditions)) +
  geom_line(data = to_plot, aes(x = season, y = value, col = Conditions)) +
  facet_wrap(~variable, nrow = 1, labeller = variable_labeller) +
  xlab('') +
  ylab('Ratio') +
  scale_color_manual(values = colset_subdued_prof[c(2, 1)]) +  
  theme_linedraw() + 
  theme(panel.spacing = unit(0.3, "lines")) +
  theme(strip.background = element_rect(fill = colset_subdued_prof[3]))


# For means
exeves_drivers_summary <- exeves_drivers[, .(evap = mean(evap), 
                                             swrad = mean(swrad), 
                                             lwrad = mean(lwrad),
                                             prec = mean(prec)), by = .(grid_id, season, period, conditions)]

dummy <- melt(exeves_drivers_summary,  id.vars = c('grid_id', 'season', 'period', 'conditions'))

dummy <- dcast(dummy, grid_id + season + conditions + variable ~ period)

dummy[, total_value_up_to_2001 := sum(up_to_2001), .(grid_id, variable, season)]
dummy[, total_value_after_2001 := sum(after_2001), .(grid_id, variable, season)]
dummy[, ratio_exeves := after_2001 / up_to_2001]
dummy[, ratio_total := total_value_after_2001 / total_value_up_to_2001]
dummy <- dummy[conditions == "ExEvE", .(grid_id, season, variable, ratio_total, ratio_exeves)]

to_plot_1 <- dummy[, .(median = median(ratio_total),
                     q95 = quantile(ratio_total, 0.75),
                     q05 = quantile(ratio_total, 0.25), Conditions = factor('All days')), .(season, variable)]

to_plot_2 <- dummy[, .(median = median(ratio_exeves),
                     q95 = quantile(ratio_exeves, 0.75),
                     q05 = quantile(ratio_exeves, 0.25), Conditions = factor('ExEvEs')), .(season, variable)]

to_plot <- rbind(to_plot_1, to_plot_2)
to_plot <- melt(to_plot,  id.vars = c('season', 'variable', 'Conditions'), variable.name = 'stat')


ggplot() +
  geom_hline(yintercept = 1, col = 'grey60') +
  geom_line(data = to_plot, aes(x = season, y = value, col = Conditions)) +
  geom_point(data = to_plot[stat == 'median'], aes(x = season, y = value, col = Conditions)) +
  facet_wrap(~variable, nrow = 1, labeller = variable_labeller, scales = 'free') +
  xlab('') +
  ylab('Ratio') +
  scale_color_manual(values = agu_palette[c(1, 3)]) +  
  theme_linedraw() + 
  theme(panel.spacing = unit(0.3, "lines")) +
  theme(strip.background = element_rect(fill = 'grey20'))

        