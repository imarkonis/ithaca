source('source/exeves.R')
library(pRecipe)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
temp <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_temp_grid.rds'))

#Preprocessing
rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves, rad, by = c('grid_id', 'date'))

#Analysis
to_plot_monthly <- exeves_drivers[!is.na(event_id), .(value = lwrad), .(period, month(date))]
ggplot(to_plot_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

to_plot_monthly <- exeves_drivers[!is.na(event_id), .(value = swrad), .(period, month(date))]
ggplot(to_plot_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

exeves_drivers_event_sums <- exeves_drivers[!is.na(event_id), .(evap = sum(value), lwrad = sum(lwrad), swrad = sum(swrad)), 
                                     .(grid_id, event_id, month(date))] 
ggplot(exeves_drivers_event_sums[grid_id < 40]) +
  geom_point(aes(x = evap, y = lwrad), col = 'darkorange', alpha = 0.1) +
  geom_smooth(aes(x = evap, y = lwrad),  col = 'darkorange', method = 'lm', se = 0) +
  geom_point(aes(x = evap, y = swrad), col = 'darkred', alpha = 0.1) +
  geom_smooth(aes(x = evap, y = swrad), col = 'darkred', method = 'lm', se = 0) +
  facet_wrap(~month, scales = "free") +
  theme_minimal()

exeves_drivers_event_sums <- 
  exeves_drivers[!is.na(event_id), .(evap = sum(std_value), lwrad = sum(std_lwrad), 
                                     swrad = sum(std_swrad)),
                                     .(grid_id, event_id, month(date))] 
ggplot(exeves_drivers_event_sums[grid_id < 40]) +
  geom_point(aes(x = evap, y = lwrad), col = 'darkorange', alpha = 0.1) +
  geom_smooth(aes(x = evap, y = lwrad),  col = 'darkorange', method = 'lm', se = 0) +
  geom_point(aes(x = evap, y = swrad), col = 'darkred', alpha = 0.1) +
  geom_smooth(aes(x = evap, y = swrad), col = 'darkred', method = 'lm', se = 0) +
  facet_wrap(~month, scales = "free") +
  theme_minimal()

exeves_temp <- merge(exeves, temp[, .(grid_id, date, temp = value)], by = c('grid_id', 'date'))
exeves_temp_event_means <- exeves_temp[, .(evap = mean(value), temp = mean(temp)),  
                                     .(event_id,  grid_id, month(date), period)]  
ggplot(exeves_temp_event_means) +
  geom_point(aes(x = evap, y = temp, col = period), alpha = 0.1) +
  geom_smooth(aes(x = evap, y = temp, col = period), method = 'lm', se = 0) +
  facet_wrap(~month, scales = "free") +
  theme_minimal()

## USED
exeves_drivers[, Conditions := ordered('ExEvE')]
exeves_drivers[is.na(event_id), Conditions :=  ordered('non-ExEvE')]
exeves_drivers[, total_rad := lwrad + swrad]


to_plot <- exeves_drivers[, .(lwrad = mean(std_lwrad), swrad = mean(std_swrad)), .(grid_id, month(date), Conditions)]
ggplot(to_plot) +
  geom_point(aes(x = lwrad, y = swrad, col = Conditions)) + 
  geom_hline(yintercept = 0, col = 'black') +
  geom_vline(xintercept = 0, col = 'black') +
  facet_wrap(~month) +
  xlab("Longwave radiation (z-score)") +
  ylab("Shortwave radiation (z-score)") +
  scale_color_manual(values = agu_palette[c(3, 1)]) +
  theme_minimal()
ggsave(paste0(PATH_OUTPUT_FIGURES, "lw_sw_rad.png"), width = 5, height = 5)

to_plot <- exeves_drivers[, .(lwrad = mean(std_lwrad), swrad = mean(std_swrad)), .(grid_id, Conditions,  month(date), period)]
to_plot[, lwrad_mean := mean(lwrad), .(Conditions,  month, period)]
to_plot[, swrad_mean := mean(swrad), .(Conditions,  month, period)]

ggplot(to_plot) +
  geom_point(aes(x = lwrad, y = swrad, col = period, shape = Conditions)) + 
  geom_hline(yintercept = 0, col = 'black') +
  geom_vline(xintercept = 0, col = 'black') +
  geom_point(aes(lwrad_mean, swrad_mean, shape = period), size = 3, col = 'red') +
  facet_wrap(~month) +
  theme_minimal()
  
to_plot <- exeves_drivers[, .(lwrad = sum(lwrad), swrad = sum(swrad)), .(grid_id, exeve,  month(date), period)]
to_plot[, lwrad_mean := mean(lwrad), .(exeve,  month, period)]
to_plot[, swrad_mean := mean(swrad), .(exeve,  month, period)]

ggplot(to_plot[exeve == TRUE & lwrad > 200]) +
  geom_point(aes(x = lwrad, y = swrad, col = period)) + 
  geom_point(aes(lwrad_mean, swrad_mean, shape = period), size = 3, col = 'red') +
  facet_wrap(~month, scales = 'free') +
  theme_minimal()
  
  
exeves_drivers_season <- unique(exeves_drivers[, .(lwrad = sum(lwrad), swrad = sum(swrad)), .(year(date), season, exeve)])
exeves_drivers_season[, all_lwrad := sum(lwrad), .(year, season)]
exeves_drivers_season[, all_swrad := sum(swrad), .(year, season)]
exeves_drivers_season[, lwrad_ratio := lwrad / all_lwrad]
exeves_drivers_season[, swrad_ratio := swrad / all_swrad]

to_plot <- melt(exeves_drivers_season[exeve == TRUE, .(year, season, lwrad_ratio, swrad_ratio)], id.vars = c('year', 'season'))
ggplot(to_plot) +
  geom_line(aes(x = year, y = value, col = variable)) +
  facet_wrap(~season) +
  theme_minimal()



to_plot <- melt(exeves_drivers_season, id.vars = c('year', 'season', 'exeve'))

ggplot(to_plot[exeve == TRUE]) +
  geom_line(aes(x = year, y = value, col = variable)) +
  geom_smooth(aes(x = year, y = value, col = variable), se = 0, method = 'lm') +
  facet_wrap(~season, scales = 'free') +
  theme_minimal()

ggplot(to_plot[exeve == FALSE]) +
  geom_line(aes(x = year, y = value, col = variable)) +
  geom_smooth(aes(x = year, y = value, col = variable), se = 0, method = 'lm') +
  facet_wrap(~season, scales = 'free') +
  theme_minimal()

exeves_drivers_season <- unique(exeves_drivers[, .(lwrad = sum(lwrad), swrad = sum(swrad)), .(year(date), season)])
to_plot <- melt(exeves_drivers_season, id.vars = c('year', 'season'))

ggplot(to_plot) +
  geom_line(aes(x = year, y = value, col = season)) +
  geom_smooth(aes(x = year, y = value, col = season), se = 0, method = 'lm') +
  facet_wrap(~variable, scales = 'free') +
  theme_minimal()





