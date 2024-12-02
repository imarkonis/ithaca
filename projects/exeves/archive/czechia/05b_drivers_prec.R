source('source/exeves.R')
library(pRecipe)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

n_days_before_event <- 5
exeves[, exeve := FALSE]
exeves[!is.na(event_id), exeve := TRUE]

exeves_drivers <- merge(exeves[, .(grid_id, date, season, period, event_id, evap = value, evap_std = std_value)], 
                        prec[, .(grid_id, date, prec = value)], 
                        by = c('grid_id', 'date'))

exeves_drivers[!is.na(event_id), first_event_day := min(date), .(event_id, grid_id)]
exeves_drivers[, prec_mean := lapply(.SD, frollmean,  #Pre-conditions
                                     n = n_days_before_event, 
                                     fill = NA, 
                                     align = "right"), by = grid_id, .SDcols =  'prec']
exeves_drivers[, prec_mean_std := scale(prec_mean), grid_id]



exeves_drivers_event_sums <- 
  exeves_drivers[!is.na(event_id), .(evap = sum(evap), prec = sum(prec), prec_before = n_days_before_event * prec_mean_std), 
                                            .(grid_id, event_id, month(date))] 

ggplot(exeves_drivers_event_sums[grid_id < 40 & prec < 200]) +
  geom_point(aes(x = evap, y = prec), col = 'darkblue', alpha = 0.1) +
  geom_smooth(aes(x = evap, y = prec),  col = 'darkblue', method = 'lm', se = 0) +
  geom_point(aes(x = evap, y = prec_before), col = 'steelblue3', alpha = 0.1) +
  geom_smooth(aes(x = evap, y = prec_before),  col = 'steelblue3', method = 'lm', se = 0) +
  facet_wrap(~month, scales = "free") +
  theme_minimal()

exeves_drivers_event_means <- 
  exeves_drivers[, .(evap = mean(evap), prec = mean(prec)), 
                                            .(grid_id, exeve, month(date))] 
ggplot(exeves_drivers_event_means) +
  geom_point(aes(x = evap, y = prec, col = exeve), alpha = 0.1) +
  geom_smooth(aes(x = evap, y = prec, col = exeve), method = mgcv::gam, se = 0) +
  facet_wrap(~month, scales = "free") +
  theme_minimal()

exeves_drivers_event_means_period <- 
  exeves_drivers[exeve == TRUE, .(evap = mean(evap), prec = mean(prec)), 
                                            .(grid_id, month(date), period)] 
ggplot(exeves_drivers_event_means_period) +
  geom_point(aes(x = evap, y = prec, col = period), alpha = 0.1) +
  geom_smooth(aes(x = evap, y = prec, col = period), method = mgcv::gam, se = 0) +
  facet_wrap(~month, scales = "free") +
  theme_minimal()

exeves_drivers_event_sums_period <- 
  exeves_drivers[exeve == TRUE, .(evap = sum(evap), prec = sum(prec)), 
                                            .(grid_id, month(date), period)] 
ggplot(exeves_drivers_event_sums_period) +
  geom_point(aes(x = evap, y = prec, col = period), alpha = 0.1) +
  geom_smooth(aes(x = evap, y = prec, col = period), method = mgcv::gam, se = 0) +
  facet_wrap(~month, scales = "free") +
  theme_minimal()
