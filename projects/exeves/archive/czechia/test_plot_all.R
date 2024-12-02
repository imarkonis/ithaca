source('source/exeves.R')
region <- 'czechia'

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))

names(evap)[3] <- "evap"
names(prec)[3] <- "prec"
names(lwrad)[3:4] <- c("lwrad", "std_lwrad")
names(swrad)[3:4] <- c("swrad", "std_swrad")
exeves[, value := NULL]

exeves_all <- merge(evap, exeves, all.x = TRUE, by = c("grid_id", "date"))
exeves_all <- merge(exeves_all, prec, by = c("grid_id", "date"))
exeves_all <- merge(exeves_all, lwrad, all.x = TRUE, by = c("grid_id", "date"))
exeves_all <- merge(exeves_all, swrad, all.x = TRUE, by = c("grid_id", "date"))

exeves_all[, event_day := seq_len(.N), by = .(event_id, grid_id)]
exeves_all[event_id > 0, extreme_day := seq_len(.N), by = .(event_id, grid_id)]
exeves_all[, event_duration := .N, by = .(event_id, grid_id)]
exeves_all[extreme_id > 0, extremes_per_event := .N, by = .(event_id, grid_id)]

test <- exeves_all[grid_id == 100]
test_months <- c(4:9)
test_year <- 2003

warm_season_start <- paste0(test_year, '-04-01')
warm_season_end <- paste0(test_year, '-10-01')
cold_season_start <- copy(warm_season_end)
cold_season_end <-  paste0(test_year + 1, '-04-01')

ggplot(data = test[date >= warm_season_start & date <= warm_season_end]) +
  geom_point(data = test[date >= warm_season_start & date <= warm_season_end & !is.na(event_qr_id)],
             aes(date, evap), col = colset_subdued_prof[3], size = 5, shape = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_point(data = test[date >= warm_season_start & date <= warm_season_end & !is.na(event_id)],
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = test[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[4]) +
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw()

ggplot(data = test[date >= cold_season_start & date <= cold_season_end]) +
  geom_point(data = test[date >= cold_season_start & date <= cold_season_end & !is.na(event_qr_id)],
             aes(date, evap), col = colset_subdued_prof[3], size = 5, shape = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_point(data = test[date >= cold_season_start & date <= cold_season_end & !is.na(event_id)],
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = test[date >= cold_season_start & date <= cold_season_end & !is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[4]) +
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw()


test_year <- 2012
ggplot(data = test[year(date) == test_year & month(date) %in% test_months]) +
geom_hline(yintercept = 0) +
geom_point(data = test[year(date) == test_year & month(date) %in% test_months & extreme_prec == TRUE],
           aes(date, 0), col = colset_subdued_prof[4], size = 4, shape = 15) +
geom_point(data = test[year(date) == test_year & month(date) %in% test_months & !is.na(event_qr_id)],
           aes(date, evap), col = 'purple', size = 5, shape = 0) +
geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
geom_point(data = test[year(date) == test_year & month(date) %in% test_months & !is.na(event_id)],
aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
geom_point(data = test[year(date) == test_year & month(date) %in% test_months & !is.na(extreme_id)],
aes(date, evap), col = colset_subdued_prof[4]) +
geom_line(data = test[year(date) == test_year & month(date) %in% test_months],
aes(date, (prec/10)), col = colset_subdued_prof[2]) +
geom_line(data = test[year(date) == test_year & month(date) %in% test_months],
aes(date, std_lwrad), col = 'darkgreen', alpha = 0.4) +
geom_line(data = test[year(date) == test_year & month(date) %in% test_months],
aes(date, std_swrad), col = 'orange', alpha = 0.4) +
#geom_line(aes(date, scale(swrad/lwrad)), col = 'red') +
geom_point(data = test[year(date) == test_year & month(date) %in% test_months & !is.na(extreme_qr_id)],
           aes(date, evap), col = 'red', size = 6, shape = 0) +
theme_minimal()

ggplot(data = test[year(date) == test_year & !is.na(season)]) +
  geom_hline(yintercept = 0) +
  geom_point(data = test[year(date) == test_year & month(date) %in% test_months & extreme_prec == TRUE], 
             aes(date, 0), col = colset_subdued_prof[4], size = 4, shape = 15) +
  geom_point(data = test[year(date) == test_year & month(date) %in% test_months & !is.na(event_qr_id)], 
             aes(date, evap), col = 'purple', size = 5, shape = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_point(data = test[year(date) == test_year & !is.na(event_id) & !is.na(season)], 
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = test[year(date) == test_year & !is.na(extreme_id) & !is.na(season)], 
             aes(date, evap), col = colset_subdued_prof[4]) +
  geom_line(data = test[year(date) == test_year & season == 'cold']aes(date, (prec/10)), col = colset_subdued_prof[2]) +
  geom_line(data = test[year(date) == test_year & season == 'cold'], aes(date, std_lwrad), col = 'darkgreen', alpha = 0.4) +
  geom_line(data = test[year(date) == test_year & season == 'warm'], aes(date, std_swrad), col = 'orange', alpha = 0.4) +
  facet_wrap(~season) + 
  geom_line(aes(date, scale(swrad/lwrad)), col = 'red') +
  geom_point(data = test[year(date) == test_year & month(date) %in% test_months & !is.na(extreme_qr_id)], 
             aes(date, evap), col = 'red', size = 6, shape = 0) +
  theme_minimal()


