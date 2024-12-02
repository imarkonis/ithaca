source('source/exeves.R')
require(quantreg)

region <- 'czechia'

# Evaporation
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_gleam.rds'))
evap <- evap[order(lon, lat)]
evap[, `:=`(grid_id, .GRP), by = list(lat, lon)]

evap_grid <- unique(evap[, .(lon, lat, grid_id)])
evap <- evap[, .(grid_id, date, value)]

## Pentads
pentads <- copy(evap)
pentads[, pentad := ceiling((yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5 )]
pentads[, std_value := (value - mean(value)) / sd(value), by = .(pentad, grid_id)]
pentads[, pentad_std_q90 := quantile(std_value, EXTREMES_THRES), by = grid_id]
pentads[, pentad_median_qr := rq(std_value ~ date,  #quantile regression: non-stationarity
                                 tau = 0.5)$fitted, by = .(pentad, grid_id)]
pentads[, pentad_std_q90_qr := rq(std_value ~ date, 
                                 tau = EXTREMES_THRES)$fitted, by = .(pentad, grid_id)]
pentads[, value := NULL]

## Events
### Stationarity assumption
exeves <- merge(evap, pentads[, .(grid_id, date, std_value, pentad_median_qr, 
                                  pentad_std_q90, pentad_std_q90_qr)], 
                all.x = TRUE, by = c("grid_id", "date"))

ggplot(exeves[grid_id == 1 & year(date)]) +
  geom_line(aes(x = date, y = std_value)) +
  geom_line(aes(x = date, y = 0), col = 'green') +
  geom_line(aes(x = date, y = pentad_std_q90), col = 'orange') +
  theme_minimal()

ggplot(exeves[grid_id == 1 & year(date)]) +
  geom_line(aes(x = date, y = std_value)) +
  geom_line(aes(x = date, y = pentad_median_qr), col = 'green') +
  geom_line(aes(x = date, y = pentad_std_q90_qr), col = 'orange') +
    theme_minimal()

exeves[, evap_event := FALSE]
exeves[, value_above_mean := FALSE]
exeves[, extreme := FALSE]
exeves[std_value > 0, value_above_mean := TRUE]
exeves[std_value > pentad_std_q90, extreme := TRUE]
exeves[, above_mean_id := rleid(value_above_mean)]
exeves[, extreme_id := rleid(extreme), .(grid_id)]

exeves[extreme == TRUE, evap_event := TRUE, .(grid_id, above_mean_id)] 
above_mean_ids_with_extreme <- exeves[extreme == TRUE, above_mean_id]
exeves[above_mean_id %in% above_mean_ids_with_extreme, evap_event := TRUE]
exeves[, event_id := rleid(evap_event), .(grid_id)]
exeves[evap_event != TRUE, event_id := NA]
exeves[extreme != TRUE, extreme_id := NA]

exeves[, period := ordered('up_to_2001')]
exeves[date > END_PERIOD_1, period := ordered('after_2001')]

exeves[month(date) < 4, season := ordered("JFM")]
exeves[month(date) >= 4 & month(date) < 7, season := ordered("AMJ")]
exeves[month(date) >= 7 & month(date) < 10, season := ordered("JAS")]
exeves[month(date) >= 10, season := ordered("OND")]

### Non-stationarity assumption
exeves_qr <- merge(evap, pentads[, .(grid_id, date, std_value, pentad_median_qr, pentad_std_q90_qr)], all.x = TRUE, by = c("grid_id", "date"))
exeves_qr[, evap_event := FALSE]
exeves_qr[, value_above_mean := FALSE]
exeves_qr[, extreme := FALSE]
exeves_qr[std_value > pentad_median_qr, value_above_mean := TRUE]
exeves_qr[std_value > pentad_std_q90_qr, extreme := TRUE]
exeves_qr[, above_mean_id := rleid(value_above_mean)]
exeves_qr[, extreme_qr_id := rleid(extreme), .(grid_id)]

exeves_qr[extreme == TRUE, evap_event := TRUE, .(grid_id, above_mean_id)]
above_mean_ids_with_extreme <- exeves_qr[extreme == TRUE, above_mean_id]
exeves_qr[above_mean_id %in% above_mean_ids_with_extreme, evap_event := TRUE]
exeves_qr[, event_qr_id := rleid(evap_event), .(grid_id)]
exeves_qr[evap_event != TRUE, event_qr_id := NA]
exeves_qr[extreme != TRUE, extreme_qr_id := NA]

exeves <- merge(exeves[, .(grid_id, date, season, period, value, std_value, event_id, extreme_id)], 
      exeves_qr[, .(grid_id, date, event_qr_id, extreme_qr_id)], by = c('grid_id', 'date'))
      
saveRDS(evap_grid, paste0(PATH_OUTPUT_DATA, "grid_", region, ".rds"))
saveRDS(evap, paste0(PATH_OUTPUT_DATA, region, '_evap_grid.rds'))
saveRDS(pentads, paste0(PATH_OUTPUT_DATA, 'pentads_std_', region, '.rds'))
saveRDS(exeves, paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

rm(evap); rm(exeves); gc()

# Precipitation
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec.rds'))

prec <- evap_grid[prec, on = .(lon, lat)][, lon := NULL][, lat := NULL]
prec[, q90 := quantile(value, 0.9), grid_id]
prec[value > q90, extreme_prec := TRUE][, q90 := NULL]

saveRDS(prec, paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

#rad
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad.rds'))

lwrad <- evap_grid[lwrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]
swrad <- evap_grid[swrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]

lwrad <- lwrad[pentads[, .(date, pentad, grid_id)], on = .(date, grid_id)]
swrad <- swrad[pentads[, .(date, pentad, grid_id)], on = .(date, grid_id)]

lwrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, grid_id)][, pentad := NULL]
swrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, grid_id)][, pentad := NULL]

saveRDS(lwrad, paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
saveRDS(swrad, paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))

#temp
temp <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_temp.rds'))
temp <- evap_grid[temp, on = .(lon, lat)][, lon := NULL][, lat := NULL]

saveRDS(temp, paste0(PATH_OUTPUT_DATA, region, '_temp_grid.rds'))


