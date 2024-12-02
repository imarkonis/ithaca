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
pentads[, pentad_std_q95 := quantile(std_value, EXTREMES_THRES), by = grid_id]
pentads[, pentad_std_q80 := quantile(std_value, LOW_THRES), by = grid_id]
pentads[, pentad_median_qr := rq(std_value ~ date,  #quantile regression: non-stationarity
                                 tau = 0.5)$fitted, by = .(pentad, grid_id)]
pentads[, pentad_std_q95_qr := rq(std_value ~ date, 
                                  tau = EXTREMES_THRES)$fitted, by = .(pentad, grid_id)]
pentads[, value := NULL]

## Events
### Mean - Quantile 0.95 definition
exeves <- merge(evap, pentads[, .(grid_id, date, std_value, pentad_median_qr, 
                                  pentad_std_q80, pentad_std_q95, pentad_std_q95_qr)], 
                all.x = TRUE, by = c("grid_id", "date"))

exeves[, evap_event := FALSE]
exeves[, value_above_low_thres := FALSE]
exeves[, extreme := FALSE]
exeves[std_value > 0, value_above_low_thres := TRUE]
exeves[std_value > pentad_std_q95, extreme := TRUE]
exeves[, above_low_thres_id := rleid(value_above_low_thres)]
exeves[, extreme_id := rleid(extreme), .(grid_id)]

exeves[extreme == TRUE, evap_event := TRUE, .(grid_id, above_low_thres_id)] 
above_low_thres_ids_with_extreme <- exeves[extreme == TRUE, above_low_thres_id]
exeves[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves[, event_id := rleid(evap_event), .(grid_id)]
exeves[evap_event != TRUE, event_id := NA]
exeves[extreme != TRUE, extreme_id := NA]

exeves[, period := ordered('up_to_2001')]
exeves[date > END_PERIOD_1, period := ordered('after_2001')]

exeves[month(date) < 4, season := ordered("JFM")]
exeves[month(date) >= 4 & month(date) < 7, season := ordered("AMJ")]
exeves[month(date) >= 7 & month(date) < 10, season := ordered("JAS")]
exeves[month(date) >= 10, season := ordered("OND")]

### Median - Quantile regression 0.95 definition
exeves_qr <- merge(evap, pentads[, .(grid_id, date, std_value, pentad_median_qr, pentad_std_q95_qr)], all.x = TRUE, by = c("grid_id", "date"))
exeves_qr[, evap_event := FALSE]
exeves_qr[, value_above_low_thres := FALSE]
exeves_qr[, extreme := FALSE]
exeves_qr[std_value > pentad_median_qr, value_above_low_thres := TRUE]
exeves_qr[std_value > pentad_std_q95_qr, extreme := TRUE]
exeves_qr[, above_low_thres_id := rleid(value_above_low_thres)]
exeves_qr[, extreme_qr_id := rleid(extreme), .(grid_id)]

exeves_qr[extreme == TRUE, evap_event := TRUE, .(grid_id, above_low_thres_id)]
above_low_thres_ids_with_extreme <- exeves_qr[extreme == TRUE, above_low_thres_id]
exeves_qr[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves_qr[, event_qr_id := rleid(evap_event), .(grid_id)]
exeves_qr[evap_event != TRUE, event_qr_id := NA]
exeves_qr[extreme != TRUE, extreme_qr_id := NA]

### Quantile 0.8 - Quantile 0.95 definition
exeves_80_95 <- merge(evap, pentads[, .(grid_id, date, std_value, pentad_median_qr, 
                                        pentad_std_q80, pentad_std_q95, pentad_std_q95_qr)], 
                      all.x = TRUE, by = c("grid_id", "date"))

exeves_80_95[, evap_event := FALSE]
exeves_80_95[, value_above_low_thres := FALSE]
exeves_80_95[, extreme := FALSE]
exeves_80_95[std_value > pentad_std_q80, value_above_low_thres := TRUE]
exeves_80_95[std_value > pentad_std_q95, extreme := TRUE]
exeves_80_95[, above_low_thres_id := rleid(value_above_low_thres)]
exeves_80_95[, extreme_id := rleid(extreme), .(grid_id)]

exeves_80_95[extreme == TRUE, evap_event := TRUE, .(grid_id, above_low_thres_id)] 
above_low_thres_ids_with_extreme <- exeves_80_95[extreme == TRUE, above_low_thres_id]
exeves_80_95[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves_80_95[, event_80_95_id := rleid(evap_event), .(grid_id)]
exeves_80_95[evap_event != TRUE, event_80_95_id := NA]
exeves_80_95[extreme != TRUE, extreme_id := NA]

### Quantile 0.80 definition
exeves_80 <- merge(evap, pentads[, .(grid_id, date, std_value, pentad_median_qr, 
                                     pentad_std_q80)], 
                   all.x = TRUE, by = c("grid_id", "date"))

exeves_80[, evap_event := FALSE]
exeves_80[std_value > pentad_std_q80, evap_event := TRUE]
exeves_80[, event_80_id := rleid(evap_event), .(grid_id)]
exeves_80[evap_event != TRUE, event_80_id := NA]

exeves <- merge(exeves[, .(grid_id, date, season, period, value, std_value, event_id, extreme_id)], 
                exeves_qr[, .(grid_id, date, event_qr_id, extreme_qr_id)], by = c('grid_id', 'date'))

exeves <- merge(exeves, 
                exeves_80_95[, .(grid_id, date, event_80_95_id)], by = c('grid_id', 'date'))

exeves <- merge(exeves, 
                exeves_80[, .(grid_id, date, event_80_id)], by = c('grid_id', 'date'))

saveRDS(pentads, paste0(PATH_OUTPUT_DATA, 'pentads_std_', region, '.rds'))
saveRDS(exeves, paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

rm(evap); rm(exeves); gc()

# Precipitation
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec.rds'))

prec <- evap_grid[prec, on = .(lon, lat)][, lon := NULL][, lat := NULL]
prec[, q95 := quantile(value, 0.9), grid_id]
prec[value > q95, extreme_prec := TRUE][, q95 := NULL]

saveRDS(prec, paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

# Radiation
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

# Temperature
temp <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_temp_mswx.rds'))
temp <- evap_grid[temp, on = .(lon, lat)][, lon := NULL][, lat := NULL]
temp <- temp[pentads[, .(date, pentad, grid_id)], on = .(date, grid_id)]
temp[, std_value := (value - mean(value)) / sd(value), by = .(pentad, grid_id)][, pentad := NULL]

saveRDS(temp, paste0(PATH_OUTPUT_DATA, region, '_temp_grid.rds'))

# Sensible Heat
sensible <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_sensible.rds'))
sensible <- evap_grid[sensible, on = .(lon, lat)][, lon := NULL][, lat := NULL]

saveRDS(sensible, paste0(PATH_OUTPUT_DATA, region, '_sensible_grid.rds'))

# Latent Heat
LATENT <-  2.45 * 10^6 / SEC_IN_DAY #W/day / kg 
latent <- exeves[, .(grid_id, date, value = LATENT * value)]
saveRDS(latent, paste0(PATH_OUTPUT_DATA, region, '_latent_grid.rds'))

# Bowen ratio
names(sensible)[3] <- "sensible"
names(latent)[3] <- "latent"
heat <- merge(sensible, latent, by = c('grid_id', 'date'))
heat[, bowen := sensible / latent]
heat[, evap_fraction := 1 / (1 + bowen)]
saveRDS(heat, paste0(PATH_OUTPUT_DATA, region, '_heat_grid.rds'))
