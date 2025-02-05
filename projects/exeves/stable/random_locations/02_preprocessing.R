source('source/exeves.R')
require(quantreg)

# Evaporation
region <- 'random_locations'
evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'evap_', region, '_gleam.rds'))

old_levels <- levels(evap$KG_class_2)
evap$KG_class_2 <- factor(evap$KG_class_2, sort(old_levels, decreasing = F))

## Pentads
pentads <- copy(evap)
pentads[, pentad := ceiling((yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5 )]
pentads[, std_value := (value - mean(value)) / sd(value), by = .(pentad, KG_class_2)]
pentads[, pentad_std_q95 := quantile(std_value, EXTREMES_THRES), by = KG_class_2]
pentads[, pentad_std_q80 := quantile(std_value, LOW_THRES), by = KG_class_2]
pentads[, pentad_median_qr := rq(std_value ~ date,  #quantile regression: non-stationarity
                                 tau = 0.5)$fitted, by = .(pentad, KG_class_2)]
pentads[, pentad_std_q95_qr := rq(std_value ~ date, 
                                 tau = EXTREMES_THRES)$fitted, by = .(pentad, KG_class_2)]
pentads[, value := NULL]

## Events
### Mean - Quantile 0.95 definition
exeves <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, 
                                  pentad_std_q80, pentad_std_q95, pentad_std_q95_qr)], 
                all.x = TRUE, by = c("KG_class_2", "date"))

exeves[, evap_event := FALSE]
exeves[, value_above_low_thres := FALSE]
exeves[, extreme := FALSE]
exeves[std_value > 0, value_above_low_thres := TRUE]
exeves[std_value > pentad_std_q95, extreme := TRUE]
exeves[, above_low_thres_id := rleid(value_above_low_thres)]
exeves[, extreme_id := rleid(extreme), .(KG_class_2)]

exeves[extreme == TRUE, evap_event := TRUE, .(KG_class_2, above_low_thres_id)] 
above_low_thres_ids_with_extreme <- exeves[extreme == TRUE, above_low_thres_id]
exeves[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves[, event_id := rleid(evap_event), .(KG_class_2)]
exeves[evap_event != TRUE, event_id := NA]
exeves[extreme != TRUE, extreme_id := NA]

exeves[, period := ordered('up_to_2001')]
exeves[date > END_PERIOD_1, period := ordered('after_2001')]

exeves[month(date) < 4, season := ordered("JFM")]
exeves[month(date) >= 4 & month(date) < 7, season := ordered("AMJ")]
exeves[month(date) >= 7 & month(date) < 10, season := ordered("JAS")]
exeves[month(date) >= 10, season := ordered("OND")]

### Median - Quantile regression 0.95 definition
exeves_qr <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, pentad_std_q95_qr)], all.x = TRUE, by = c("KG_class_2", "date"))
exeves_qr[, evap_event := FALSE]
exeves_qr[, value_above_low_thres := FALSE]
exeves_qr[, extreme := FALSE]
exeves_qr[std_value > pentad_median_qr, value_above_low_thres := TRUE]
exeves_qr[std_value > pentad_std_q95_qr, extreme := TRUE]
exeves_qr[, above_low_thres_id := rleid(value_above_low_thres)]
exeves_qr[, extreme_qr_id := rleid(extreme), .(KG_class_2)]

exeves_qr[extreme == TRUE, evap_event := TRUE, .(KG_class_2, above_low_thres_id)]
above_low_thres_ids_with_extreme <- exeves_qr[extreme == TRUE, above_low_thres_id]
exeves_qr[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves_qr[, event_qr_id := rleid(evap_event), .(KG_class_2)]
exeves_qr[evap_event != TRUE, event_qr_id := NA]
exeves_qr[extreme != TRUE, extreme_qr_id := NA]

### Quantile 0.8 - Quantile 0.95 definition
exeves_80_95 <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, 
                                  pentad_std_q80, pentad_std_q95, pentad_std_q95_qr)], 
                all.x = TRUE, by = c("KG_class_2", "date"))

exeves_80_95[, evap_event := FALSE]
exeves_80_95[, value_above_low_thres := FALSE]
exeves_80_95[, extreme := FALSE]
exeves_80_95[std_value > pentad_std_q80, value_above_low_thres := TRUE]
exeves_80_95[std_value > pentad_std_q95, extreme := TRUE]
exeves_80_95[, above_low_thres_id := rleid(value_above_low_thres)]
exeves_80_95[, extreme_id := rleid(extreme), .(KG_class_2)]

exeves_80_95[extreme == TRUE, evap_event := TRUE, .(KG_class_2, above_low_thres_id)] 
above_low_thres_ids_with_extreme <- exeves_80_95[extreme == TRUE, above_low_thres_id]
exeves_80_95[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves_80_95[, event_80_95_id := rleid(evap_event), .(KG_class_2)]
exeves_80_95[evap_event != TRUE, event_80_95_id := NA]
exeves_80_95[extreme != TRUE, extreme_id := NA]

### Quantile 0.80 definition
exeves_80 <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, 
                                  pentad_std_q80)], 
                all.x = TRUE, by = c("KG_class_2", "date"))

exeves_80[, evap_event := FALSE]
exeves_80[std_value > pentad_std_q80, evap_event := TRUE]
exeves_80[, event_80_id := rleid(evap_event), .(KG_class_2)]
exeves_80[evap_event != TRUE, event_80_id := NA]

exeves <- merge(exeves[, .(KG_class_2, date, season, period, value, std_value, event_id, extreme_id)], 
      exeves_qr[, .(KG_class_2, date, event_qr_id, extreme_qr_id)], by = c('KG_class_2', 'date'))
      
exeves <- merge(exeves, 
      exeves_80_95[, .(KG_class_2, date, event_80_95_id)], by = c('KG_class_2', 'date'))
      
exeves <- merge(exeves, 
      exeves_80[, .(KG_class_2, date, event_80_id)], by = c('KG_class_2', 'date'))

saveRDS(pentads, paste0(PATH_OUTPUT_DATA, 'pentads_std_', region, '.rds'))
saveRDS(exeves, paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

rm(evap); rm(exeves); gc()

# Precipitation
prec <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_', region, '.rds'))
old_levels <- levels(prec$KG_class_2)
prec$KG_class_2 <- factor(prec$KG_class_2, sort(old_levels, decreasing = F))

prec[, q95 := quantile(value, 0.9), KG_class_2]
prec[value > q95, extreme_prec := TRUE][, q95 := NULL]

saveRDS(prec, paste0(PATH_OUTPUT_DATA, region, '_prec.rds'))

# Radiation
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, 'lwrad_', region, '.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, 'swrad_', region, '.rds'))

old_levels <- levels(lwrad$KG_class_2)
lwrad$KG_class_2 <- factor(lwrad$KG_class_2, sort(old_levels, decreasing = F))
old_levels <- levels(swrad$KG_class_2)
swrad$KG_class_2 <- factor(swrad$KG_class_2, sort(old_levels, decreasing = F))

lwrad <- lwrad[pentads[, .(date, pentad, KG_class_2)], on = .(date, KG_class_2)]
swrad <- swrad[pentads[, .(date, pentad, KG_class_2)], on = .(date, KG_class_2)]

lwrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, KG_class_2)][, pentad := NULL]
swrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, KG_class_2)][, pentad := NULL]

saveRDS(lwrad, paste0(PATH_OUTPUT_DATA, region, '_lwrad.rds'))
saveRDS(swrad, paste0(PATH_OUTPUT_DATA, region, '_swrad.rds'))

# Temperature
temp <- readRDS(paste0(PATH_OUTPUT_DATA, 'temp_', region, '.rds'))
old_levels <- levels(temp$KG_class_2)
temp$KG_class_2 <- factor(temp$KG_class_2, sort(old_levels, decreasing = F))

saveRDS(temp, paste0(PATH_OUTPUT_DATA, region, '_temp.rds'))

# Sensible Heat
sensible <- readRDS(paste0(PATH_OUTPUT_DATA, 'sensible_', region, '.rds'))
old_levels <- levels(sensible$KG_class_2)
sensible$KG_class_2 <- factor(sensible$KG_class_2, sort(old_levels, decreasing = F))

saveRDS(sensible, paste0(PATH_OUTPUT_DATA, region, '_sensible.rds'))

# Latent Heat
LATENT <-  2.45 * 10^6 / SEC_IN_DAY #W/day / kg 
latent <- exeves[, .(KG_class_2, date, value = LATENT * value)]
old_levels <- levels(latent$KG_class_2)
latent$KG_class_2 <- factor(latent$KG_class_2, sort(old_levels, decreasing = F))
saveRDS(latent, paste0(PATH_OUTPUT_DATA, region, '_latent.rds'))

# Bowen ratio
names(sensible)[3] <- "sensible"
names(latent)[3] <- "latent"
heat <- merge(sensible, latent, by = c('KG_class_2', 'date'))
heat[, bowen := sensible / latent]
heat[, evap_fraction := 1 / (1 + bowen)]
saveRDS(heat, paste0(PATH_OUTPUT_DATA, region, '_heat.rds'))



