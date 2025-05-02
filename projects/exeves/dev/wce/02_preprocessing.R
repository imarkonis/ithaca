source('source/exeves.R')
require(pRecipe)
require(quantreg)

region <- 'wce'
spatial_info <- pRecipe_masks()[ipcc_short_region == "WCE"]

# Evaporation
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_gleam.rds'))
evap <- evap[order(lon, lat)]
evap[, `:=`(grid_id, .GRP), by = list(lat, lon)]
evap_grid <- unique(evap[, .(lon, lat, grid_id)])
evap <- evap[, .(grid_id, date, value)]

n_cells <-  evap[, .N, grid_id]
max_cells <- max(n_cells$N)
valid_cells_ids <- n_cells[N == max_cells, grid_id] #some islands have missing values
evap <- evap[grid_id %in% valid_cells_ids]
evap_grid <- evap_grid[grid_id %in% valid_cells_ids]

spatial_info_region <- evap_grid[spatial_info, on = .(lon, lat)]
spatial_info_region <- spatial_info_region[complete.cases(spatial_info_region)]

evap_grid <- evap_grid[grid_id %in% spatial_info_region$grid_id]
evap <- evap[grid_id %in% evap_grid$grid_id]

## Pentads
pentads <- copy(evap)
pentads[, pentad := ceiling((yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5 )]
pentads[, std_value := (value - mean(value)) / sd(value), by = .(pentad, grid_id)]
pentads[, pentad_std_q90 := quantile(std_value, EXTREMES_THRES), by = grid_id]
pentads[, value := NULL]

## Events
### Stationarity assumption
exeves <- merge(evap, pentads[, .(grid_id, date, std_value, pentad_std_q90)], 
                all.x = TRUE, by = c("grid_id", "date"))

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

exeves <- exeves[, .(grid_id, date, season, period, value, std_value, event_id, extreme_id)]
      
saveRDS(evap_grid, paste0(PATH_OUTPUT_DATA, "grid_", region, ".rds"))
saveRDS(spatial_info_region, paste0(PATH_OUTPUT_DATA, "spatial_info_", region, ".rds"))
saveRDS(evap, paste0(PATH_OUTPUT_DATA, region, '_evap_grid.rds'))
saveRDS(pentads, paste0(PATH_OUTPUT_DATA, 'pentads_std_', region, '.rds'))
saveRDS(exeves, paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

rm(evap); rm(exeves); gc()

# Precipitation
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec.rds'))
prec <- evap_grid[prec, on = .(lon, lat)][, lon := NULL][, lat := NULL]
prec <- prec[!is.na(grid_id)]
prec[, q90 := quantile(value, 0.9), grid_id]
prec[value > q90, extreme_prec := TRUE][, q90 := NULL]

saveRDS(prec, paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

# Radiation
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad.rds'))

lwrad <- evap_grid[lwrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]
swrad <- evap_grid[swrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]

lwrad <- lwrad[!is.na(grid_id)]
swrad <- swrad[!is.na(grid_id)]

lwrad <- lwrad[pentads[, .(date, pentad, grid_id)], on = .(date, grid_id)]
swrad <- swrad[pentads[, .(date, pentad, grid_id)], on = .(date, grid_id)]

lwrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, grid_id)][, pentad := NULL]
swrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, grid_id)][, pentad := NULL]

saveRDS(lwrad, paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
saveRDS(swrad, paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))


