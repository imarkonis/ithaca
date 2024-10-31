source('source/exeves.R')

region <- 'czechia'
evap_grid <- readRDS(paste0(PATH_OUTPUT_DATA, 'grid_', region, '.rds'))

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

# Pre-processing
rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_rad <- merge(exeves, rad, by = c('grid_id', 'date'))
exeves <- merge(exeves_rad, 
                prec[, .(grid_id, date, prec = value)], by = c('grid_id', 'date'))

exeves <- evap_grid[exeves, on = 'grid_id'][, grid_id := NULL]
rm(evap_grid); gc()

# Analysis
## Severity: All values
evap_severity_period <- exeves[, .(value = sum(value)), .(lon, lat, period)]
evap_severity_period[, diff_value := diff(value), by = .(lon, lat)]
evap_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
evap_severity_period$variable <- "E: All days"

prec_severity_period <- exeves[, .(value = sum(prec)), .(lon, lat, period)]
prec_severity_period[, diff_value := diff(value), by = .(lon, lat)]
prec_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
prec_severity_period$variable <- "P: All days"

swrad_severity_period <- exeves[, .(value = sum(swrad)), .(lon, lat, period)]
swrad_severity_period[, diff_value := diff(value), by = .(lon, lat)]
swrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
swrad_severity_period$variable <- "SW: All days"

lwrad_severity_period <- exeves[, .(value = sum(lwrad)), .(lon, lat, period)]
lwrad_severity_period[, diff_value := diff(value), by = .(lon, lat)]
lwrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
lwrad_severity_period$variable <- "LW: All days"

## Severity: ExEvEs
event_evap_severity_period <- exeves[!is.na(event_id), .(value = sum(value)), .(lon, lat, period)]
event_evap_severity_period[, diff_value := diff(value), by = .(lon, lat)]
event_evap_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_evap_severity_period$variable <- "Evaporation (ExEvEs)"

event_prec_severity_period <- exeves[!is.na(event_id), .(value = sum(prec)), .(lon, lat, period)]
event_prec_severity_period[, diff_value := diff(value), by = .(lon, lat)]
event_prec_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_prec_severity_period$variable <- "Precipitation (ExEvEs)"

event_swrad_severity_period <- exeves[!is.na(event_id), .(value = sum(swrad)), .(lon, lat, period)]
event_swrad_severity_period[, diff_value := diff(value), by = .(lon, lat)]
event_swrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_swrad_severity_period$variable <- "SW Radiation (ExEvEs)"

event_lwrad_severity_period <- exeves[!is.na(event_id), .(value = sum(lwrad)), .(lon, lat, period)]
event_lwrad_severity_period[, diff_value := diff(value), by = .(lon, lat)]
event_lwrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_lwrad_severity_period$variable <- "LW Radiation (ExEvEs)"

## Intensity
event_evap_intensity_period <- exeves[!is.na(event_id), .(value = round(mean(value), 2)), .(lon, lat, period)]
event_evap_intensity_period[, diff_value := diff(value), by = .(lon, lat)]
event_evap_intensity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_evap_intensity_period$variable <- "Intensity (E)"

event_prec_intensity_period <- exeves[!is.na(event_id), .(value = round(mean(prec), 2)), .(lon, lat, period)]
event_prec_intensity_period[, diff_value := diff(value), by = .(lon, lat)]
event_prec_intensity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_prec_intensity_period$variable <- "Intensity (P)"

## Frequency
event_frequency_period <- exeves[!is.na(event_id), .(value = .N), .(lon, lat, period)]
event_frequency_period[, diff_value := diff(value), by = .(lon, lat)]
event_frequency_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_frequency_period$variable <- factor("ExEvEs Frequency")

## Duration
event_duration_period <- exeves[!is.na(event_id), .(value = .N), .(event_id, lon, lat, period)]
event_duration_period <- event_duration_period[, .(value = mean(value)), .(lon, lat, period)]
event_duration_period[, diff_value := diff(value), by = .(lon, lat)]
event_duration_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period)]
event_duration_period$variable <- factor("ExEvEs Duration")

exeve_properties_change <- rbind(event_evap_severity_period, event_prec_severity_period, 
                                 event_swrad_severity_period, event_lwrad_severity_period, 
                                 event_evap_intensity_period, event_prec_intensity_period, event_frequency_period, event_duration_period)
saveRDS(exeve_properties_change, file = paste0(PATH_OUTPUT, 'spatial_changes.rds'))

# Validation
ggplot(event_evap_severity_period[period == "up_to_2001"]) +
  geom_tile(aes(lon, lat, fill = ratio)) +
  geom_sf(data = borders, alpha = 0.1, col = 'black', lwd = 0.4) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 1) +
  theme_minimal()