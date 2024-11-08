source('source/exeves.R')

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))
heat <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_heat_grid.rds'))
temp <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_temp_grid.rds'))
sensible <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_sensible_grid.rds'))

rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves, rad, by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        prec[, .(grid_id, date, prec = value)], by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        heat, by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        temp[, .(temp = value, grid_id, date)], by = c('grid_id', 'date'))

exeves_drivers[, conditions := ordered('ExEvE')]
exeves_drivers[is.na(event_80_95_id), conditions :=  ordered('non-ExEvE')]

exeves_drivers[conditions == 'ExEvE', event_day := seq_len(.N), by = .(event_80_95_id, grid_id)]
exeves_drivers[conditions == 'ExEvE', event_duration := .N, by = .(event_80_95_id, grid_id)]

saveRDS(exeves_drivers, paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers_all.rds'))

exeves_drivers <- exeves_drivers[, .(grid_id, date, conditions, event_day, event_duration,
                   evap = value, swrad, std_swrad, lwrad, std_lwrad, temp, sensible, prec)]
saveRDS(exeves_drivers, paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))
