source('source/exeves.R')

region <- 'random_locations'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec.rds'))
heat <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_heat.rds'))
temp <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_temp.rds'))
sensible <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_sensible.rds'))

rad <- merge(lwrad[, .(KG_class_2, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(KG_class_2, date, swrad = value, std_swrad = std_value)], 
             by = c('KG_class_2', 'date'))
exeves_drivers <- merge(exeves, rad, by = c('KG_class_2', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        prec[, .(KG_class_2, date, prec = value)], by = c('KG_class_2', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        heat, by = c('KG_class_2', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        temp[, .(temp = value, KG_class_2, date)], by = c('KG_class_2', 'date'))

exeves_drivers[, conditions := ordered('ExEvE')]
exeves_drivers[is.na(event_80_95_id), conditions :=  ordered('non-ExEvE')]

exeves_drivers[conditions == 'ExEvE', event_day := seq_len(.N), by = .(event_80_95_id, KG_class_2)]
exeves_drivers[conditions == 'ExEvE', event_duration := .N, by = .(event_80_95_id, KG_class_2)]

saveRDS(exeves_drivers, paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers_all.rds'))

exeves_drivers <- exeves_drivers[, .(KG_class_2, date, conditions, event_day, event_duration,
                   evap = value, swrad, std_swrad, lwrad, std_lwrad, temp, sensible, prec)]
saveRDS(exeves_drivers, paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))
