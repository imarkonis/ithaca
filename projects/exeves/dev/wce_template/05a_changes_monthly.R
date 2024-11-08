source('source/exeves.R')

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

exeves[, month := factor(month(date, label = TRUE))]
rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_changes <- merge(exeves, rad, by = c('grid_id', 'date'))
exeves_changes <- merge(exeves_changes, 
                        prec[, .(grid_id, date, prec = value)], by = c('grid_id', 'date'))

exeves_changes <- exeves_changes[, .(grid_id, period, month, event_80_95_id, evap = value, swrad, lwrad, prec)]

exeves_changes[, conditions := ordered('ExEvE')]
exeves_changes[is.na(event_80_95_id), conditions :=  ordered('non-ExEvE')][, event_80_95_id := NULL]

exeves_changes_summary <- exeves_changes[, .(evap = sum(evap), 
                                             prec = sum(prec), 
                                             swrad = sum(swrad), 
                                             lwrad = sum(lwrad)),
                                         by = .(grid_id, month, period, conditions)]

exeves_changes_mean_summary <- exeves_changes[, .(evap = mean(evap), 
                                             prec = mean(prec), 
                                             swrad = mean(swrad), 
                                             lwrad = mean(lwrad)),
                                         by = .(grid_id, month, period, conditions)]

saveRDS(exeves_changes_summary, file = paste0(PATH_OUTPUT, 'monthly_changes.rds'))
saveRDS(exeves_changes_mean_summary, file = paste0(PATH_OUTPUT, 'monthly_changes_mean.rds'))




