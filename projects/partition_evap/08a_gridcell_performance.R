# Quantify uncertainty grid-based
source('source/partition_evap.R')

## Data 
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]

evap_grid[, ensemble_mean := mean(evap_volume), .(lon, lat)]
evap_grid[, ratio := evap_volume/ensemble_mean ]
evap_grid[, diff := ratio-1, ]
evap_grid[, abs_diff := abs(diff),]
evap_grid[, rank_diff := rank(abs_diff), .(lon, lat)]
evap_grid[diff < 0, performance := "Under"]
evap_grid[diff > 0, performance := "Over"]
evap_grid[rank_diff < 2.6, performance := "Closest"]

evap_summary <- evap_grid[, .(evap_sum = sum(evap_volume), evap_area = sum(area)), .(dataset, performance)]
evap_summary[, volume_fraction := evap_sum/sum(evap_sum), .(dataset)]
evap_summary[, area_fraction := evap_area/sum(evap_area), .(dataset)]

saveRDS(evap_summary, paste0(PATH_SAVE_PARTITION_EVAP, "grid_performance_datasets.rds"))
write.table(evap_summary, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "global_fraction_performance_datasets.csv"), sep = ",", row.names = F)
