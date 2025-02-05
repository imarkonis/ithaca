# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]

## Variables
evap <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                   evap_grid[, .(lon, lat, area, evap_volume, dataset)], by = c("lon", "lat"), all.y = TRUE)
evap <- evap[complete.cases(evap)]
evap_stats <- evap[, .(environment_volume = round(sum(evap_volume, na.rm = T),)), .(evap_quant, dataset)]
partition_evap_datasets <- dcast(evap_stats, dataset ~ evap_quant, fun = mean, na.rm = TRUE)
global <- evap_stats[, .(Global = sum(environment_volume)), .(dataset)]
evap_merged <- merge(partition_evap_datasets, global, by = c("dataset"))

saveRDS(evap_merged, paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap_datasets_global.rds"))

evap_stats[, mean_environment := mean(environment_volume), .(evap_quant)]
evap_stats[, ratio := environment_volume/mean_environment, .(evap_quant)]
evap_stats[, diff := ratio-1, .(evap_quant)]
evap_stats[, abs_diff := abs(diff),]
evap_stats[, rank_diff := rank(abs_diff), .(evap_quant)]
evap_stats[diff < 0, performance := "Under"]
evap_stats[diff > 0, performance := "Over"]
evap_stats[rank_diff < 2.6, performance := "Closest"]
evap_stats[, label_color := "black"]
evap_stats[performance == "best", label_color := "firebrick"]
saveRDS(evap_stats, paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap_datasets_for_plot.rds"))
