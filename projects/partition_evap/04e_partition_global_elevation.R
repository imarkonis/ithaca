# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]

## Variables
elevation <- merge(evap_mask[, .(lat, lon, elev_class)], 
              evap_grid[, .(lon, lat, area, evap_volume, dataset)], by = c("lon", "lat"), all.y = TRUE)
elevation <- elevation[complete.cases(elevation)]
elevation_stats <- elevation[, .(environment_volume = round(sum(evap_volume, na.rm = T),)), .(elev_class, dataset)]
partition_elevation_datasets <- dcast(elevation_stats, dataset ~ elev_class, fun = mean, na.rm = TRUE)
global <- elevation_stats[, .(Global = sum(environment_volume)), .(dataset)]
elevation_merged <- merge(partition_elevation_datasets, global, by = c("dataset"))

saveRDS(elevation_merged, paste0(PATH_SAVE_PARTITION_EVAP, "partition_elevation_datasets_global.rds"))

elevation_stats[, mean_environment := mean(environment_volume), .(elev_class)]
elevation_stats[, ratio := environment_volume/mean_environment, .(elev_class)]
elevation_stats[, diff := ratio-1, .(elev_class)]
elevation_stats[, abs_diff := abs(diff),]
elevation_stats[, rank_diff := rank(abs_diff), .(elev_class)]
elevation_stats[diff < 0, performance := "Under"]
elevation_stats[diff > 0, performance := "Over"]
elevation_stats[rank_diff < 2.6, performance := "Closest"]
elevation_stats[, label_color := "black"]
elevation_stats[performance == "best", label_color := "firebrick"]
saveRDS(elevation_stats, paste0(PATH_SAVE_PARTITION_EVAP, "partition_elevation_datasets_for_plot.rds"))
