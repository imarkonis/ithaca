# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]

## Variables
land_cover <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                    evap_grid[, .(lon, lat, area, evap_volume, dataset)], by = c("lon", "lat"), all.y = TRUE)
land_cover <- land_cover[complete.cases(land_cover)]
land_cover_stats <- land_cover[, .(environment_volume = round(sum(evap_volume, na.rm = T),)), .(land_cover_short_class, dataset)]
partition_land_cover_datasets <- dcast(land_cover_stats, dataset ~ land_cover_short_class, fun = mean, na.rm = TRUE)
global <- land_cover_stats[, .(Global = sum(environment_volume)), .(dataset)]
land_cover_merged <- merge(partition_land_cover_datasets, global, by = c("dataset"))

saveRDS(land_cover_merged, paste0(PATH_SAVE_PARTITION_EVAP, "partition_land_cover_datasets_global.rds"))
global <- land_cover_stats[, .(environment_volume = sum(environment_volume)), .(dataset)]
global[, land_cover_short_class := "Global"]

land_cover_stats <- merge(land_cover_stats, global, by = c("dataset", "land_cover_short_class", "environment_volume"), all = T)
land_cover_stats[, mean_environment := mean(environment_volume), .(land_cover_short_class)]
land_cover_stats[, ratio := environment_volume/mean_environment, .(land_cover_short_class)]
land_cover_stats[, diff := ratio-1, .(land_cover_short_class)]
land_cover_stats[, abs_diff := abs(diff),]
land_cover_stats[, rank_diff := rank(abs_diff), .(land_cover_short_class)]
land_cover_stats[diff < 0, performance := "Under"]
land_cover_stats[diff > 0, performance := "Over"]
land_cover_stats[rank_diff < 2.6, performance := "Closest"]
land_cover_stats[, label_color := "black"]
land_cover_stats[performance == "best", label_color := "firebrick"]
saveRDS(land_cover_stats, paste0(PATH_SAVE_PARTITION_EVAP, "partition_land_cover_datasets_for_plot.rds"))
