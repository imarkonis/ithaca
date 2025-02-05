# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]

## Variables
biome <- merge(evap_mask[, .(lat, lon, biome_short_class)], 
            evap_grid[, .(lon, lat, area, evap_volume, dataset)], by = c("lon", "lat"), all.y = TRUE)
biome <- biome[complete.cases(biome)]
biome_stats <- biome[, .(environment_volume = round(sum(evap_volume, na.rm = T),)), .(biome_short_class, dataset)]
partition_biome_datasets <- dcast(biome_stats, dataset ~ biome_short_class, fun = mean, na.rm = TRUE)
global <- biome_stats[, .(Global = sum(environment_volume)), .(dataset)]
biome_merged <- merge(partition_biome_datasets, global, by = c("dataset"))

saveRDS(biome_merged, paste0(PATH_SAVE_PARTITION_EVAP, "partition_biome_datasets_global.rds"))

biome_stats[, mean_environment := mean(environment_volume), .(biome_short_class)]
biome_stats[, ratio := environment_volume/mean_environment, .(biome_short_class)]
biome_stats[, diff := ratio-1, .(biome_short_class)]
biome_stats[, abs_diff := abs(diff),]
biome_stats[, rank_diff := rank(abs_diff), .(biome_short_class)]
biome_stats[diff < 0, performance := "Under"]
biome_stats[diff > 0, performance := "Over"]
biome_stats[rank_diff < 2.6, performance := "Closest"]
biome_stats[, label_color := "black"]
biome_stats[performance == "best", label_color := "firebrick"]
saveRDS(biome_stats, paste0(PATH_SAVE_PARTITION_EVAP, "partition_biome_datasets_for_plot.rds"))
