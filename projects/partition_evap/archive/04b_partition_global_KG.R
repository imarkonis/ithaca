# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]

## Variables
KG <- merge(evap_mask[, .(lat, lon, KG_class_1_name)], 
                    evap_grid[, .(lon, lat, area, evap_volume, dataset)], by = c("lon", "lat"), all.y = TRUE)
KG <- KG[complete.cases(KG)]
KG_stats <- KG[, .(environment_volume = round(sum(evap_volume, na.rm = T),)), .(KG_class_1_name, dataset)]
partition_KG_datasets <- dcast(KG_stats, dataset ~ KG_class_1_name, fun = mean, na.rm = TRUE)
global <- KG_stats[, .(Global = sum(environment_volume)), .(dataset)]
KG_merged <- merge(partition_KG_datasets, global, by = c("dataset"))

saveRDS(KG_merged, paste0(PATH_SAVE_PARTITION_EVAP, "partition_KG_datasets_global.rds"))

KG_stats[, mean_environment := mean(environment_volume), .(KG_class_1_name)]
KG_stats[, ratio := environment_volume/mean_environment, .(KG_class_1_name)]
KG_stats[, diff := ratio-1, .(KG_class_1_name)]
KG_stats[, abs_diff := abs(diff),]
KG_stats[, rank_diff := rank(abs_diff), .(KG_class_1_name)]
KG_stats[diff < 0, performance := "underestimated"]
KG_stats[diff > 0, performance := "overestimated"]
KG_stats[rank_diff < 2.6, performance := "best"]
KG_stats[, label_color := "black"]
KG_stats[performance == "best", label_color := "firebrick"]
saveRDS(KG_stats, paste0(PATH_SAVE_PARTITION_EVAP, "partition_KG_datasets_for_plot.rds"))
