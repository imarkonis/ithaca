# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]

## Variables
IPCC <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
            evap_grid[, .(lon, lat, area, evap_volume, dataset)], by = c("lon", "lat"), all.y = TRUE)
IPCC <- IPCC[complete.cases(IPCC)]
IPCC_stats <- IPCC[, .(environment_volume = round(sum(evap_volume, na.rm = T),)), .(IPCC_ref_region, dataset)]
partition_IPCC_datasets <- dcast(IPCC_stats, dataset ~ IPCC_ref_region, fun = mean, na.rm = TRUE)
global <- IPCC_stats[, .(Global = sum(environment_volume)), .(dataset)]
IPCC_merged <- merge(partition_IPCC_datasets, global, by = c("dataset"))

saveRDS(IPCC_merged, paste0(PATH_SAVE_PARTITION_EVAP, "partition_IPCC_datasets_global.rds"))

IPCC_stats[, mean_environment := mean(environment_volume), .(IPCC_ref_region)]
IPCC_stats[, ratio := environment_volume/mean_environment, .(IPCC_ref_region)]
IPCC_stats[, diff := ratio-1, .(IPCC_ref_region)]
IPCC_stats[, abs_diff := abs(diff),]
IPCC_stats[, rank_diff := rank(abs_diff), .(IPCC_ref_region)]
IPCC_stats[diff < 0, performance := "Under"]
IPCC_stats[diff > 0, performance := "Over"]
IPCC_stats[rank_diff < 2.6, performance := "Closest"]
IPCC_stats[, label_color := "black"]
IPCC_stats[performance == "best", label_color := "firebrick"]
saveRDS(IPCC_stats, paste0(PATH_SAVE_PARTITION_EVAP, "partition_IPCC_datasets_for_plot.rds"))
