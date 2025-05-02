# Interannual variance of  environments
source('source/partition_evap.R')

## Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))

## evaporation quantiles ----
data <- merge(evap_mask[, .(lat, lon, evap_quant)], 
              evap[, .(lon, lat, area, evap_volume, dataset, year)], by = c("lon", "lat"), all.y = TRUE)
data_stats <- data[, .(environment_volume = round(sum(evap_volume, na.rm = T),), area_sum = sum(area)), .(evap_quant, dataset, year)]
global <- data_stats[, .(environment_volume = sum(environment_volume), area_sum = sum(area_sum)), .(dataset, year)]
global[, evap_quant := "Global"]
data_merged <- merge(data_stats, global, by = c("dataset", "year", "evap_quant", "environment_volume", "area_sum"), all = T)
data_merged[, evap_mean := environment_volume/area_sum/MM_TO_KM/M2_TO_KM2]

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_evap_quant.rds"))

### table for Zenodo ----
write.table(data_merged, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "interannual_variance_evap_quant.csv"), row.names = F, sep = ",")



## landcover classes ----
data <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                    evap[, .(lon, lat, area, evap_volume, dataset, year)], by = c("lon", "lat"), all.y = TRUE)
data_stats <- data[, .(environment_volume = round(sum(evap_volume, na.rm = T),), area_sum = sum(area)), .(land_cover_short_class, dataset, year)]
global <- data_stats[, .(environment_volume = sum(environment_volume), area_sum = sum(area_sum)), .(dataset, year)]
global[, land_cover_short_class := "Global"]
data_merged <- merge(data_stats, global, by = c("dataset", "year", "land_cover_short_class", "environment_volume", "area_sum"), all = T)
data_merged[, evap_mean := environment_volume/area_sum/MM_TO_KM/M2_TO_KM2]

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_land_cover.rds"))

### table for Zenodo ----
write.table(data_merged, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "interannual_variance_land_cover.csv"), row.names = F, sep = ",")



## biomes ----
data <- merge(evap_mask[, .(lat, lon, biome_short_class)], 
              evap[, .(lon, lat, area, evap_volume, dataset, year)], by = c("lon", "lat"), all.y = TRUE)
data_stats <- data[, .(environment_volume = round(sum(evap_volume, na.rm = T),), area_sum = sum(area)), .(biome_short_class, dataset, year)]
global <- data_stats[, .(environment_volume = sum(environment_volume), area_sum = sum(area_sum)), .(dataset, year)]
global[, biome_short_class := "Global"]
data_merged <- merge(data_stats, global, by = c("dataset", "year", "biome_short_class", "environment_volume", "area_sum"), all = T)
data_merged[, evap_mean := environment_volume/area_sum/MM_TO_KM/M2_TO_KM2]

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_biomes.rds"))

### table for Zenodo ----
write.table(data_merged, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "interannual_variance_biomes.csv"), row.names = F, sep = ",")



## IPCC ----
data <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
              evap[, .(lon, lat, area, evap_volume, dataset, year)], by = c("lon", "lat"), all.y = TRUE)
data_stats <- data[, .(environment_volume = round(sum(evap_volume, na.rm = T),), area_sum = sum(area)), .(IPCC_ref_region, dataset, year)]
global <- data_stats[, .(environment_volume = sum(environment_volume), area_sum = sum(area_sum)), .(dataset, year)]
global[, IPCC_ref_region := "Global"]
data_merged <- merge(data_stats, global, by = c("dataset", "year", "IPCC_ref_region", "environment_volume", "area_sum"), all = T)
data_merged[, evap_mean := environment_volume/area_sum/MM_TO_KM/M2_TO_KM2]

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ipcc.rds"))

### table for Zenodo ----
write.table(data_merged, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "interannual_variance_ipcc.csv"), row.names = F, sep = ",")



## elevation ----
data <- merge(evap_mask[, .(lat, lon, elev_class)], 
              evap[, .(lon, lat, area, evap_volume, dataset, year)], by = c("lon", "lat"), all.y = TRUE)
data_stats <- data[, .(environment_volume = round(sum(evap_volume, na.rm = T),), area_sum = sum(area)), .(elev_class, dataset, year)]
global <- data_stats[, .(environment_volume = sum(environment_volume), area_sum = sum(area_sum)), .(dataset, year)]
global[, elev_class := "Global"]
data_merged <- merge(data_stats, global, by = c("dataset", "year", "elev_class", "environment_volume", "area_sum"), all = T)
data_merged[, evap_mean := environment_volume/area_sum/MM_TO_KM/M2_TO_KM2]

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_elevation.rds"))

### table for Zenodo ----
write.table(data_merged, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "interannual_variance_elevation.csv"), row.names = F, sep = ",")



## Koeppen-Geiger ----
data <- merge(evap_mask[, .(lat, lon, KG_class_1)], 
              evap[, .(lon, lat, area, evap_volume, dataset, year)], by = c("lon", "lat"), all.y = TRUE)
data_stats <- data[, .(environment_volume = round(sum(evap_volume, na.rm = T),), area_sum = sum(area)), .(KG_class_1, dataset, year)]
global <- data_stats[, .(environment_volume = sum(environment_volume), area_sum = sum(area_sum)), .(dataset, year)]
global[, KG_class_1 := "Global"]
data_merged <- merge(data_stats, global, by = c("dataset", "year", "KG_class_1", "environment_volume", "area_sum"), all = T)
data_merged[, evap_mean := environment_volume/area_sum/MM_TO_KM/M2_TO_KM2]

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_KG.rds"))


