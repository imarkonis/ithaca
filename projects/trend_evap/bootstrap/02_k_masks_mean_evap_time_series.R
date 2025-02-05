# Division of data into landcover, biome, elevation, quantile classes and ipcc reference regions  ----
source('source/evap_trend.R')

## Data 
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))
evap_datasets[, dataset_count := .N, .(lat, lon, year)]
evap_datasets <- evap_datasets[dataset_count >= 14]

### Input Data generated in projects/partition_evap/04
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

### Land cover class  ----
land_cover_class <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                          evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, land_cover_short_class, year)]
land_cover_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

land_cover_class_ensemble <- land_cover_class_global[, .(evap_mean = mean(evap_mean)), .(land_cover_short_class, year)]

### Biomes ----
biome_class <- merge(evap_mask[, .(lat, lon, biome_class)], 
                     evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                     by = c("lon", "lat"))
biome_class_global <- biome_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                  .(dataset, biome_class, year)]
biome_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global <- biome_class_global[complete.cases(biome_class_global)]

biome_class_ensemble <- biome_class_global[, .(evap_mean = mean(evap_mean)), .(biome_class, year)]

### Elevation ----
elev_class <- merge(evap_mask[, .(lat, lon, elev_class)], 
                    evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                    by = c("lon", "lat"))
elev_class_global <- elev_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, elev_class, year)]
elev_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]


elev_class_ensemble <- elev_class_global[, .(evap_mean = mean(evap_mean)), .(elev_class, year)]

### Quantiles ----
evap_class <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                    evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                    by = c("lon", "lat"))
evap_class_global <- evap_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, evap_quant, year)]
evap_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

evap_class_ensemble <- evap_class_global[, .(evap_mean = mean(evap_mean)), .(evap_quant, year)]

### IPCC reference region ----
ipcc_class <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                    by = c("lon", "lat"))
ipcc_class_global <- ipcc_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, IPCC_ref_region, year)]
ipcc_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
ipcc_class_ensemble <- ipcc_class_global[, .(evap_mean = mean(evap_mean)), .(IPCC_ref_region, year)]


### KG level 3 classes
KG_class_3_class <- merge(evap_mask[, .(lat, lon, KG_class_3)], 
                          evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_3_class_global <- KG_class_3_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, KG_class_3, year)]
KG_class_3_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

KG_class_3_class_ensemble <- KG_class_3_class_global[, .(evap_mean = mean(evap_mean)), .(KG_class_3, year)]


### KG level 2 classes
KG_class_2_class <- merge(evap_mask[, .(lat, lon, KG_class_2)], 
                          evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_2_class_global <- KG_class_2_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, KG_class_2, year)]
KG_class_2_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
KG_class_2_class_ensemble <- KG_class_2_class_global[, .(evap_mean = mean(evap_mean)), .(KG_class_2, year)]



### KG level 1 classes
KG_class_1_class <- merge(evap_mask[, .(lat, lon, KG_class_1)], 
                          evap_datasets[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_1_class_global <- KG_class_1_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, KG_class_1, year)]
KG_class_1_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

KG_class_1_class_ensemble <- KG_class_1_class_global[, .(evap_mean = mean(evap_mean)), .(KG_class_1, year)]

## Save data ----
saveRDS(land_cover_class_global, paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_mean.rds"))
saveRDS(biome_class_global, paste0(PATH_SAVE_EVAP_TREND, "biome_class_mean.rds"))
saveRDS(elev_class_global, paste0(PATH_SAVE_EVAP_TREND, "elev_class_mean.rds"))
saveRDS(evap_class_global, paste0(PATH_SAVE_EVAP_TREND, "evap_class_mean.rds"))
saveRDS(ipcc_class_global, paste0(PATH_SAVE_EVAP_TREND, "ipcc_class_mean.rds"))
saveRDS(KG_class_3_class_global, paste0(PATH_SAVE_EVAP_TREND, "KG_3_class_mean.rds"))
saveRDS(KG_class_2_class_global, paste0(PATH_SAVE_EVAP_TREND, "KG_2_class_mean.rds"))
saveRDS(KG_class_1_class_global, paste0(PATH_SAVE_EVAP_TREND, "KG_1_class_mean.rds"))

saveRDS(land_cover_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_ensemble_mean.rds"))
saveRDS(biome_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "biome_class_ensemble_mean.rds"))
saveRDS(elev_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "elev_class_ensemble_mean.rds"))
saveRDS(evap_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "evap_class_ensemble_mean.rds"))
saveRDS(ipcc_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "ipcc_class_ensemble_mean.rds"))
saveRDS(KG_class_3_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "KG_3_class_ensemble_mean.rds"))
saveRDS(KG_class_2_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "KG_2_class_ensemble_mean.rds"))
saveRDS(KG_class_1_class_ensemble, paste0(PATH_SAVE_EVAP_TREND, "KG_1_class_ensemble_mean.rds"))
