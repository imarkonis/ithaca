# Division of data into landcover, biome, elevation, quantile classes and ipcc reference regions  ----
source('source/changing_prec.R')

## Data 
prec_datasets <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "prec_datasets.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "prec_mean_volume_grid.rds"))

## Analysis  ----
prec_datasets_volume <- merge(prec_datasets[dataset %in% PREC_GLOBAL_DATASETS, .(lon, lat, year, dataset, prec)], 
                              prec_grid[, .(lon, lat, area)], 
                              by = c("lon", "lat"), all = TRUE)
prec_datasets_volume[, prec_volume := area * M2_TO_KM2 * prec * MM_TO_KM]
prec_datasets_volume[, dataset_count := .N, .(lat, lon, year)]
prec_datasets_volume <- prec_datasets_volume[dataset_count >= 10]

### Land cover class  ----
land_cover_class <- merge(prec_mask[, .(lat, lon, land_cover_short_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                            .(dataset, land_cover_short_class, year)]
land_cover_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]

### Biomes ----
biome_class <- merge(prec_mask[, .(lat, lon, biome_class)], 
                     prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
                     by = c("lon", "lat"))
biome_class_global <- biome_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                  .(dataset, biome_class, year)]
biome_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global <- biome_class_global[complete.cases(biome_class_global)]

### Elevation ----
elev_class <- merge(prec_mask[, .(lat, lon, elev_class)], 
                    prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
                    by = c("lon", "lat"))
elev_class_global <- elev_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                .(dataset, elev_class, year)]
elev_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]

### Quantiles ----
# prec_class <- merge(prec_mask[, .(lat, lon, prec_quant)], 
#                     prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
#                     by = c("lon", "lat"))
# prec_class_global <- prec_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
#                                 .(dataset, prec_quant, year)]
# prec_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]

### IPCC reference region ----
ipcc_class <- merge(prec_mask[, .(lat, lon, IPCC_ref_region)], 
                    prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
                    by = c("lon", "lat"))
ipcc_class_global <- ipcc_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                .(dataset, IPCC_ref_region, year)]
ipcc_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]


### KG level 3 classes
KG_class_3_class <- merge(prec_mask[, .(lat, lon, KG_class_3)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_3_class_global <- KG_class_3_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                            .(dataset, KG_class_3, year)]
KG_class_3_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]



### KG level 2 classes
KG_class_2_class <- merge(prec_mask[, .(lat, lon, KG_class_2)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_2_class_global <- KG_class_2_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                            .(dataset, KG_class_2, year)]
KG_class_2_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]



### KG level 1 classes
KG_class_1_class <- merge(prec_mask[, .(lat, lon, KG_class_1)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_1_class_global <- KG_class_1_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                            .(dataset, KG_class_1, year)]
KG_class_1_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]


## Save data ----
saveRDS(land_cover_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_land_cover_class_mean.rds"))
saveRDS(biome_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_biome_class_mean.rds"))
saveRDS(elev_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_elev_class_mean.rds"))
#saveRDS(prec_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_prec_class_mean.rds"))
saveRDS(ipcc_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_ipcc_class_mean.rds"))
saveRDS(KG_class_3_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_KG_3_class_mean.rds"))
saveRDS(KG_class_2_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_KG_2_class_mean.rds"))
saveRDS(KG_class_1_class_global, paste0(PATH_SAVE_CHANGING_PREC, "02_k_KG_1_class_mean.rds"))
