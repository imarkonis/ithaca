# Trend magnitude per class ----
source('source/evap_trend.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/partition_evap/04
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

### Input Data generated in projects/evap_trend/01_a
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope.rds"))  

evap_trend[dataset %in% EVAP_DATASETS_REANAL, dataset_type := 'Reanalysis'
][dataset %in% EVAP_DATASETS_REMOTE, dataset_type := 'Remote sensing'
][dataset %in% EVAP_DATASETS_HYDROL, dataset_type := 'Hydrologic model'
][dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := 'Ensemble']

evap_trend <- evap_trend[complete.cases(evap_trend)]

evap_trend <- merge(evap_trend, evap_mask, by = c("lon", "lat"))

## Analysis ----
grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend <- grid_cell_area[evap_trend, on = .(lon, lat)]

evap_trend[, dataset := factor(dataset, 
                               levels = c("bess", "etmonitor", "gleam", "mod16a",
                                          "camele", "etsynthesis", 
                                          "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate",
                                          "era5-land", "jra55","merra2"),
                               labels = c("bess", "etmonitor", "gleam", "mod16a",
                                          "camele", "etsynthesis", 
                                          "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate",
                                          "era5-land", "jra55","merra2")
                               , ordered = TRUE)]
evap_trend[, evap_trend_volume := area * M2_TO_KM2 * theil_sen_slope * MM_TO_KM]
evap_trend[, evap_volume := area * M2_TO_KM2 * mean_evap * MM_TO_KM]

### Land cover ----
land_cover_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area), 
                                          evap_volume = sum(evap_volume, na.rm = T)), 
                                      .(dataset, dataset_type, land_cover_short_class)]

land_cover_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]
land_cover_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
land_cover_class_global[, evap_trend_percent := evap_trend_mean/evap_mean*100]

### Biome types ----

biome_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area),
                                     evap_volume = sum(evap_volume, na.rm = T)),  
                                 .(dataset, dataset_type, biome_class)]

biome_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global[, evap_trend_percent := evap_trend_mean/evap_mean*100]

biome_class_global[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_class_global[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_class_global[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_class_global[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_class_global[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_class_global[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_class_global[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_class_global[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_class_global[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_class_global[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_class_global[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_class_global[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_class_global[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_class_global[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_class_global[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_class_global[, biome_short_class := factor(biome_short_class)]

### elevation ----
elev_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area), 
                                    evap_volume = sum(evap_volume, na.rm = T)),
                                .(dataset, dataset_type, elev_class)]

elev_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]
elev_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
elev_class_global[, evap_trend_percent := evap_trend_mean/evap_mean*100]

### IPCC reference regions ----
ipcc_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area), 
                                    evap_volume = sum(evap_volume, na.rm = T)), 
                                .(dataset, dataset_type, IPCC_ref_region)]

ipcc_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]
ipcc_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
ipcc_class_global[, evap_trend_percent := evap_trend_mean/evap_mean*100]
ipcc_class_global[grepl("O", as.character(IPCC_ref_region)) == TRUE, ocean := "yes"]
ipcc_class_global[IPCC_ref_region %in% c("BOB", "ARS"), ocean := "yes"]

### Koeppen-Geiger classes ----
KG_class_3_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area), 
                                    evap_volume = sum(evap_volume, na.rm = T)), 
                                .(dataset, dataset_type, KG_class_3)]

KG_class_3_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]
KG_class_3_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
KG_class_3_global[, evap_trend_percent := evap_trend_mean/evap_mean*100]

## save data ----

saveRDS(land_cover_class_global, paste0(PATH_SAVE_EVAP_TREND, "land_cover_mean_slope.rds"))
saveRDS(biome_class_global, paste0(PATH_SAVE_EVAP_TREND, "biomes_mean_slope.rds"))
saveRDS(elev_class_global, paste0(PATH_SAVE_EVAP_TREND, "elevation_mean_slope.rds"))
saveRDS(ipcc_class_global, paste0(PATH_SAVE_EVAP_TREND, "ipcc_mean_slope.rds"))
saveRDS(KG_class_3_global, paste0(PATH_SAVE_EVAP_TREND, "KG_3_mean_slope.rds"))
