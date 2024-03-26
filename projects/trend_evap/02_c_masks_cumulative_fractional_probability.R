# Cumulative fraction based on probability index  ----
source('source/evap_trend.R')
source('source/graphics.R')

## Data ----
### Input Data generated in projects/evap_trend/01_d
evap_trend_indices <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))
### Input Data generated in projects/partition_evap/04
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Analysis all grids----
evap_trend_masks <- merge(evap_trend_indices, evap_mask, all.x = T, by = c("lon", "lat"))
evap_trend_masks[, trend := factor(trend, levels = c("positive likely","positive probable", "no trend",
                                                 "negative probable",
                                                 "negative likely",
                                                 "uncertain"), ordered = TRUE)]

### Land cover  ----
land_cover_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, land_cover_short_class)]
land_cover_uncertainty <- land_cover_uncertainty[complete.cases(land_cover_uncertainty)]
land_cover_uncertainty[, land_cover_area:= sum(trend_area), .(land_cover_short_class)]
land_cover_uncertainty[, land_cover_fraction:= trend_area/land_cover_area]
land_cover_uncertainty <- land_cover_uncertainty[!(is.na(land_cover_short_class))]

### Biome types ----
biome_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, biome_class)]
biome_uncertainty <- biome_uncertainty[complete.cases(biome_uncertainty)]
biome_uncertainty[, biome_area:= sum(trend_area), .(biome_class)]
biome_uncertainty[, biome_fraction:= trend_area/biome_area]
biome_uncertainty <- biome_uncertainty[!(biome_class == "N/A")]

biome_uncertainty[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_uncertainty[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_uncertainty[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_uncertainty[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_uncertainty[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_uncertainty[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_uncertainty[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_uncertainty[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_uncertainty[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_uncertainty[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_uncertainty[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_uncertainty[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_uncertainty[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_uncertainty[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_uncertainty[, biome_short_class := factor(biome_short_class)]

### Elevation class ----

elev_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, elev_class)]
elev_uncertainty <- elev_uncertainty[complete.cases(elev_uncertainty)]
elev_uncertainty[, elev_area := sum(trend_area), .(elev_class)]
elev_uncertainty[, elev_fraction:= trend_area/elev_area]

### IPCC reference regions ----
ipcc_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, IPCC_ref_region)]
ipcc_uncertainty <- ipcc_uncertainty[complete.cases(ipcc_uncertainty)]
ipcc_uncertainty[, ipcc_area:= sum(trend_area), .(IPCC_ref_region)]
ipcc_uncertainty[, ipcc_fraction:= trend_area/ipcc_area]

### Koeppen-Geiger ----
KG_3_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, KG_class_3)]
KG_3_uncertainty <- KG_3_uncertainty[complete.cases(KG_3_uncertainty)]
KG_3_uncertainty[, KG_3_area := sum(trend_area), .(KG_class_3)]
KG_3_uncertainty[, KG_3_fraction := trend_area/KG_3_area]

## save data ----
saveRDS(land_cover_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "land_cover_uncertainty.rds"))
saveRDS(biome_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "biomes_uncertainty.rds"))
saveRDS(elev_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "elevation_uncertainty.rds"))
saveRDS(ipcc_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "ipcc_uncertainty.rds"))
saveRDS(KG_3_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "KG_3_uncertainty.rds"))

## Analysis grids with 14 datasets ----
evap_trend_masks <- merge(evap_trend_indices, evap_mask, all.x = T, by = c("lon", "lat"))
evap_trend_masks[, trend := factor(trend, levels = c("positive likely","positive probable", "no trend",
                                                     "negative probable",
                                                     "negative likely",
                                                     "uncertain"), ordered = TRUE)]
evap_trend_masks <- evap_trend_masks[count >= 14, ]

### Land cover  ----
land_cover_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, land_cover_short_class)]
land_cover_uncertainty <- land_cover_uncertainty[complete.cases(land_cover_uncertainty)]
land_cover_uncertainty[, land_cover_area:= sum(trend_area), .(land_cover_short_class)]
land_cover_uncertainty[, land_cover_fraction:= trend_area/land_cover_area]
land_cover_uncertainty <- land_cover_uncertainty[!(is.na(land_cover_short_class))]

### Biome types ----
biome_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, biome_class)]
biome_uncertainty <- biome_uncertainty[complete.cases(biome_uncertainty)]
biome_uncertainty[, biome_area:= sum(trend_area), .(biome_class)]
biome_uncertainty[, biome_fraction:= trend_area/biome_area]
biome_uncertainty <- biome_uncertainty[!(biome_class == "N/A")]

biome_uncertainty[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_uncertainty[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_uncertainty[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_uncertainty[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_uncertainty[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_uncertainty[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_uncertainty[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_uncertainty[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_uncertainty[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_uncertainty[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_uncertainty[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_uncertainty[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_uncertainty[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_uncertainty[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_uncertainty[, biome_short_class := factor(biome_short_class)]

### Elevation class ----

elev_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, elev_class)]
elev_uncertainty <- elev_uncertainty[complete.cases(elev_uncertainty)]
elev_uncertainty[, elev_area := sum(trend_area), .(elev_class)]
elev_uncertainty[, elev_fraction:= trend_area/elev_area]

### IPCC reference regions ----
ipcc_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, IPCC_ref_region)]
ipcc_uncertainty <- ipcc_uncertainty[complete.cases(ipcc_uncertainty)]
ipcc_uncertainty[, ipcc_area:= sum(trend_area), .(IPCC_ref_region)]
ipcc_uncertainty[, ipcc_fraction:= trend_area/ipcc_area]

### Koeppen-Geiger ----
KG_3_uncertainty <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, KG_class_3)]
KG_3_uncertainty <- KG_3_uncertainty[complete.cases(KG_3_uncertainty)]
KG_3_uncertainty[, KG_3_area := sum(trend_area), .(KG_class_3)]
KG_3_uncertainty[, KG_3_fraction := trend_area/KG_3_area]

## save data ----
saveRDS(land_cover_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "land_cover_uncertainty_N14.rds"))
saveRDS(biome_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "biomes_uncertainty_N14.rds"))
saveRDS(elev_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "elevation_uncertainty_N14.rds"))
saveRDS(ipcc_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "ipcc_uncertainty_N14.rds"))
saveRDS(KG_3_uncertainty, paste0(PATH_SAVE_EVAP_TREND, "KG_3_uncertainty_N14.rds"))
