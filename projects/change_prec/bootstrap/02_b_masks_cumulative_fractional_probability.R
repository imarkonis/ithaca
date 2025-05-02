# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# Cumulative fraction based on probability index  ----
source('source/changing_prec.R')
source('source/graphics.R')

## Data ----
### Input Data generated in projects/changing_prec/bootstrap/01_d
prec_trend_indices <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_d_global_grid_slope_indices_opp_allowed_bootstrap.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))

## Analysis all grids----
prec_trend_masks <- merge(prec_trend_indices, prec_mask, all.x = T, by = c("lon", "lat"))
prec_trend_masks[, trend := factor(trend, levels = c("positive likely","positive probable", "no trend",
                                                 "negative probable",
                                                 "negative likely",
                                                 "uncertain"), ordered = TRUE)]

### Land cover  ----
land_cover_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, land_cover_short_class)]
land_cover_uncertainty <- land_cover_uncertainty[complete.cases(land_cover_uncertainty)]
land_cover_uncertainty[, land_cover_area:= sum(trend_area), .(land_cover_short_class)]
land_cover_uncertainty[, land_cover_fraction:= trend_area/land_cover_area]
land_cover_uncertainty <- land_cover_uncertainty[!(is.na(land_cover_short_class))]

### Biome types ----
biome_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, biome_class)]
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

elev_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, elev_class)]
elev_uncertainty <- elev_uncertainty[complete.cases(elev_uncertainty)]
elev_uncertainty[, elev_area := sum(trend_area), .(elev_class)]
elev_uncertainty[, elev_fraction:= trend_area/elev_area]

### IPCC reference regions ----
ipcc_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, IPCC_ref_region)]
ipcc_uncertainty <- ipcc_uncertainty[complete.cases(ipcc_uncertainty)]
ipcc_uncertainty[, ipcc_area:= sum(trend_area), .(IPCC_ref_region)]
ipcc_uncertainty[, ipcc_fraction:= trend_area/ipcc_area]

### Koeppen-Geiger ----
KG_3_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, KG_class_3)]
KG_3_uncertainty <- KG_3_uncertainty[complete.cases(KG_3_uncertainty)]
KG_3_uncertainty[, KG_3_area := sum(trend_area), .(KG_class_3)]
KG_3_uncertainty[, KG_3_fraction := trend_area/KG_3_area]

## save data ----
saveRDS(land_cover_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_land_cover_uncertainty_bootstrap.rds"))
saveRDS(biome_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_biomes_uncertainty_bootstrap.rds"))
saveRDS(elev_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_elevation_uncertainty_bootstrap.rds"))
saveRDS(ipcc_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_ipcc_uncertainty_bootstrap.rds"))
saveRDS(KG_3_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_KG_3_uncertainty_bootstrap.rds"))

## Analysis grids with 14 datasets ----
prec_trend_masks <- merge(prec_trend_indices, prec_mask, all.x = T, by = c("lon", "lat"))
prec_trend_masks[, trend := factor(trend, levels = c("positive likely","positive probable", "no trend",
                                                     "negative probable",
                                                     "negative likely",
                                                     "uncertain"), ordered = TRUE)]
prec_trend_masks <- prec_trend_masks[count >= 12, ]

### Land cover  ----
land_cover_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, land_cover_short_class)]
land_cover_uncertainty <- land_cover_uncertainty[complete.cases(land_cover_uncertainty)]
land_cover_uncertainty[, land_cover_area:= sum(trend_area), .(land_cover_short_class)]
land_cover_uncertainty[, land_cover_fraction:= trend_area/land_cover_area]
land_cover_uncertainty <- land_cover_uncertainty[!(is.na(land_cover_short_class))]

### Biome types ----
biome_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, biome_class)]
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

elev_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, elev_class)]
elev_uncertainty <- elev_uncertainty[complete.cases(elev_uncertainty)]
elev_uncertainty[, elev_area := sum(trend_area), .(elev_class)]
elev_uncertainty[, elev_fraction:= trend_area/elev_area]

### IPCC reference regions ----
ipcc_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, IPCC_ref_region)]
ipcc_uncertainty <- ipcc_uncertainty[complete.cases(ipcc_uncertainty)]
ipcc_uncertainty[, ipcc_area:= sum(trend_area), .(IPCC_ref_region)]
ipcc_uncertainty[, ipcc_fraction:= trend_area/ipcc_area]

### Koeppen-Geiger ----
KG_3_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, KG_class_3)]
KG_3_uncertainty <- KG_3_uncertainty[complete.cases(KG_3_uncertainty)]
KG_3_uncertainty[, KG_3_area := sum(trend_area), .(KG_class_3)]
KG_3_uncertainty[, KG_3_fraction := trend_area/KG_3_area]

## save data ----
saveRDS(land_cover_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_land_cover_uncertainty_N12_bootstrap.rds"))
saveRDS(biome_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_biomes_uncertainty_N12_bootstrap.rds"))
saveRDS(elev_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_elevation_uncertainty_N12_bootstrap.rds"))
saveRDS(ipcc_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_ipcc_uncertainty_N12_bootstrap.rds"))
saveRDS(KG_3_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_b_KG_3_uncertainty_N12_bootstrap.rds"))
