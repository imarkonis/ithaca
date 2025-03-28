# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# Increase of uncertainty per class per dataset ----
source('source/changing_prec.R')

## Data ----
### Input Data generated in projects/changing_prec/bootstrap/01_e_ ----
prec_trend_indices <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_e_global_grid_uncertainty_dataset_leftout_bootstrap.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))

## Analysis ----
prec_trend_masks <- merge(prec_trend_indices, prec_mask, all.x = T, by = c("lon", "lat"))
prec_trend_masks[, trend := factor(trend)]
levels(prec_trend_masks$trend)

prec_trend_masks[, trend := factor(trend, levels = c("positive likely","positive probable", "no trend",
                                                     "negative probable",
                                                     "negative likely",
                                                     "uncertain"), ordered = TRUE)]


### Land cover  ----
land_cover_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, land_cover_short_class, 
                                                                        dataset_leftout)]
land_cover_uncertainty <- land_cover_uncertainty[complete.cases(land_cover_uncertainty)]
land_cover_uncertainty[, land_cover_area:= sum(trend_area), .(land_cover_short_class, dataset_leftout)]
land_cover_uncertainty[, land_cover_fraction:= trend_area/land_cover_area, dataset_leftout]
land_cover_uncertainty <- land_cover_uncertainty[!(is.na(land_cover_short_class))]
land_cover_uncertainty <- land_cover_uncertainty[trend == "uncertain"]

### Biome types ----
biome_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, biome_class, 
                                      dataset_leftout)]
biome_uncertainty <- biome_uncertainty[complete.cases(biome_uncertainty)]
biome_uncertainty[, biome_area:= sum(trend_area), .(biome_class, dataset_leftout)]
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
biome_uncertainty <- biome_uncertainty[trend == "uncertain"]

### Elevation class ----
elev_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, elev_class, 
                                     dataset_leftout)]
elev_uncertainty <- elev_uncertainty[complete.cases(elev_uncertainty)]
elev_uncertainty[, elev_area := sum(trend_area), .(elev_class, dataset_leftout)]
elev_uncertainty[, elev_fraction:= trend_area/elev_area]
elev_uncertainty <- elev_uncertainty[trend == "uncertain"]

### IPCC reference regions ----
ipcc_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, IPCC_ref_region, 
                                     dataset_leftout)]
ipcc_uncertainty <- ipcc_uncertainty[complete.cases(ipcc_uncertainty)]
ipcc_uncertainty[, ipcc_area:= sum(trend_area), .(IPCC_ref_region, dataset_leftout)]
ipcc_uncertainty[, ipcc_fraction:= trend_area/ipcc_area]
ipcc_uncertainty <- ipcc_uncertainty[trend == "uncertain"]

### Koeppen-Geiger ----
KG_3_uncertainty <- prec_trend_masks[,.(trend_area = sum(area)),.(trend, KG_class_3, 
                                     dataset_leftout)]
KG_3_uncertainty <- KG_3_uncertainty[complete.cases(KG_3_uncertainty)]
KG_3_uncertainty[, KG_3_area := sum(trend_area), .(KG_class_3, dataset_leftout)]
KG_3_uncertainty[, KG_3_fraction := trend_area/KG_3_area]
KG_3_uncertainty <- KG_3_uncertainty[trend == "uncertain"]

## Read uncertainty data from previous analysis from changing_prec/bootstrap/02_b ----

land_cover <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_land_cover_uncertainty_N12_bootstrap.rds"))
land_cover <- land_cover[trend == "uncertain"]
biome <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_biomes_uncertainty_N12_bootstrap.rds"))
biome <- biome[trend == "uncertain"]
elev <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_elevation_uncertainty_N12_bootstrap.rds"))
elev <- elev[trend == "uncertain"]
IPCC <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_ipcc_uncertainty_N12_bootstrap.rds"))
IPCC <- IPCC[trend == "uncertain"]
KG_3 <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_KG_3_uncertainty_N12_bootstrap.rds"))
KG_3 <- KG_3[trend == "uncertain"]

## Merge data and calculate difference and ratios ----
land_cover_uncertainty <- merge(land_cover_uncertainty, land_cover, by = c("trend", "land_cover_short_class"), all = T)
land_cover_uncertainty[, diff_area_fraction := land_cover_fraction.x-land_cover_fraction.y]
land_cover_uncertainty[, ratio_area_fraction := land_cover_fraction.y/land_cover_fraction.x]

biome_uncertainty <- merge(biome_uncertainty, biome, by = c("trend", "biome_class"), all = T)
biome_uncertainty[, diff_area_fraction := biome_fraction.x-biome_fraction.y]
biome_uncertainty[, ratio_area_fraction := biome_fraction.y/biome_fraction.x]

elev_uncertainty <- merge(elev_uncertainty, elev, by = c("trend", "elev_class"), all = T)
elev_uncertainty[, diff_area_fraction := elev_fraction.x-elev_fraction.y]
elev_uncertainty[, ratio_area_fraction := elev_fraction.y/elev_fraction.x]

ipcc_uncertainty <- merge(ipcc_uncertainty, IPCC, by = c("trend", "IPCC_ref_region"), all = T)
ipcc_uncertainty[, diff_area_fraction := ipcc_fraction.x-ipcc_fraction.y]
ipcc_uncertainty[, ratio_area_fraction := ipcc_fraction.y/ipcc_fraction.x]

KG_3_uncertainty <- merge(KG_3_uncertainty, KG_3, by = c("trend", "KG_class_3"), all = T)
KG_3_uncertainty[, diff_area_fraction := KG_3_fraction.x-KG_3_fraction.y]
KG_3_uncertainty[, ratio_area_fraction := KG_3_fraction.y/KG_3_fraction.x]

## save data ----
saveRDS(land_cover_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_d_land_cover_uncertainty_dataset_leftout_bootstrap.rds"))
saveRDS(biome_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_d_biome_uncertainty_dataset_leftout_bootstrap.rds"))
saveRDS(elev_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_d_elev_uncertainty_dataset_leftout_bootstrap.rds"))
saveRDS(ipcc_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_d_ipcc_uncertainty_dataset_leftout_bootstrap.rds"))
saveRDS(KG_3_uncertainty, paste0(PATH_SAVE_CHANGING_PREC, "02_d_KG_3_uncertainty_dataset_leftout_bootstrap.rds"))

