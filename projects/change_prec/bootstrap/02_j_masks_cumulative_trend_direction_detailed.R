# Cumulative significant trend direction per mask item  ----
source('source/changing_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data ----
prec_mask <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))

### Input Data generated in projects/changing_prec/bootstrap/01_c
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  

## Analysis ----
prec_trend_masks <- merge(prec_trend, prec_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(prec_trend[, .(lon, lat)]) %>% grid_area() # m2
prec_trend_masks <- grid_cell_area[prec_trend_masks, on = .(lon, lat)]

prec_trend_masks[slope >= 0 , trend_direction_detailed := "positive p > 0.1"]
prec_trend_masks[slope > 0 & p <= 0.1 , trend_direction_detailed := "positive p <= 0.1"]
prec_trend_masks[slope > 0 & p < 0.05 , trend_direction_detailed := "positive p < 0.05"]
prec_trend_masks[slope < 0  , trend_direction_detailed := "negative p > 0.1"]
prec_trend_masks[slope < 0 & p <= 0.1 , trend_direction_detailed := "negative p <= 0.1"]
prec_trend_masks[slope < 0 & p < 0.05 , trend_direction_detailed := "negative p < 0.05"]
prec_trend_masks[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                 level = c("positive p < 0.05", "positive p <= 0.1", "positive p > 0.1",
                                                           "negative p > 0.1", "negative p <= 0.1", "negative p < 0.05"), ordered = T),]

### Biome types ----
biome_trends <- prec_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, biome_class, dataset)]
biome_trends <- biome_trends[complete.cases(biome_trends)]
biome_trends[, biome_area:= sum(trend_area), .(biome_class, dataset)]
biome_trends[, biome_fraction:= trend_area/biome_area]
biome_trends[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_trends[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_trends[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_trends[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_trends[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_trends[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_trends[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_trends[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_trends[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_trends[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_trends[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_trends[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_trends[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_trends[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_trends[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_trends[, biome_short_class := factor(biome_short_class)]
biome_trends <- biome_trends[complete.cases(biome_trends)]


### ipcc ref regions  ----
ipcc_trends <- prec_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, IPCC_ref_region, dataset)]
ipcc_trends <- ipcc_trends[complete.cases(ipcc_trends)]
ipcc_trends[, ipcc_area:= sum(trend_area), .(IPCC_ref_region, dataset)]
ipcc_trends[, ipcc_fraction:= trend_area/ipcc_area]


### land use ----
land_trends <- prec_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, land_cover_short_class, dataset)]
land_trends <- land_trends[complete.cases(land_cover_short_class)]
land_trends[, land_area:= sum(trend_area), .(land_cover_short_class, dataset)]
land_trends[, land_fraction:= trend_area/land_area]


### elevation ----
elev_trends <- prec_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, elev_class, dataset)]
elev_trends <- elev_trends[complete.cases(elev_class)]
elev_trends[, elev_area:= sum(trend_area), .(elev_class, dataset)]
elev_trends[, elev_fraction:= trend_area/elev_area]


### KG 3 ----
KG_class_3_trends <- prec_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, KG_class_3, dataset)]
KG_class_3_trends <- KG_class_3_trends[complete.cases(KG_class_3_trends)]
KG_class_3_trends[, KG_class_3_area:= sum(trend_area), .(KG_class_3, dataset)]
KG_class_3_trends[, KG_class_3_fraction:= trend_area/KG_class_3_area]


## Save data ----
saveRDS(biome_trends, paste0(PATH_SAVE_CHANGING_PREC, "02_j_biome_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(ipcc_trends, paste0(PATH_SAVE_CHANGING_PREC, "02_j_ipcc_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(land_trends, paste0(PATH_SAVE_CHANGING_PREC, "02_j_land_cover_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(elev_trends, paste0(PATH_SAVE_CHANGING_PREC, "02_j_elevation_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(KG_class_3_trends, paste0(PATH_SAVE_CHANGING_PREC, "02_j_KG_3_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))

