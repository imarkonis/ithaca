# Add categorical classes to each variable 

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/mask_paths.R')

## Packages
library("gtools")

## Data 
prec_era5 <- brick(paste0(PATH_SAVE_PARTITION_PREC_RAW, "era5_tp_mm_land_200001_201912_025_monthly.nc"))
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))

## Masks
### Uncertainty - Dataset agreement
prec_stats[, quant_ens_cv := ordered(quantcut(ens_mean_cv, 5), 
                                         labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_stats[, rel_dataset_agreement := ordered(quantcut(std_quant_range, 5), 
                                                  labels = c('high', 'above average', 'average', 'below average', 'low'))]

prec_stats[, abs_dataset_agreement := ordered(1, labels = "very high")]
prec_stats[std_quant_range > 0.1 & std_quant_range < 0.2, abs_dataset_agreement := ordered(2, labels = "high")]
prec_stats[std_quant_range > 0.2 & std_quant_range < 0.4, abs_dataset_agreement := ordered(3, labels = "above average")]
prec_stats[std_quant_range > 0.4 & std_quant_range < 0.6, abs_dataset_agreement := ordered(4, labels = "average")]
prec_stats[std_quant_range > 0.6 & std_quant_range < 0.8, abs_dataset_agreement := ordered(5, labels = "below average")]
prec_stats[std_quant_range > 0.8 & std_quant_range < 1, abs_dataset_agreement := ordered(6, labels = "low")]
prec_stats[std_quant_range > 1, abs_dataset_agreement := ordered(7, labels = "very low")]

prec_stats[, outlier_dataset := FALSE]
prec_stats[ens_mean_mean / ens_mean_median > 1.2 | ens_mean_mean / ens_mean_median < 0.8, outlier_dataset := TRUE]

## Masks
### Precipitation
prec_stats[, prec_class := ordered(quantcut(ens_mean_mean, 5), labels = c('low', 'below average', 'average', 'above average', 'high'))]
prec_stats[, prec_quant := ordered(quantcut(ens_mean_mean, 10), labels = c('0-0.1', '0.1-0.2', '0.2-0.3', '0.3-0.4', '0.4-0.5',
                                                                           '0.5-0.6', '0.6-0.7', '0.7-0.8', '0.8-0.9', '0.9-1'))]

### Koppen-Geiger
fname_shape <- list.files(path = PATH_MASKS_KOPPEN, full.names = TRUE, pattern = "climate_beck_level3.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class_1')
shape_mask_df$KG_class_1 <- factor(shape_mask_df$KG_class_1)
prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))

fname_shape <- list.files(path = PATH_MASKS_KOPPEN, full.names = TRUE, pattern = "climate_beck_level2.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class_2')
shape_mask_df$KG_class_2 <- factor(shape_mask_df$KG_class_2)
prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))

fname_shape <- list.files(path = PATH_MASKS_KOPPEN, full.names = TRUE, pattern = "climate_beck_level1.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class_3')
shape_mask_df$KG_class_3 <- factor(shape_mask_df$KG_class_3)
prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))

### Elevation
fname <- list.files(path = PATH_MASKS_ELEVATION, full.names = TRUE, pattern = "mask_orography_groups_025.nc")
shape_mask <- raster(paste0(fname[1]))
shape_mask <- ratify(shape_mask)

mask_fname <- list.files(path = PATH_MASKS_ELEVATION, pattern = "*groups_025_classes.txt" , full.names = T)
mask_raster_classes <- read.table(paste(mask_fname[1]))
mask_raster_classes <- as.data.frame(sapply(mask_raster_classes,
                         mapvalues, from = c("(-Inf,100]", "(800,1.5e+03]", "(1.5e+03,3e+03]", "(3e+03, Inf]"), 
                         to = c("(0,100]", "(800,1500]", "(1500,3000]", "(3000,Inf]")))
levels(shape_mask)[[1]] <- mask_raster_classes

shape_mask_df <- shape_mask %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'elev_class')
shape_mask_df$elev_class <- factor(shape_mask_df$elev_class, 
                                   levels = c("(0,100]", "(100,400]", "(400,800]", "(800,1500]", "(1500,3000]", "(3000,Inf]"), 
                                   labels = c("0-100", "100-400", "400-800", "800-1500", "1500-3000", "3000+"), 
                                   ordered =TRUE)
prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))

### Land use
fname <- list.files(path = PATH_MASKS_LAND_USE, full.names = TRUE, pattern = "mask_landcover_modis_025.nc")
shape_mask <- raster(paste0(fname[1]))
shape_mask <- ratify(shape_mask)

mask_fname <- list.files(path = PATH_MASKS_LAND_USE, pattern = "*modis_025_classes.txt" , full.names = T)
mask_raster_classes <- read.table(paste(mask_fname[1]))
levels(shape_mask) <- mask_raster_classes

shape_mask_df <- shape_mask %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'land_class')
shape_mask_df$land_class <- factor(shape_mask_df$land_class)
prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))

### Biomes
fname_shape <- list.files(path = PATH_MASKS_BIOME, full.names = TRUE, pattern = "mask_biomes_dinerstein.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]]) 
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'layer_BIOME_NAME'))
colnames(shape_mask_df) <- c('lon', 'lat', 'biome_class')
shape_mask_df$biome_class <- factor(shape_mask_df$biome_class)
prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))

prec_masks <- prec_stats[, .(lon, lat, prec_mean = ens_mean_mean, rel_dataset_agreement, 
                             abs_dataset_agreement, outlier_dataset, prec_class, 
                             KG_class_1,  KG_class_2,  KG_class_3, elev_class, land_use_class = land_class, biome_class)]

### Extra masks
prec_masks[grepl("Shrub", land_use_class) == TRUE, land_use_short_class := "Shrublands"]
prec_masks[grepl("Forest", land_use_class) == TRUE, land_use_short_class := "Forests"]
prec_masks[grepl("Savannas", land_use_class) == TRUE, land_use_short_class := "Savannas"]
prec_masks[grepl("Cropland", land_use_class) == TRUE, land_use_short_class := "Croplands"]
prec_masks[grepl("Grasslands", land_use_class) == TRUE, land_use_short_class := "Grasslands"]
prec_masks[grepl("Urban", land_use_class) == TRUE, land_use_short_class := "Other"]
prec_masks[grepl("Unclassified", land_use_class) == TRUE, land_use_short_class := "Other"]
prec_masks[grepl("Ice", land_use_class) == TRUE, land_use_short_class := "Snow/Ice"]
prec_masks[grepl("Water", land_use_class) == TRUE, land_use_short_class := "Water"]
prec_masks[grepl("Wetlands", land_use_class) == TRUE, land_use_short_class := "Water"]
prec_masks[grepl("Barren", land_use_class) == TRUE, land_use_short_class := "Barren"]
prec_masks[, land_use_short_class := factor(land_use_short_class)    ]

prec_masks[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
prec_masks[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
prec_masks[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Forests"]
prec_masks[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Forests"]
prec_masks[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Forests"]
prec_masks[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Forests"]
prec_masks[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. Forests"]
prec_masks[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
prec_masks[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
prec_masks[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
prec_masks[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
prec_masks[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Flooded"]
prec_masks[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
prec_masks[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
prec_masks[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
prec_masks[, biome_short_class := factor(biome_short_class)]

prec_mask[KG_class_1 == 'A', KG_class_1_name := 'Tropical']
prec_mask[KG_class_1 == 'B', KG_class_1_name := 'Dry']
prec_mask[KG_class_1 == 'C', KG_class_1_name := 'Temperate']
prec_mask[KG_class_1 == 'D', KG_class_1_name := 'Continental']
prec_mask[KG_class_1 == 'E', KG_class_1_name := 'Polar']

prec_masks <- prec_masks[, c(1:11, 17, 12:13, 15, 14, 16)]

## Save data
saveRDS(prec_masks, paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))


## Validation
to_plot <- prec_stats
p00 <- ggplot() +
  geom_raster(data = to_plot, aes(x = lon, y = lat, fill = elev_class)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = 'Class') +
  scale_x_continuous(expand = c(0.015, 0.015)) +
  scale_fill_manual(values = rev(colset_mid_qual[1:10])) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 

y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0.015, 0.015), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01





