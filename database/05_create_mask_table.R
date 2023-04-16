# Creates a data.table with categorical masks
source('source/main.R')
source('source/geo_functions.R')
source('source/mask_paths.R')

## Data 
prec_era5 <- brick(paste0(PATH_PREC_SIM, "era5_tp_mm_land_195901_202112_025_yearly.nc"))

## Masks at 0.25 resolution
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
shape_mask_dt <- merge(shape_mask_dt, shape_mask_df, by = c('lon', 'lat'), all = TRUE)

### Biomes
fname_shape <- list.files(path = PATH_MASKS_BIOME, full.names = TRUE, pattern = "mask_biomes_dinerstein.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]]) 
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'layer_BIOME_NAME'))
colnames(shape_mask_df) <- c('lon', 'lat', 'biome_class')
shape_mask_df$biome_class <- factor(shape_mask_df$biome_class)
shape_mask_dt <- merge(shape_mask_dt, shape_mask_df, by = c('lon', 'lat'), all = TRUE)

### Koppen-Geiger
fname_shape <- list.files(path = PATH_MASKS_KOPPEN, full.names = TRUE, pattern = "climate_beck_level3.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)
shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class_1')
shape_mask_df$KG_class_1 <- factor(shape_mask_df$KG_class_1)
shape_mask_dt  <- data.table(shape_mask_df)

fname_shape <- list.files(path = PATH_MASKS_KOPPEN, full.names = TRUE, pattern = "climate_beck_level2.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)
shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class_2')
shape_mask_df$KG_class_2 <- factor(shape_mask_df$KG_class_2)
shape_mask_dt <- merge(shape_mask_dt, shape_mask_df, by = c('lon', 'lat'), all.x = TRUE)

fname_shape <- list.files(path = PATH_MASKS_KOPPEN, full.names = TRUE, pattern = "climate_beck_level1.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)
shape_mask_raster <- rasterize(shape_mask, prec_era5[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class_3')
shape_mask_df$KG_class_3 <- factor(shape_mask_df$KG_class_3)
shape_mask_dt <- merge(shape_mask_dt, shape_mask_df, by = c('lon', 'lat'))

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
shape_mask_dt <- merge(shape_mask_dt, shape_mask_df, by = c('lon', 'lat'), all = TRUE)

### Extra masks
shape_mask_dt[KG_class_1 == 'A', KG_class_1_name := factor("Tropical")]
shape_mask_dt[KG_class_1 == 'B', KG_class_1_name := factor("Arid")]
shape_mask_dt[KG_class_1 == 'C', KG_class_1_name := factor("Temperate")]
shape_mask_dt[KG_class_1 == 'D', KG_class_1_name := factor("Continental")]
shape_mask_dt[KG_class_1 == 'E', KG_class_1_name := factor("Polar")]

shape_mask_dt[grepl("Shrub", land_class) == TRUE, land_use_short_class := "Shrublands"]
shape_mask_dt[grepl("Forest", land_class) == TRUE, land_use_short_class := "Forests"]
shape_mask_dt[grepl("Savannas", land_class) == TRUE, land_use_short_class := "Savannas"]
shape_mask_dt[grepl("Cropland", land_class) == TRUE, land_use_short_class := "Croplands"]
shape_mask_dt[grepl("Grasslands", land_class) == TRUE, land_use_short_class := "Grasslands"]
shape_mask_dt[grepl("Urban", land_class) == TRUE, land_use_short_class := "Other"]
shape_mask_dt[grepl("Unclassified", land_class) == TRUE, land_use_short_class := "Other"]
shape_mask_dt[grepl("Ice", land_class) == TRUE, land_use_short_class := "Snow/Ice"]
shape_mask_dt[grepl("Water", land_class) == TRUE, land_use_short_class := "Water"]
shape_mask_dt[grepl("Wetlands", land_class) == TRUE, land_use_short_class := "Water"]
shape_mask_dt[grepl("Barren", land_class) == TRUE, land_use_short_class := "Barren"]
shape_mask_dt[, land_use_short_class := factor(land_use_short_class)]

shape_mask_dt[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
shape_mask_dt[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
shape_mask_dt[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Forests"]
shape_mask_dt[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Forests"]
shape_mask_dt[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Forests"]
shape_mask_dt[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Forests"]
shape_mask_dt[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. Forests"]
shape_mask_dt[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
shape_mask_dt[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
shape_mask_dt[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
shape_mask_dt[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
shape_mask_dt[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Flooded"]
shape_mask_dt[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
shape_mask_dt[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
shape_mask_dt[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
shape_mask_dt[, biome_short_class := factor(biome_short_class)]

shape_mask_dt <- shape_mask_dt[, c(1:14, 16, 15, 17)]

shape_mask_dt <- shape_mask_dt[, .(lon, lat, elev_class, KG_class_1,  KG_class_2,  KG_class_3, KG_class_1_name, 
                             land_cover_class = land_class, land_cover_short_class = land_use_short_class, biome_class, biome_short_class)]

## Save data
saveRDS(shape_mask_dt, paste0(PATH_SAVE, "/misc/masks_global.rds"))
saveRDS(shape_mask_dt[land_cover_class != "Water"], paste0(PATH_SAVE, "/misc/masks_land.rds"))

