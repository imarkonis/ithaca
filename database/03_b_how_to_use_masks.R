# load directories to mask data
source("source/masks_source.R")

# load sample data
source("database/scripts/03_a_load_sample_data.R")

library(data.table)
library(rasterVis)
library(sf)
library(ggplot2)

### Shape masks 

## Koeppen-Geiger Beck et al. 2022
## 0.5 x 0.5 resolution
fname_shape <- list.files(path = masks_dir_KG_beck, full.names = T, pattern = "climate_beck_level3.shp")
shape_mask <- st_read(paste0(fname_shape[1]))

unique(shape_mask$classes)

ggplot(shape_mask)+
  geom_sf(aes(geometry = geometry, fill = classes), col = NA)+
  theme_bw()

shape_mask_select_class <- subset(shape_mask, classes == "E")
shape_mask_selet_several_classes <- shape_mask[which(shape_mask$classes %in% c("A", "B")),]

test_data_mask <- mask(test_data, shape_mask_select_class)
image(test_data_mask, col = "royalblue4")

test_data_mask_several_classes <- mask(test_data, shape_mask_selet_several_classes )
image(test_data_mask_several_classes, col = "royalblue4")

## country borders original from library(maptools)
fname_shape <- list.files(path = masks_dir_country, full.names = T, pattern = "borders.shp")
shape_mask <- st_read(paste0(fname_shape[1]))

unique(shape_mask$classes)
shape_mask_select_class <- subset(shape_mask, classes == "Kenya")

test_data_crop <- crop(test_data, extent(shape_mask_select_class))
test_data_mask <- mask(test_data_crop, shape_mask_select_class)
image(test_data_mask, col = "royalblue4")

## Biomes - dinerstein et al 2017
fname_shape <- list.files(path = masks_dir_ecoregions, full.names = T, pattern = "mask_biomes*")
shape_mask <- st_read(paste0(fname_shape[1]))

unique(shape_mask$classes)
shape_mask_select_class <- subset(shape_mask, classes == "Boreal Forests/Taiga")

test_data_crop <- crop(test_data, extent(shape_mask_select_class))
test_data_mask <- mask(test_data_crop, shape_mask_select_class)
image(test_data_mask, col = "royalblue4")

## Sediment Boerker et al. 

# pyroclastics
fname_shape <- list.files(path = masks_dir_sed, pattern = '*sediment_pyro', full.names = T)
shape_mask <- st_read(fname_shape[1])
unique(shape_mask$classes)

test_mask <- mask(test_data, shape_mask)
image(test_mask, col = "royalblue4")

# sediments - careful larger data product
fname_shape <- list.files(path = masks_dir_sed, pattern = '*sediment_boerker.shp', full.names = T)
shape_mask <- st_read(fname_shape[1])
unique(shape_mask$classes)

shape_mask_select_class <- subset(shape_mask, classes == "Us")

test_mask <- mask(test_data, shape_mask_select_class)
image(test_mask, col = "royalblue4")

## Turn shape file into gridded data using rasterize
fname_shape <- list.files(path = masks_dir_country, full.names = T, pattern = "borders.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask_select_class <- subset(shape_mask, classes == "Kenya")

test_data_crop <- crop(test_data, extent(shape_mask_select_class))
shape_mask_as_raster <- rasterize(shape_mask_select_class, test_data_crop)
image(shape_mask_as_raster, col = "royalblue4")

### Raster masks

## Land Ocean IMERG - binary mask
mask_fname <- list.files(path = masks_dir_landsea, pattern = "mask_land_ocean_025" , full.names = T)
mask_raster <- stack(paste(mask_fname[1]))
test_data_mask <- mask(test_data, mask_raster, inverse = T)
image(test_data_mask)

test_data_mask <- mask(test_data, mask_raster, inverse = F)
image(test_data_mask)

## Elevation Zones- era 5 geopotential
mask_fname <- list.files(path = masks_dir_oro, pattern = "*groups_025.nc" , full.names = T)
mask_raster <- raster(paste(mask_fname[1]))
mask_raster <- ratify(mask_raster)

mask_fname <- list.files(path = masks_dir_oro, pattern = "*groups_025_classes.txt" , full.names = T)
mask_raster_classes <- read.table(paste(mask_fname[1]))

levels(mask_raster)[[1]] <- mask_raster_classes
levelplot(mask_raster)
levels(mask_raster)[[1]]

mask_raster_select <- mask_raster
mask_raster_select[which(values(mask_raster) > 4)] <- NA

test_data_mask <- mask(test_data, mask_raster_select)
image(test_data_mask, col = "royalblue4")

test_data_mask <- mask(test_data, mask_raster_select, inverse = T)
image(test_data_mask, col = "royalblue4")



## Modis landcover

mask_fname <- list.files(path = masks_dir_landcover, pattern = "*modis_025.nc" , full.names = T)
mask_raster <- raster(paste(mask_fname[1]))
mask_raster <- ratify(mask_raster)

mask_fname <- list.files(path = masks_dir_landcover, pattern = "*modis_025_classes.txt" , full.names = T)
mask_raster_classes <- read.table(paste(mask_fname[1]))

levels(mask_raster)[[1]] <- mask_raster_classes
levelplot(mask_raster)
levels(mask_raster)[[1]]

mask_raster_select <- mask_raster
mask_raster_select[which(values(mask_raster) > 4)] <- NA

test_data_mask <- mask(test_data, mask_raster_select)
image(test_data_mask, col = "royalblue4")

test_data_mask <- mask(test_data, mask_raster_select, inverse = T)
image(test_data_mask, col = "royalblue4")

## Lithology from Hartmann and Moosdorf

mask_fname <- list.files(path = masks_dir_lith, pattern = "*hartmann_025.nc" , full.names = T)
mask_raster <- raster(paste(mask_fname[1]))
mask_raster <- ratify(mask_raster)

mask_fname <- list.files(path = masks_dir_lith, pattern = "*hartmann_025_classes.txt" , full.names = T)
mask_raster_classes <- read.table(paste(mask_fname[1]))

levels(mask_raster)[[1]] <- mask_raster_classes
levelplot(mask_raster)
levels(mask_raster)[[1]]

mask_raster_select <- mask_raster
mask_raster_select[which(values(mask_raster) > 10)] <- NA

test_data_mask <- mask(test_data, mask_raster_select)
image(test_data_mask, col = "royalblue4")

# Alternative selection of classes
mask_raster_select <- mask_raster
id_raster_select <- which(levels(mask_raster)[[1]]$classes %in% c("su", "vb"))

mask_raster_select[which(values(mask_raster) %in% id_raster_select)] <- NA
test_data_mask <- mask(test_data, mask_raster_select)
image(test_data_mask, col = "royalblue4")



