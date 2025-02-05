# Under and overestimator
source('source/partition_evap.R')

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_land_cover_datasets_for_plot.rds"))

data_summary <- data[, .N, .(dataset, performance)]
landcover <- dcast(data_summary, dataset ~ performance)
landcover[is.na(landcover)] <- 0


data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_biome_datasets_for_plot.rds"))
data_summary <- data[, .N, .(dataset, performance)]
biomes <- dcast(data_summary, dataset ~ performance)
biomes[is.na(biomes)] <- 0



data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_IPCC_datasets_for_plot.rds"))
data_summary <- data[, .N, .(dataset, performance)]
IPCC <- dcast(data_summary, dataset ~ performance)
IPCC[is.na(IPCC)] <- 0



data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_elevation_datasets_for_plot.rds"))
data_summary <- data[, .N, .(dataset, performance)]
elevation <- dcast(data_summary, dataset ~ performance)
elevation[is.na(elevation)] <- 0
