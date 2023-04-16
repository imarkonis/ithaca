# Partition precipitation to different regional properties and quantify their uncertainty
source('source/partition_prec.R')

## Data 
prec_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))

## Variables
prec_datasets_area <- merge(prec_datasets[, .(lon, lat, dataset)], 
                    prec_grid[, .(lon, lat, area)], by = c("lon", "lat"), all = TRUE)
global_area_dataset <- prec_datasets_area[, .(area = sum(area)), .(dataset)] 
prec_annual_vol <- prec_annual_vol[global_area_dataset, on = .(dataset)]

prec_annual_vol <- prec_annual_vol[, prec_volume_year := area  * M2_TO_KM2 * prec_mean * MM_TO_KM
][, prec_mean := NULL][, area := NULL] # km3

prec_annual_vol[dataset %in% PREC_GLOBAL_DATASETS, 
                                        .(prec_volume = mean(prec_volume_year, na.rm = TRUE)), dataset]

prec_dataset_sd <- prec_annual_vol[dataset %in% PREC_GLOBAL_DATASETS, .(prec_sd = sd(prec_volume_year, na.rm = TRUE)), dataset]
prec_dataset_sd[, range(prec_sd)]
prec_dataset_sd[, median(prec_sd)]

prec_annual_sd <- prec_annual_vol[dataset %in% PREC_GLOBAL_DATASETS, .(prec_sd = sd(prec_volume_year, na.rm = TRUE)), year]
prec_annual_sd <- prec_annual_sd[complete.cases(prec_annual_sd)]
prec_annual_sd[, range(prec_sd)]
prec_annual_sd[, median(prec_sd)]

