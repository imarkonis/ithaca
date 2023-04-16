# Partition precipitation to different dataset agreement classes and quantify their uncertainty
source('source/partition_prec.R')

## Data 
source('source/partition_prec.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))

## Variables
dataset_agreement <- merge(prec_mask[, .(lon, lat, rel_dataset_agreement)], 
                           prec_grid[, .(lon, lat, area)], 
                           by = c("lon", "lat"))
dataset_agreement <- merge(dataset_agreement, prec_datasets[dataset %in% PREC_GLOBAL_DATASETS], by = c("lon", "lat"))
dataset_agreement[, prec_volume_year := area * M2_TO_KM2 * prec_mean * MM_TO_KM
][, prec_mean := NULL][, prec_sd := NULL][, area := NULL] # km3


## Analysis
sd(dataset_agreement[rel_dataset_agreement == 'low' | rel_dataset_agreement == 'below average', 
                     sum(prec_volume_year), .(dataset)]$V1)

sd(dataset_agreement[rel_dataset_agreement != 'low' & rel_dataset_agreement != 'below average', 
                     sum(prec_volume_year), .(dataset)]$V1)

dataset_agreement_spread <- dataset_agreement[, .(prec_volume_year = sum(prec_volume_year)), .(rel_dataset_agreement, dataset)]
dataset_agreement_spread <- dataset_agreement_spread[, .(prec_mean = round(mean(prec_volume_year), 0),
                                                         prec_sd = round(sd(prec_volume_year), 0), 
                                                         cv = round(sd(prec_volume_year) / mean(prec_volume_year), 2)), 
                                                     rel_dataset_agreement]
dataset_agreement_spread <- dataset_agreement_spread[c(3, 2, 1, 4, 5)]
