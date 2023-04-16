# Add categorical classes to each variable 

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/mask_paths.R')

## Packages
library("gtools")

## Data 
masks_land <- readRDS(paste0(PATH_SAVE, "/misc/masks_land.rds"))
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))

## Masks
### Precipitation
prec_stats[, prec_quant := ordered(quantcut(ens_mean_mean, 10), labels = c('0-0.1', '0.1-0.2', '0.2-0.3', '0.3-0.4', '0.4-0.5',
                                                                           '0.5-0.6', '0.6-0.7', '0.7-0.8', '0.8-0.9', '0.9-1'))]
prec_stats[, prec_class := ordered(quantcut(ens_mean_mean, 5), labels = c('low', 'below average', 'average', 'above average', 'high'))]

### Uncertainty - Dataset agreement
prec_stats[, quant_ens_cv := ordered(quantcut(ens_mean_cv, 5), 
                                         labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00')), prec_quant]

prec_stats[std_quant_range <= 0.11, rel_dataset_agreement := ordered(1, labels = "high")] 
prec_stats[std_quant_range > 0.11 & std_quant_range <= 0.18, rel_dataset_agreement := ordered(3, labels = "above average")]
prec_stats[std_quant_range > 0.18 & std_quant_range <= 0.38, rel_dataset_agreement := ordered(4, labels = "average")]
prec_stats[std_quant_range > 0.38 & std_quant_range <= 0.61, rel_dataset_agreement := ordered(5, labels = "below average")]
prec_stats[std_quant_range > 0.61, rel_dataset_agreement := ordered(7, labels = "low")]

prec_stats[std_quant_range <= 0.1, abs_dataset_agreement := ordered(1, labels = "high")] 
prec_stats[std_quant_range > 0.1 & std_quant_range <= 0.25, abs_dataset_agreement := ordered(3, labels = "above average")]
prec_stats[std_quant_range > 0.25 & std_quant_range <= 0.5, abs_dataset_agreement := ordered(4, labels = "average")]
prec_stats[std_quant_range > 0.5 & std_quant_range <= 1, abs_dataset_agreement := ordered(5, labels = "below average")]
prec_stats[std_quant_range > 1, abs_dataset_agreement := ordered(7, labels = "low")]

prec_stats[, prec_quant_dataset_agreement := ordered(quantcut(std_quant_range, c(0, 0.1, 0.3, 0.7, 0.9, 1)),
                                                          labels = c('high', 'above average', 'average', 'below average', 'low')), prec_quant]

prec_masks <- merge(masks_land, prec_stats[, .(lon, lat, prec_quant, rel_dataset_agreement, #Merges only complete cases
                               abs_dataset_agreement, 
                               prec_quant_dataset_agreement)], by = c("lon", "lat"))

## Save data
saveRDS(prec_masks, paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
