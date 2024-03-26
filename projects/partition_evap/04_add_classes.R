# Addition of categorical classes to each grid cell 

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/mask_paths.R')

## Packages
library("gtools")

## Data 
masks_global <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))
evap_stats <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats.rds"))
evap_volume <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

## Masks
### Evaporation
evap_stats[, evap_quant := ordered(quantcut(ens_mean_mean, 10), 
                                   labels = c('0-0.1', '0.1-0.2', '0.2-0.3', '0.3-0.4', '0.4-0.5',     #Caution: This is biased due to different grid size
                                              '0.5-0.6', '0.6-0.7', '0.7-0.8', '0.8-0.9', '0.9-1'))]

### Uncertainty - Dataset agreement
evap_stats[, quant_ens_cv := ordered(quantcut(ens_mean_cv, 5), 
                                         labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00')), evap_quant]


### Relative dataset agreement at quantile 0.1, 0.3, 0.7. 0.9
quant_thr_0_1 <- quantile(evap_stats$std_quant_range, c(0.1))
quant_thr_0_3 <- quantile(evap_stats$std_quant_range, c(0.3))
quant_thr_0_7 <- quantile(evap_stats$std_quant_range, c(0.7))
quant_thr_0_9 <- quantile(evap_stats$std_quant_range, c(0.9))

evap_stats[std_quant_range <= quant_thr_0_1, rel_dataset_agreement := ordered(1, labels = "high")] 
evap_stats[std_quant_range > quant_thr_0_1 & std_quant_range <= quant_thr_0_3, rel_dataset_agreement := ordered(3, labels = "above average")]
evap_stats[std_quant_range > quant_thr_0_3 & std_quant_range <= quant_thr_0_7, rel_dataset_agreement := ordered(4, labels = "average")]
evap_stats[std_quant_range > quant_thr_0_7 & std_quant_range <= quant_thr_0_9, rel_dataset_agreement := ordered(5, labels = "below average")]
evap_stats[std_quant_range > quant_thr_0_9, rel_dataset_agreement := ordered(7, labels = "low")]


evap_stats[, evap_quant_dataset_agreement := ordered(quantcut(std_quant_range, c(0, 0.1, 0.3, 0.7, 0.9, 1)),
                                                     labels = c('high', 'above average', 'average', 'below average', 'low')), evap_quant]


evap_masks <- merge(masks_global, evap_stats[, .(lon, lat, evap_quant, rel_dataset_agreement, #Merges only complete cases
                               std_quant_range, evap_quant_dataset_agreement)], by = c("lon", "lat"))



evap_masks[, biome_dataset_agreement := ordered(quantcut(std_quant_range, c(0, 0.1, 0.3, 0.7, 0.9, 1)),
                                                labels = c('high', 'above average', 'average', 'below average', 'low')), biome_class]


evap_masks[, landcover_dataset_agreement := ordered(quantcut(std_quant_range, c(0, 0.1, 0.3, 0.7, 0.9, 1)),
                                                    labels = c('high', 'above average', 'average', 'below average', 'low')), land_cover_short_class]

evap_masks[, grid_count_ipcc := .N, IPCC_ref_region]
evap_masks[grid_count_ipcc < 10, IPCC_ref_region := NA]
evap_masks[grepl("O", as.character(IPCC_ref_region)) == TRUE, ocean := "yes"]
evap_masks[IPCC_ref_region %in% c("BOB", "ARS"), ocean := "yes"]
evap_masks[ocean == "yes", IPCC_ref_region:= NA]
evap_masks[, ipcc_dataset_agreement := ordered(quantcut(std_quant_range, c(0, 0.1, 0.3, 0.7, 0.9, 1)),
                                                    labels = c('high', 'above average', 'average', 'below average', 'low')), IPCC_ref_region]

evap_masks[,ocean:= NULL]

## Save data
saveRDS(evap_masks, paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))


## Code review
evap_masks <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

ggplot(evap_masks)+
  geom_tile(aes(x = lon, y = lat, fill = rel_dataset_agreement))+
  theme_bw()

ggplot(evap_masks)+
  geom_tile(aes(x = lon, y = lat, fill = evap_quant_dataset_agreement))+
  theme_bw()

IPCC_test <- evap_masks[, .N, IPCC_ref_region]

ggplot(IPCC_test)+
  geom_bar(aes(y = N, x = IPCC_ref_region), stat = "identity")+
  theme_bw()
