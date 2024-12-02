# Join all agreement indices grid-wise

source("source/partition_evap.R")

evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
distribution <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise.rds")))

## Quartile agreement ----
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")

## Quartile range ----
evap_mask[, Qdiff := ens_mean_q75-ens_mean_q25]

quant_iqr_0_1 <- quantile(evap_mask$Qdiff, c(0.1))
quant_iqr_0_3 <- quantile(evap_mask$Qdiff, c(0.3))
quant_iqr_0_7 <- quantile(evap_mask$Qdiff, c(0.7))
quant_iqr_0_9 <- quantile(evap_mask$Qdiff, c(0.9))

evap_mask[Qdiff > quant_iqr_0_9, Qdiff_brk := ordered(1, labels = "High")]
evap_mask[Qdiff > quant_iqr_0_7 & Qdiff <= quant_iqr_0_9, Qdiff_brk := ordered(2, labels = "Above average")]
evap_mask[Qdiff > quant_iqr_0_3 & Qdiff <= quant_iqr_0_7, Qdiff_brk := ordered(3, labels = "Average")]
evap_mask[Qdiff> quant_iqr_0_1 & Qdiff <= quant_iqr_0_3, Qdiff_brk := ordered(4, labels = "Below average")]
evap_mask[Qdiff <= quant_iqr_0_1, Qdiff_brk := ordered(5, labels = "Low")] 
evap_mask[, IQR_agreement := Qdiff_brk] 


## Distribution agreement ----
quant_thr_0_1 <- quantile(distribution$index, c(0.1))
quant_thr_0_3 <- quantile(distribution$index, c(0.3))
quant_thr_0_7 <- quantile(distribution$index, c(0.7))
quant_thr_0_9 <- quantile(distribution$index, c(0.9))

distribution[index > quant_thr_0_9, dist_dataset_agreement := ordered(1, labels = "High")]
distribution[index > quant_thr_0_7 & index <= quant_thr_0_9, dist_dataset_agreement := ordered(2, labels = "Above average")]
distribution[index > quant_thr_0_3 & index <= quant_thr_0_7, dist_dataset_agreement := ordered(3, labels = "Average")]
distribution[index > quant_thr_0_1 & index <= quant_thr_0_3, dist_dataset_agreement := ordered(4, labels = "Below average")]
distribution[index <= quant_thr_0_1, dist_dataset_agreement := ordered(5, labels = "Low")] 



grid_wise_agreement_environments <- merge(evap_mask[,.(lon, lat, land_cover_short_class, biome_short_class,
                                                       IPCC_ref_region, elev_class, evap_quant, IQR_agreement, rel_dataset_agreement)],
                                          distribution[,.(lon, lat, dist_dataset_agreement)], by = c("lon", "lat"), all = T)

grid_wise_agreement_environments <- merge(evap_grid [,.(lon, lat, area)],grid_wise_agreement_environments,
                                          by = c("lon", "lat"), all = T)

## Save data ----

saveRDS(grid_wise_agreement_environments, paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))

## Table Zenodo ----
write.table(grid_wise_agreement_environments, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "dataset_agreement_grid_wise.csv"), sep = ",", row.names = F)
