# Volume and area fractions of agreement indices

source('source/partition_evap.R')

evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

evap_mean <- evap_grid[, .(evap_volume = mean(evap_volume), area = area), .(lat, lon)]
evap_mean <- unique(evap_mean)

levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

global <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement)], evap_mean[, .(lon, lat, evap_volume, area)], by = c("lon", "lat"))

global_agreement <- global[, .(evap_sum = sum(evap_volume), area_sum = sum(area)), .(rel_dataset_agreement)]
global_agreement[, volume_fraction := evap_sum/sum(evap_sum)]  
global_agreement[, area_fraction := area_sum/sum(area_sum)]  

global_agreement[, evap_sum := round(evap_sum, 0)]
global_agreement[, area_sum := area_sum*M2_TO_KM2]
global_agreement[, area_sum := round(area_sum, 0)]
global_agreement[, volume_fraction := round(volume_fraction, 2)]  
global_agreement[, area_fraction := round(area_fraction, 2)] 


distribution <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise.rds")))
distribution <-  distribution[!is.na(index),]
quant_thr_0_1 <- quantile(distribution$index, c(0.1))
quant_thr_0_3 <- quantile(distribution$index, c(0.3))
quant_thr_0_7 <- quantile(distribution$index, c(0.7))
quant_thr_0_9 <- quantile(distribution$index, c(0.9))


distribution[index > quant_thr_0_9, agreement_fac := ordered(1, labels = "High")]
distribution[index > quant_thr_0_7 & index <= quant_thr_0_9, agreement_fac := ordered(2, labels = "Above average")]
distribution[index > quant_thr_0_3 & index <= quant_thr_0_7, agreement_fac := ordered(3, labels = "Average")]
distribution[index > quant_thr_0_1 & index <= quant_thr_0_3, agreement_fac := ordered(4, labels = "Below average")]
distribution[index <= quant_thr_0_1, agreement_fac := ordered(5, labels = "Low")] 

global <- merge(distribution[, .(lat, lon, agreement_fac)], evap_mean[, .(lon, lat, evap_volume, area)], by = c("lon", "lat"))

global_agreement <- global[, .(evap_sum = sum(evap_volume), area_sum = sum(area)), .(agreement_fac)]
global_agreement[, volume_fraction := evap_sum/sum(evap_sum)]  
global_agreement[, area_fraction := area_sum/sum(area_sum)]  

global_agreement[, evap_sum := round(evap_sum, 0)]
global_agreement[, area_sum := area_sum*M2_TO_KM2]
global_agreement[, area_sum := round(area_sum, 0)]
global_agreement[, volume_fraction := round(volume_fraction, 2)]  
global_agreement[, area_fraction := round(area_fraction, 2)] 

