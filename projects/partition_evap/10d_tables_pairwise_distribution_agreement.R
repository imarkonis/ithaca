## Table for Zenodo pair-wise distribution agreement ---- 
source('source/partition_evap.R')

## landcover ----
landcover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_land_cover.rds"))
summary(landcover$volume_fraction)
q10 <- quantile(landcover$volume_fraction, c(0.1))
q30 <- quantile(landcover$volume_fraction, c(0.3))
q70 <- quantile(landcover$volume_fraction, c(0.7))
q90 <- quantile(landcover$volume_fraction, c(0.9))

landcover[, pairwise_distribution_agreement:= cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                                labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

## biome ----

biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))
q10 <- quantile(biome$volume_fraction, c(0.1))
q30 <- quantile(biome$volume_fraction, c(0.3))
q70 <- quantile(biome$volume_fraction, c(0.7))
q90 <- quantile(biome$volume_fraction, c(0.9))

biome[, pairwise_distribution_agreement:= cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                            labels = c("Low", "Below Average", "Average", "Above Average", "High"))]


## ipcc reference regions ----
ipcc <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_ipcc.rds"))
q10 <- quantile(ipcc$volume_fraction, c(0.1))
q30 <- quantile(ipcc$volume_fraction, c(0.3))
q70 <- quantile(ipcc$volume_fraction, c(0.7))
q90 <- quantile(ipcc$volume_fraction, c(0.9))

ipcc[, pairwise_distribution_agreement:= cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

## elevation ----

elev <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_elev.rds"))
q10 <- quantile(elev$volume_fraction, c(0.1))
q30 <- quantile(elev$volume_fraction, c(0.3))
q70 <- quantile(elev$volume_fraction, c(0.7))
q90 <- quantile(elev$volume_fraction, c(0.9))

elev[, pairwise_distribution_agreement:= cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

## evap ----

evap <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_evap.rds"))
q10 <- quantile(evap$volume_fraction, c(0.1))
q30 <- quantile(evap$volume_fraction, c(0.3))
q70 <- quantile(evap$volume_fraction, c(0.7))
q90 <- quantile(evap$volume_fraction, c(0.9))

evap[, pairwise_distribution_agreement:= cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

## Save tables for Zenodo
write.table(landcover, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "pairwise_distribution_agreement_land_cover.csv"), sep = ",", row.names = F)
write.table(biome , paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "pairwise_distribution_agreement_biome.csv"), sep = ",", row.names = F)
write.table(ipcc, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "pairwise_distribution_agreement_ipcc_reference_regions.csv"), sep = ",", row.names = F)
write.table(elev, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "pairwise_distribution_agreement_elevation.csv"), sep = ",", row.names = F)
write.table(evap, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "pairwise_distribution_agreement_evaporation_quantiles.csv"), sep = ",", row.names = F)

