# Volume fraction of distribution agreement over environments ----

source('source/partition_evap.R')
source('source/graphics.R')

## Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

evap_grid <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_gridwise.rds")))
evap_grid <-  evap_grid[!is.na(agreement_index),]
evap_grid[, summary(agreement_index)]

evap_vol_mean <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_mean <- evap_vol_mean[, .(evap_volume = mean(evap_volume)), .(lat, lon)]

## Analysis ----
### Relative Distribution\nagreement at quantile 0.1, 0.3, 0.7. 0.9 ----
quant_thr_0_1 <- quantile(evap_grid$agreement_index, c(0.1))
quant_thr_0_3 <- quantile(evap_grid$agreement_index, c(0.3))
quant_thr_0_7 <- quantile(evap_grid$agreement_index, c(0.7))
quant_thr_0_9 <- quantile(evap_grid$agreement_index, c(0.9))

evap_grid[agreement_index > quant_thr_0_9, agreement_fac := ordered(1, labels = "High")]
evap_grid[agreement_index > quant_thr_0_7 & agreement_index <= quant_thr_0_9, agreement_fac := ordered(2, labels = "Above average")]
evap_grid[agreement_index > quant_thr_0_3 & agreement_index <= quant_thr_0_7, agreement_fac := ordered(3, labels = "Average")]
evap_grid[agreement_index > quant_thr_0_1 & agreement_index <= quant_thr_0_3, agreement_fac := ordered(4, labels = "Below average")]
evap_grid[agreement_index <= quant_thr_0_1, agreement_fac := ordered(5, labels = "Low")] 


evap_grid <- evap_mean[evap_grid, on = .(lon, lat)]

evap_grid <- evap_grid[complete.cases(evap_grid),]

land_cover_class <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], evap_grid[, .(lon, lat, evap_volume, agreement_index, agreement_fac)], by = c("lon", "lat"))
biome_class <- merge(evap_mask[, .(lat, lon, biome_short_class)], evap_grid[, .(lon, lat, evap_volume, agreement_index, agreement_fac)], by = c("lon", "lat"))
elevation_class <- merge(evap_mask[, .(lat, lon, elev_class)], evap_grid[, .(lon, lat, evap_volume, agreement_index, agreement_fac)], by = c("lon", "lat"))
evap_quant <- merge(evap_mask[, .(lat, lon, evap_quant)], evap_grid[, .(lon, lat, evap_volume, agreement_index, agreement_fac)], by = c("lon", "lat"))
IPCC_ref_regions <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], evap_grid[, .(lon, lat, evap_volume, agreement_index, agreement_fac)], by = c("lon", "lat"))

## Analysis
### Global
global_agreement <- evap_grid[, .(evap_sum = sum(evap_volume)), .(agreement_fac)]
global_agreement[, fraction := evap_sum/sum(evap_sum)]  

### Land use ----
land_cover_agreement <- land_cover_class[, .(evap_sum = sum(evap_volume)), .(agreement_fac, land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(agreement_fac, land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(evap_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := evap_sum / land_cover_sum]

data_high <- land_cover_agreement[agreement_fac == "High" | agreement_fac == "Above average", 
                                  .(fraction = sum(land_cover_fraction)), land_cover_short_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

land_cover_agreement[, land_cover_short_class := factor(land_cover_short_class, levels = data_high$land_cover_short_class)]

### Biome types
biome_agreement <- biome_class[, .(evap_sum = sum(evap_volume)), .(agreement_fac, biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(agreement_fac, biome_short_class), ]
biome_agreement[, biome_sum := sum(evap_sum), biome_short_class]
biome_agreement[, biome_fraction := evap_sum / biome_sum]

data_high <- biome_agreement[agreement_fac == "High" | agreement_fac == "Above average", 
                             .(fraction = sum(biome_fraction)), biome_short_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

biome_agreement[, biome_short_class := factor(biome_short_class, levels = data_high$biome_short_class)]

### Elevation ----

elevation_agreement <- elevation_class[, .(evap_sum = sum(evap_volume)), .(agreement_fac, elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(agreement_fac, elev_class), ]
elevation_agreement[, elev_sum := sum(evap_sum), elev_class]
elevation_agreement[, elev_fraction := evap_sum / elev_sum]

data_high <- elevation_agreement[agreement_fac == "High" | agreement_fac == "Above average", 
                                 .(fraction = sum(elev_fraction)), elev_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

elevation_agreement[, elev_class := factor(elev_class, levels = data_high$elev_class)]

### Evaporation quantiles ----

evap_quant_agreement <- evap_quant[, .(evap_sum = sum(evap_volume)), .(agreement_fac, evap_quant)]
evap_quant_agreement <- evap_quant_agreement[complete.cases(evap_quant_agreement)]
evap_quant_agreement <- evap_quant_agreement[order(agreement_fac, evap_quant), ]
evap_quant_agreement[, evap_quant_sum := sum(evap_sum), evap_quant]
evap_quant_agreement[, evap_quant_fraction := evap_sum / evap_quant_sum]
data_high <- evap_quant_agreement[agreement_fac == "High" | agreement_fac == "Above average", 
                                  .(fraction = sum(evap_quant_fraction)), evap_quant]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

evap_quant_agreement[, evap_quant := factor(evap_quant, levels = data_high$evap_quant)]

### IPCC ----
IPCC_agreement <- IPCC_ref_regions[, .(evap_sum = sum(evap_volume)), .(agreement_fac, IPCC_ref_region)]
IPCC_agreement <- IPCC_agreement[complete.cases(IPCC_agreement)]
IPCC_agreement <- IPCC_agreement[order(agreement_fac, IPCC_ref_region), ]
IPCC_agreement[, IPCC_ref_region_sum := sum(evap_sum), IPCC_ref_region]
IPCC_agreement[, IPCC_ref_region_fraction := evap_sum / IPCC_ref_region_sum]
data_high <- IPCC_agreement[agreement_fac == "High" | agreement_fac == "Above average", 
                            .(fraction = sum(IPCC_ref_region_fraction)), IPCC_ref_region]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

IPCC_agreement[, IPCC_ref_region := factor(IPCC_ref_region, levels = c("NZ", as.character(data_high$IPCC_ref_region)))]
## Save data
save(land_cover_agreement, biome_agreement, 
     elevation_agreement,evap_quant_agreement, IPCC_agreement,
     file = paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_masks.Rdata"))

## Save tables for Zenodo
write.table(land_cover_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "distribution_agreement_land_cover.csv"), sep = ",", row.names = F)
write.table(biome_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "distribution_agreement_biome.csv"), sep = ",", row.names = F)
write.table(elevation_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "distribution_agreement_elevation.csv"), sep = ",", row.names = F)
write.table(evap_quant_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "distribution_agreement_evaporation_quantiles.csv"), sep = ",", row.names = F)
write.table(IPCC_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "distribution_agreement_ipcc_reference_regions.csv"), sep = ",", row.names = F)
