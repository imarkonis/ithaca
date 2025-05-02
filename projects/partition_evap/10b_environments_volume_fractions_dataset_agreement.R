# Volume fraction of quartile agreement over environments ----
source("source/partition_evap.R")

load(paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))

evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

evap_mean <- evap_grid[, .(evap_volume = mean(evap_volume)), .(lat, lon)]

## Variables ----
evap_mask[, KG_class_1_name := factor(KG_class_1_name, levels = levels(evap_mask$KG_class_1_name)[c(5, 4, 2, 3, 1)])]
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

land_cover_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, land_cover_short_class, KG_class_1_name)], evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
biome_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, biome_short_class, KG_class_1_name)], evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
elevation_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, elev_class, KG_class_1_name)], evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
evap_quant <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, evap_quant, KG_class_1_name)], evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
IPCC_ref_regions <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, IPCC_ref_region, KG_class_1_name)], evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
global <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement)], evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"))


### Global ----

global_agreement <- global[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement)]
global_agreement[, fraction := evap_sum/sum(evap_sum)]  

### Land use ----
land_cover_evap <- land_cover_class[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, land_cover_short_class)]
land_cover_evap <- land_cover_evap[complete.cases(land_cover_evap)]
land_cover_evap <- land_cover_evap[order(KG_class_1_name, land_cover_short_class), ]

land_cover_agreement <- land_cover_class[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(rel_dataset_agreement, land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(evap_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := evap_sum / land_cover_sum]

data_high <- land_cover_agreement[rel_dataset_agreement == "Low" | rel_dataset_agreement == "Below average", 
                                  .(fraction = sum(land_cover_fraction)), land_cover_short_class]
data_high[, rank := rank(fraction)]
data_high <- data_high[order(-fraction)]

land_cover_agreement[, land_cover_short_class := factor(land_cover_short_class, levels = c(data_high$land_cover_short_class))]

### Biome types ----
biome_evap <- biome_class[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, biome_short_class)]
biome_evap <- biome_evap[complete.cases(biome_evap)]
biome_evap <- biome_evap[order(KG_class_1_name, biome_short_class), ]

biome_agreement <- biome_class[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(rel_dataset_agreement, biome_short_class), ]
biome_agreement[, biome_sum := sum(evap_sum), biome_short_class]
biome_agreement[, biome_fraction := evap_sum / biome_sum]


data_high <- biome_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                             .(fraction = sum(biome_fraction)), biome_short_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

biome_agreement[, biome_short_class := factor(biome_short_class, levels = data_high$biome_short_class)]

### Elevation ----
elevation_evap <- elevation_class[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, elev_class)]
elevation_evap <- elevation_evap[complete.cases(elevation_evap)]
elevation_evap <- elevation_evap[order(KG_class_1_name, elev_class), ]

elevation_agreement <- elevation_class[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(rel_dataset_agreement, elev_class), ]
elevation_agreement[, elev_sum := sum(evap_sum), elev_class]
elevation_agreement[, elev_fraction := evap_sum / elev_sum]

data_high <- elevation_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                                 .(fraction = sum(elev_fraction)), elev_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

elevation_agreement[, elev_class := factor(elev_class, levels = data_high$elev_class)]

### Evaporation quantiles ----
evap_quant_evap <- evap_quant[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, evap_quant)]
evap_quant_evap <- evap_quant_evap[complete.cases(evap_quant_evap)]
evap_quant_evap <- evap_quant_evap[order(KG_class_1_name, evap_quant), ]

evap_quant_agreement <- evap_quant[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, evap_quant)]
evap_quant_agreement <- evap_quant_agreement[complete.cases(evap_quant_agreement)]
evap_quant_agreement <- evap_quant_agreement[order(rel_dataset_agreement, evap_quant), ]
evap_quant_agreement[, evap_quant_sum := sum(evap_sum), evap_quant]
evap_quant_agreement[, evap_quant_fraction := evap_sum / evap_quant_sum]

data_high <- evap_quant_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                                  .(fraction = sum(evap_quant_fraction)), evap_quant]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

evap_quant_agreement[, evap_quant := factor(evap_quant, levels = data_high$evap_quant)]

### IPCC ----

IPCC_ref_regions_evap <- IPCC_ref_regions[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, IPCC_ref_region)]
IPCC_ref_regions_evap <- IPCC_ref_regions_evap[complete.cases(IPCC_ref_regions_evap)]
IPCC_ref_regions_evap <- IPCC_ref_regions_evap[order(KG_class_1_name, IPCC_ref_region), ]

IPCC_agreement <- IPCC_ref_regions[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, IPCC_ref_region)]
IPCC_agreement <- IPCC_agreement[complete.cases(IPCC_agreement)]
IPCC_agreement <- IPCC_agreement[order(rel_dataset_agreement, IPCC_ref_region), ]
IPCC_agreement[, IPCC_ref_region_sum := sum(evap_sum), IPCC_ref_region]
IPCC_agreement[, IPCC_ref_region_fraction := evap_sum / IPCC_ref_region_sum]

data_high <- IPCC_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                            .(fraction = sum(IPCC_ref_region_fraction)), IPCC_ref_region]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

IPCC_agreement[, IPCC_ref_region := factor(IPCC_ref_region, levels = data_high$IPCC_ref_region)]

## Save data ----
save(land_cover_evap, land_cover_agreement, biome_evap, biome_agreement, 
     elevation_evap, elevation_agreement, evap_quant_evap, evap_quant_agreement, IPCC_ref_regions_evap, IPCC_agreement,
     file = paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))

## Save tables for Zenodo
write.table(land_cover_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "quartile_agreement_land_cover.csv"), sep = ",", row.names = F)
write.table(biome_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "quartile_agreement_biome.csv"), sep = ",", row.names = F)
write.table(elevation_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "quartile_agreement_elevation.csv"), sep = ",", row.names = F)
write.table(evap_quant_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "quartile_agreement_evaporation_quantiles.csv"), sep = ",", row.names = F)
write.table(IPCC_agreement, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "quartile_agreement_ipcc_reference_regions.csv"), sep = ",", row.names = F)

