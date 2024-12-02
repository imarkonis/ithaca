## area fractions of environments ----

source('source/partition_evap.R')
source('source/geo_functions.R')

## data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
grid_cell_area <- unique(evap_mask[, .(lon, lat)]) %>% grid_area() # m2
evap_mask <- grid_cell_area[evap_mask, on = .(lon, lat)]

## landcover ----

landcover <- evap_mask[, .(area = sum(area)), .(land_cover_short_class)]
landcover[, area_fraction := round(area/sum(area)*100,2)]

saveRDS(landcover, paste0(PATH_SAVE_PARTITION_EVAP, "land_cover_area_fraction.rds"))
write.table(landcover, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "land_cover_area_fraction.cvs"), row.names = F)


## biomes ----

biome <- evap_mask[, .(area = sum(area)), .(biome_short_class)]
biome <- biome[!is.na(biome_short_class)]

biome[, area_fraction := round(area/sum(area)*100,2)]

saveRDS(biome, paste0(PATH_SAVE_PARTITION_EVAP, "biome_area_fraction.rds"))
write.table(biome, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "biome_area_fraction.cvs"), row.names = F)


## IPCC ----

ipcc <- evap_mask[, .(area = sum(area)), .(IPCC_ref_region)]
ipcc <- ipcc[!is.na(IPCC_ref_region)]

ipcc[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
ipcc[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
ipcc[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
ipcc[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
ipcc[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
ipcc[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]

ipcc[, area_fraction := round(area/sum(area)*100,2)]
saveRDS(ipcc, paste0(PATH_SAVE_PARTITION_EVAP, "ipcc_area_fraction.rds"))
write.table(ipcc, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "ipcc_area_fraction.cvs"), row.names = F)


## elevation  ----

elevation <- evap_mask[, .(area = sum(area)), .(elev_class)]
elevation[, area_fraction := round(area/sum(area)*100,2)]
saveRDS(elevation, paste0(PATH_SAVE_PARTITION_EVAP, "elevation_area_fraction.rds"))
write.table(elevation, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "elevation_area_fraction.cvs"), row.names = F)


## evaporation quantiles  ----

evap <- evap_mask[, .(area = sum(area)), .(evap_quant)]
evap[, area_fraction := round(area/sum(area)*100,2)]
saveRDS(evap, paste0(PATH_SAVE_PARTITION_EVAP, "evaporation_quantiles_area_fraction.rds"))
write.table(evap, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "evap_area_fraction.cvs"), row.names = F)

