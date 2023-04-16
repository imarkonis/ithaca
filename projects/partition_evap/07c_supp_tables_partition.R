# Partition precipitation to different regional properties for SI Tables
source('source/partition_prec.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
land_cover_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, land_cover_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
biome_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, biome_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
elevation_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, elev_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
prec_quant <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, prec_quant, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))

## Analysis
land_cover <- land_cover_class[, .(prec_sum = round(sum(prec_volume_year), 0)), .(land_cover_short_class)]
land_cover <- land_cover[complete.cases(land_cover)]
land_cover <- land_cover[order(-prec_sum)]
land_cover[, prec_cum_sum := cumsum(prec_sum)]
land_cover[, prec_cum_fraction := round(prec_cum_sum / sum(prec_sum), 2)]

biome <- biome_class[, .(prec_sum = round(sum(prec_volume_year), 0)), .(biome_short_class)]
biome <- biome[complete.cases(biome)]
biome <- biome[order(-prec_sum)]
biome[, prec_cum_sum := cumsum(prec_sum)]
biome[, prec_cum_fraction := round(prec_cum_sum / sum(prec_sum), 2)]

elevation <- elevation_class[, .(prec_sum = round(sum(prec_volume_year), 0)), .(elev_class)]
elevation <- elevation[complete.cases(elev_class)]
elevation <- elevation[order(-prec_sum)]
elevation[, prec_cum_sum := cumsum(prec_sum)]
elevation[, prec_cum_fraction := round(prec_cum_sum / sum(prec_sum), 2)]

prec_quant <- prec_quant[, .(prec_sum = round(sum(prec_volume_year), 0)), .(prec_quant)]
prec_quant <- prec_quant[complete.cases(prec_quant)]
prec_quant <- prec_quant[order(-prec_sum)]
prec_quant[, prec_cum_sum := cumsum(prec_sum)]
prec_quant[, prec_cum_fraction := round(prec_cum_sum / sum(prec_sum), 2)]

## Save data
write.csv(land_cover, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_land_cover.csv"))
write.csv(biome, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_biomes.csv"))
