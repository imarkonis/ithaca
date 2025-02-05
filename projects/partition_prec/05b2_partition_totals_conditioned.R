# Same as 05b but for conditioned dataset agreement

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

library(ggthemes)
library(scales)

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))

## Variables
prec_mask[, KG_class_1_name := relevel(factor(KG_class_1_name), "Polar")]
levels(prec_mask$prec_quant_dataset_agreement ) <- c("High", "Above average", "Average", "Below average", "Low")
land_cover_class <- merge(prec_mask[, .(lat, lon, prec_quant_dataset_agreement , land_cover_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
biome_class <- merge(prec_mask[, .(lat, lon, prec_quant_dataset_agreement , biome_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
elevation_class <- merge(prec_mask[, .(lat, lon, prec_quant_dataset_agreement , elev_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
prec_quant <- merge(prec_mask[, .(lat, lon, prec_quant_dataset_agreement , prec_quant, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))

## Analysis
### Land use
land_cover_agreement <- land_cover_class[, .(prec_sum = sum(prec_volume_year)), .(prec_quant_dataset_agreement , land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(prec_quant_dataset_agreement , land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(prec_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := prec_sum / land_cover_sum]

### Biome types
biome_agreement <- biome_class[, .(prec_sum = sum(prec_volume_year)), .(prec_quant_dataset_agreement , biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(prec_quant_dataset_agreement , biome_short_class), ]
biome_agreement[, biome_sum := sum(prec_sum), biome_short_class]
biome_agreement[, biome_fraction := prec_sum / biome_sum]

### Elevation
elevation_agreement <- elevation_class[, .(prec_sum = sum(prec_volume_year)), .(prec_quant_dataset_agreement , elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(prec_quant_dataset_agreement , elev_class), ]
elevation_agreement[, elev_sum := sum(prec_sum), elev_class]
elevation_agreement[, elev_fraction := prec_sum / elev_sum]

### Precipitation quantiles
prec_quant_agreement <- prec_quant[, .(prec_sum = sum(prec_volume_year)), .(prec_quant_dataset_agreement , prec_quant)]
prec_quant_agreement <- prec_quant_agreement[complete.cases(prec_quant_agreement)]
prec_quant_agreement <- prec_quant_agreement[order(prec_quant_dataset_agreement , prec_quant), ]
prec_quant_agreement[, prec_quant_sum := sum(prec_sum), prec_quant]
prec_quant_agreement[, prec_quant_fraction := prec_sum / prec_quant_sum]

## Figures
### Land Use
land_cover_agreement$land_cover_short_class <- factor(land_cover_agreement$land_cover_short_class, 
                                                          levels = c("Forests", "Savannas", "Croplands", "Shrublands", "Grasslands", "Water", "Barren", "Snow/Ice", "Other"))
fig_land_cover_partition_fraction <- ggplot(land_cover_agreement[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = prec_quant_dataset_agreement ), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Biomes
biome_agreement$biome_short_class <- factor(biome_agreement$biome_short_class, 
                                                  levels = c("T/S Forests", "T/S Grasslands", "T. Forests", 
                                                             "B. Forests", "T. Grasslands", "Deserts", "Tundra", 
                                                             "Flooded", "M. Grasslands", "Mediterranean", NA))
fig_biome_partition_fraction <- ggplot(biome_agreement) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = prec_quant_dataset_agreement ), stat = "identity") +
  xlab('Biome class')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Elevation
fig_elevation_partition_fraction <- ggplot(elevation_agreement) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = prec_quant_dataset_agreement ), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Precipitation classes
fig_prec_partition_fraction <- ggplot(prec_quant_agreement) +
  geom_bar(aes(x = prec_quant, y = prec_quant_fraction, fill = prec_quant_dataset_agreement ), stat = "identity") +
  xlab('Precipitation quantile')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Figure 2
gg_fig_2 <- ggarrange(fig_land_cover_partition_fraction,fig_biome_partition_fraction,
                      fig_elevation_partition_fraction, fig_prec_partition_fraction,
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 1, ncol = 4)
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "partition_fraction_agreement.png"), width = 10, height = 10)

