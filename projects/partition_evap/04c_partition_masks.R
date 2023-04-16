# Partition precipitation to different regional properties and quantify their uncertainty
source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

library(ggthemes)
library(scales)

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))

## Variables
prec_mask[, KG_class_1_name := relevel(factor(KG_class_1_name), "Polar")]
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

land_cover_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, land_cover_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
biome_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, biome_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
elevation_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, elev_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
prec_quant <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, prec_quant, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))

## Analysis
### Land use
land_cover_prec <- land_cover_class[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, land_cover_short_class)]
land_cover_prec <- land_cover_prec[complete.cases(land_cover_prec)]
land_cover_prec <- land_cover_prec[order(KG_class_1_name, land_cover_short_class), ]

land_cover_agreement <- land_cover_class[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(rel_dataset_agreement, land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(prec_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := prec_sum / land_cover_sum]

### Biome types
biome_prec <- biome_class[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, biome_short_class)]
biome_prec <- biome_prec[complete.cases(biome_prec)]
biome_prec <- biome_prec[order(KG_class_1_name, biome_short_class), ]

biome_agreement <- biome_class[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(rel_dataset_agreement, biome_short_class), ]
biome_agreement[, biome_sum := sum(prec_sum), biome_short_class]
biome_agreement[, biome_fraction := prec_sum / biome_sum]

### Elevation
elevation_prec <- elevation_class[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, elev_class)]
elevation_prec <- elevation_prec[complete.cases(elevation_prec)]
elevation_prec <- elevation_prec[order(KG_class_1_name, elev_class), ]

elevation_agreement <- elevation_class[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(rel_dataset_agreement, elev_class), ]
elevation_agreement[, elev_sum := sum(prec_sum), elev_class]
elevation_agreement[, elev_fraction := prec_sum / elev_sum]

### Precipitation quantiles
prec_quant_prec <- prec_quant[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, prec_quant)]
prec_quant_prec <- prec_quant_prec[complete.cases(prec_quant_prec)]
prec_quant_prec <- prec_quant_prec[order(KG_class_1_name, prec_quant), ]

prec_quant_agreement <- prec_quant[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, prec_quant)]
prec_quant_agreement <- prec_quant_agreement[complete.cases(prec_quant_agreement)]
prec_quant_agreement <- prec_quant_agreement[order(rel_dataset_agreement, prec_quant), ]
prec_quant_agreement[, prec_quant_sum := sum(prec_sum), prec_quant]
prec_quant_agreement[, prec_quant_fraction := prec_sum / prec_quant_sum]

## Save data
save(land_cover_prec, land_cover_agreement, biome_prec, biome_agreement, 
     elevation_prec, elevation_agreement, prec_quant_prec, prec_quant_agreement, 
     file = paste0(PATH_SAVE_PARTITION_PREC, "partition_prec.Rdata"))

## Figures Main
### Land Use
fig_land_cover_partition_prec_volume <- ggplot(land_cover_prec[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = reorder(land_cover_short_class,-(prec_sum)), y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Land cover type')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

land_cover_agreement$land_cover_short_class <- factor(land_cover_agreement$land_cover_short_class, 
                                                          levels = c("Forests", "Savannas", "Croplands", "Shrublands", "Grasslands", "Water", "Barren", "Snow/Ice", "Other"))
fig_land_cover_partition_fraction <- ggplot(land_cover_agreement[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Biomes
fig_biome_partition_prec_volume <- ggplot(biome_prec) +
  geom_bar(aes(x = reorder(biome_short_class, -(prec_sum)), y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Biome class')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

biome_agreement$biome_short_class <- factor(biome_agreement$biome_short_class, 
                                                  levels = c("T/S Forests", "T/S Grasslands", "T. Forests", 
                                                             "B. Forests", "T. Grasslands", "Deserts", "Tundra", 
                                                             "Flooded", "M. Grasslands", "Mediterranean", NA))
fig_biome_partition_fraction <- ggplot(biome_agreement) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Biome class')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Elevation
fig_elevation_partition_prec_volume <- ggplot(elevation_prec) +
  geom_bar(aes(x = elev_class, y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Elevation [m]')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_elevation_partition_fraction <- ggplot(elevation_agreement) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Precipitation classes
fig_prec_partition_prec_volume <- ggplot(prec_quant_prec) +
  geom_bar(aes(x = prec_quant, y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Precipitation quantile')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_prec_partition_fraction <- ggplot(prec_quant_agreement) +
  geom_bar(aes(x = prec_quant, y = prec_quant_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Precipitation quantile')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Figure 1
gg_fig_1 <- ggarrange(fig_land_cover_partition_prec_volume, fig_biome_partition_prec_volume, 
                      fig_elevation_partition_prec_volume, fig_prec_partition_prec_volume, 
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "partition_volume_climate.png"), width = 10, height = 10)

### Figure 2
gg_fig_2 <- ggarrange(fig_land_cover_partition_fraction,fig_biome_partition_fraction,
                      fig_elevation_partition_fraction, fig_prec_partition_fraction,
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "partition_fraction_agreement.png"), width = 10, height = 10)

