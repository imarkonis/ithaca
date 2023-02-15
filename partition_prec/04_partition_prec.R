# Partition precipitation to different regional properties and quantify their uncertainty
source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

library(ggthemes)
library(scales)

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.rds"))

## Variables
prec_mask[, KG_class_1_name := relevel(factor(KG_class_1_name), "Polar")]
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

climate_KG <- merge(prec_mask[, .(lat, lon, KG_class_1_name)], prec_grid[, .(lon, lat, area)], by = c("lon", "lat"))
datasets_KG <- merge(climate_KG, prec_datasets, by = c("lon", "lat"))
datasets_KG[, prec_volume_year := 12 * area * 10 ^ (-9) * prec_mean * 0.001][, prec_mean := NULL] # km3

land_use_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, land_use_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
biome_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, biome_short_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
elev_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, elev_class, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
prec_quant <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, prec_quant, KG_class_1_name)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))

## Analysis
### Climate
datasets_KG[, .(area = round(sum(area), 2)), .(dataset, dataset_type)] #Antarctica 13.66 million km2
dataset_partition_KG <- datasets_KG[, .(prec_sum = round(sum(prec_volume_year), 0)), .(KG_class_1_name, dataset, dataset_type)]
dataset_partition_KG[(dataset == 'cmorph' |                       #Remove as they do not cover the whole planet
                        dataset == 'persiann' | 
                        dataset == 'chirps') & 
                       (KG_class_1_name == 'Polar' | KG_class_1_name == 'Continental'), prec_sum := NA]
#### Mean
partition_KG_global <- dcast(dataset_partition_KG, . ~ KG_class_1_name, 
                                     fun = mean, na.rm = TRUE)
colnames(partition_KG_global)[1] <- "Source"
partition_KG_dataset_types <- dcast(dataset_partition_KG, dataset_type ~ KG_class_1_name, 
                                           fun = mean, na.rm = TRUE)
colnames(partition_KG_dataset_types)[1] <- "Source"
partition_KG <- rbind(partition_KG_global, partition_KG_dataset_types)
partition_KG$Sum <- apply(partition_KG[, 2:6], 1, sum)
partition_KG[, Source := c("Global", "Ground Stations", "Reanalysis", "Remote Sensing")]

partition_KG_datasets <- dcast(dataset_partition_KG, dataset ~ KG_class_1_name, fun = mean, na.rm = TRUE)
partition_KG_datasets <- merge(prec_datasets[, .(dataset = unique(dataset)), dataset_type], partition_KG_datasets, by = 'dataset')
colnames(partition_KG_datasets)[1] <- c("Dataset")
partition_KG_datasets[, Sum := rowSums(.SD), .SDcols = 3:7]
partition_KG <- cbind(partition_KG[, 1], apply(partition_KG[, 2:7], 2, round, 0))

#### St. Dev
partition_KG_global_sd <- dcast(dataset_partition_KG, . ~ KG_class_1_name, 
                                     fun = sd, na.rm = TRUE)
colnames(partition_KG_global_sd)[1] <- "Source"
partition_KG_dataset_types_sd <- dcast(dataset_partition_KG, dataset_type ~ KG_class_1_name, 
                                           fun = sd, na.rm = TRUE)
colnames(partition_KG_dataset_types_sd)[1] <- "Source"
partition_KG_sd <- rbind(partition_KG_global_sd, partition_KG_dataset_types_sd)
partition_KG_sd[, Source := c("Global", "Ground Stations", "Reanalysis", "Remote Sensing")]
partition_KG_sd$Sum <- partition_KG_datasets[, sd(Sum, na.rm = TRUE)]
partition_KG_sd$Sum[2:4] <- partition_KG_datasets[, sd(Sum, na.rm = TRUE), dataset_type]$V1[c(2, 3, 1)]
partition_KG_sd <- cbind(partition_KG_sd[, 1], apply(partition_KG_sd[, 2:7], 2, round, 0))

### Land use
dataset_agreement_land_use <- land_use_class[, .N, .(rel_dataset_agreement, land_use_short_class)]
dataset_agreement_land_use <- dataset_agreement_land_use[complete.cases(dataset_agreement_land_use)]
dataset_agreement_land_use <- dataset_agreement_land_use[order(rel_dataset_agreement, land_use_short_class), ]
dataset_agreement_land_use[, land_use_sum := sum(N), land_use_short_class]
dataset_agreement_land_use[, land_use_fraction := N/land_use_sum]

dataset_agreement_land_use_prec <- land_use_class[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, land_use_short_class)]
dataset_agreement_land_use_prec <- dataset_agreement_land_use_prec[complete.cases(dataset_agreement_land_use_prec)]
dataset_agreement_land_use_prec <- dataset_agreement_land_use_prec[order(KG_class_1_name, land_use_short_class), ]

### Biome types
dataset_agreement_biome <- biome_class[, .N, .(rel_dataset_agreement, biome_short_class)]
dataset_agreement_biome <- dataset_agreement_biome[complete.cases(dataset_agreement_biome)]
dataset_agreement_biome <- dataset_agreement_biome[order(rel_dataset_agreement, biome_short_class), ]
dataset_agreement_biome[, biome_sum := sum(N), biome_short_class]
dataset_agreement_biome[, biome_fraction := N/biome_sum]
dataset_agreement_biome <- dataset_agreement_biome[order(biome_sum, rel_dataset_agreement), ]

dataset_agreement_biome_prec <- biome_class[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, biome_short_class)]
dataset_agreement_biome_prec <- dataset_agreement_biome_prec[complete.cases(dataset_agreement_biome_prec)]
dataset_agreement_biome_prec <- dataset_agreement_biome_prec[order(KG_class_1_name, biome_short_class), ]

### Elevation
dataset_agreement_elevation <- elev_class[, .N, .(rel_dataset_agreement, elev_class)]
dataset_agreement_elevation <- dataset_agreement_elevation[complete.cases(dataset_agreement_elevation)]
dataset_agreement_elevation <- dataset_agreement_elevation[order(rel_dataset_agreement, elev_class), ]
dataset_agreement_elevation[, elev_sum := sum(N), elev_class]
dataset_agreement_elevation[, elev_fraction := N/elev_sum]

dataset_agreement_elevation_prec <- elev_class[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, elev_class)]
dataset_agreement_elevation_prec <- dataset_agreement_elevation_prec[complete.cases(dataset_agreement_elevation_prec)]
dataset_agreement_elevation_prec <- dataset_agreement_elevation_prec[order(KG_class_1_name, elev_class), ]

### Precipitation quantiles
dataset_agreement_prec <- prec_quant[, .N, .(rel_dataset_agreement, prec_quant)]
dataset_agreement_prec <- dataset_agreement_prec[complete.cases(dataset_agreement_prec)]
dataset_agreement_prec <- dataset_agreement_prec[order(rel_dataset_agreement, prec_quant), ]
dataset_agreement_prec[, prec_sum := sum(N), prec_quant]
dataset_agreement_prec[, prec_fraction := N/prec_sum]

dataset_agreement_prec_prec <- prec_quant[, .(prec_sum = sum(prec_volume_year)), .(KG_class_1_name, prec_quant)]
dataset_agreement_prec_prec <- dataset_agreement_prec_prec[complete.cases(dataset_agreement_prec_prec)]
dataset_agreement_prec_prec <- dataset_agreement_prec_prec[order(KG_class_1_name, prec_quant), ]

## Save data
write.csv(partition_KG, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_KG.csv"))
write.csv(partition_KG_sd, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_KG_sd.csv"))
write.csv(partition_KG_datasets[, -2], paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_KG_datasets.csv"))

## Figures Main
### Land Use
fig_land_use_partition_prec_volume <- ggplot(dataset_agreement_land_use_prec[land_use_short_class != "Other"]) +
  geom_bar(aes(x = reorder(land_use_short_class,-(prec_sum)), y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Land cover type')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dataset_agreement_land_use$land_use_short_class <- factor(dataset_agreement_land_use$land_use_short_class, 
                                                          levels = c("Forests", "Savannas", "Croplands", "Shrublands", "Grasslands", "Water", "Barren", "Snow/Ice", "Other"))
fig_land_use_partition_fraction <- ggplot(dataset_agreement_land_use[land_use_short_class != "Other"]) +
  geom_bar(aes(x = land_use_short_class, y = land_use_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Elevation
fig_elevation_partition_prec_volume <- ggplot(dataset_agreement_elevation_prec) +
  geom_bar(aes(x = elev_class, y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Elevation [m]')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_elevation_partition_fraction <- ggplot(dataset_agreement_elevation) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Precipitation classes
fig_prec_partition_prec_volume <- ggplot(dataset_agreement_prec_prec) +
  geom_bar(aes(x = prec_quant, y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Precipitation quantile')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_prec_partition_fraction <- ggplot(dataset_agreement_prec) +
  geom_bar(aes(x = prec_quant, y = prec_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Precipitation quantile')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Biomes
fig_biome_partition_prec_volume <- ggplot(dataset_agreement_biome_prec) +
  geom_bar(aes(x = reorder(biome_short_class, -(prec_sum)), y = prec_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Biome class')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dataset_agreement_biome$biome_short_class <- factor(dataset_agreement_biome$biome_short_class, 
                                                    levels= c("(Sub-) Tropical Forests", "(Sub-) Tropical Grasslands", "Temperate Forests", "Boreal Forests", "Temperate Grasslands", "Deserts", "Tundra", "Montane Grasslands", "Flooded & Mangroves", "Mediterranean"))
fig_biome_partition_fraction <- ggplot(dataset_agreement_biome) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Biome class')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Figure 1
gg_fig_1 <- ggarrange(fig_land_use_partition_prec_volume, fig_biome_partition_prec_volume, 
                      fig_elevation_partition_prec_volume, fig_prec_partition_prec_volume, 
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "01_partition_volume_climate.png"), width = 10, height = 10)

### Figure 2
gg_fig_2 <- ggarrange(fig_land_use_partition_fraction,fig_biome_partition_fraction,
                      fig_elevation_partition_fraction, fig_prec_partition_fraction,
                    labels = c('a', 'b', 'c', 'd'),
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "02_partition_fraction_agreement.png"), width = 10, height = 10)

