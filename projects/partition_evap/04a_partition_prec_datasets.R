# Partition precipitation to different regional properties and quantify their uncertainty
install.packages("ggstatsplot")

source('source/partition_prec.R')
source('source/graphics.R')

library(ggthemes)
library(scales)
library(ggstatsplot)

## Data 
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))

## Analysis
prec_datasets_volume <- merge(prec_datasets[, .(lon, lat, year, dataset, prec)], 
                            prec_grid[, .(lon, lat, area)], 
                            by = c("lon", "lat"), all = TRUE)
prec_datasets_volume[, prec_volume := area  * M2_TO_KM2 * prec * MM_TO_KM
                     ][, prec := NULL
                       ][, area := NULL]     

land_cover_class <- merge(prec_mask[, .(lat, lon, land_cover_short_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, dataset)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(prec_volume = sum(prec_volume)), 
                                     .(dataset, land_cover_short_class, year)]

biome_class <- merge(prec_mask[, .(lat, lon, biome_short_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, dataset)], 
                          by = c("lon", "lat"))
biome_class_global <- biome_class[, .(prec_volume = sum(prec_volume)), 
                                     .(dataset, biome_short_class, year)]

elev_class <- merge(prec_mask[, .(lat, lon, elev_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, dataset)], 
                          by = c("lon", "lat"))
elev_class_global <- elev_class[, .(prec_volume = sum(prec_volume)), 
                                     .(dataset, elev_class, year)]

prec_class <- merge(prec_mask[, .(lat, lon, prec_quant)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, dataset)], 
                          by = c("lon", "lat"))
prec_class_global <- prec_class[, .(prec_volume = sum(prec_volume)), 
                                     .(dataset, prec_quant, year)]

## Plots
### Land use
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = prec_volume )) +
  geom_violin(fill = NA) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "Land cover class") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  facet_wrap(~land_cover_short_class, scales = 'free') +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 

ggplot(biome_class_global, aes(x = biome_short_class, y = prec_volume )) +
  geom_violin(fill = NA) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "Biome type") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  facet_wrap(~biome_short_class, scales = 'free') +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 

ggplot(elev_class_global, aes(x = elev_class, y = prec_volume )) +
  geom_violin(fill = NA) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "Elevation zone") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  facet_wrap(~elev_class, scales = 'free') +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 

ggplot(prec_class_global, aes(x = prec_quant, y = prec_volume )) +
  geom_violin(fill = NA) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "Precipitaiton quantile") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  facet_wrap(~prec_quant, scales = 'free') +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 
