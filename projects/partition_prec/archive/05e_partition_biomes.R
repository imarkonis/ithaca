##NEEDS EDITING ACCORDING TO 05d

source('source/blueprint.R')
source('source/mask_paths.R')
source('source/graphics.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_BLUEPRINT, "prec_masks.rds"))
prec_mean_datasets <- readRDS(paste0(PATH_SAVE_BLUEPRINT, "prec_mean_datasets.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask_biomes[, unique(rel_dataset_agreement)], prec_mask_biomes[, unique(biome_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'biome_class')

## Analysis
biome_class <- prec_mask_biomes[, .(sum_biome_class = sum(prec_mean)), 
                                .(biome_class, rel_dataset_agreement)]
biome_class <-  merge(biome_class, needed_for_cumsum, by = c('biome_class', "rel_dataset_agreement"), all.y = TRUE)
biome_class <- biome_class[order(biome_class, rel_dataset_agreement), ]
biome_class[is.na(sum_biome_class), sum_biome_class := 0]

biome_class_cum <- biome_class[, .(cumsum_biome_class = cumsum(sum_biome_class), 
                                   rel_dataset_agreement), .(biome_class)]
biome_class_cum[, fraction_bias := cumsum_biome_class  / sum(cumsum_biome_class), rel_dataset_agreement]

## Quick validation
ggplot() +
  geom_raster(data = prec_mask_biomes, aes(lon, lat, fill = biome_class)) +
  geom_point(data = prec_mask_biomes[rel_dataset_agreement == 'high', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual[3:7]) +
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask_biomes) +
  geom_bar(aes(x = biome_class)) + 
  theme_light()

## Plot
# Main: Partition (%) of total precipitation per climatology for different levels of dataset agreement 
ggplot(biome_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = biome_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual[2:6]) +
  theme_light()

ggplot(prec_mask_biomes) +
  geom_boxplot(aes(y = prec_mean, fill = rel_dataset_agreement)) +
  facet_wrap(~biome_class, scales = 'free') +
  labs(fill = 'Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_fill_manual(values = colset_mid_qual[c(1, 4, 3, 2, 5)]) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
