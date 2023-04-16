# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/blueprint.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask[, unique(rel_dataset_agreement)], prec_mask[, unique(KG_class_3)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'KG_class_3')

### Weighted Average
prec_weights <- prec_mask[, .(lon, lat)] %>% spatial_weight()
prec_mask <- merge(prec_mask, prec_weights, by = c("lon", "lat"))
prec_mask <- prec_mask[, prec_weight := prec_mean * weight, by = .(lon, lat)
                       ][, weight := NULL]

## Analysis
KG_class <- prec_mask[, .(sum_KG_class = sum(prec_weight)), 
                        .(KG_class_3, rel_dataset_agreement)]
KG_class <-  merge(KG_class, needed_for_cumsum, by = c('KG_class_3', "rel_dataset_agreement"), all.y = TRUE)
KG_class <- KG_class[order(KG_class_3, rel_dataset_agreement), ]
KG_class[is.na(sum_KG_class), sum_KG_class := 0]

KG_class_cum <- KG_class[, .(cumsum_KG_class = cumsum(sum_KG_class), 
                                 rel_dataset_agreement), .(KG_class_3)]
KG_class_cum[, fraction_bias := cumsum_KG_class  / sum(cumsum_KG_class), rel_dataset_agreement]

KG_class_prec <- prec_mask[, .(lon, lat, prec_mean, KG_class_3, rel_dataset_agreement)]

KG_class_datasets <- merge(prec_mask[, .(lon, lat, KG_class_3, rel_dataset_agreement)], prec_mean_datasets, by = c("lat", "lon"))
KG_class_datasets <- KG_class_datasets[, .(sum_KG_class = sum(prec_mean)), .(KG_class_3, rel_dataset_agreement, dataset)]
KG_class_datasets <-  merge(KG_class_datasets, needed_for_cumsum, by = c('KG_class_3', "rel_dataset_agreement"), all.y = TRUE)
KG_class_datasets <- KG_class_datasets[order(KG_class_3, rel_dataset_agreement, dataset), ]
KG_class_datasets_cum <- KG_class_datasets[, .(cumsum_KG_class = cumsum(sum_KG_class), 
                             rel_dataset_agreement), .(KG_class_3, dataset)]
KG_class_datasets_cum[, fraction_bias := cumsum_KG_class  / sum(cumsum_KG_class), .(rel_dataset_agreement, dataset)]

## Validation
ggplot() +
  geom_raster(data = prec_mask, aes(lon, lat, fill = KG_class_3)) +
  geom_point(data = prec_mask[rel_dataset_agreement == 'high', .(lon, lat)],  aes(lon, lat)) +
#  scale_fill_manual(values = colset_mid_qual) +
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask) +
  geom_bar(aes(x = KG_class_3)) + 
  theme_light()

## Figures
### Main: Partition (%) of total precipitation per climatology for different levels of dataset agreement 
ggplot(KG_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = KG_class_3), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
#  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(KG_class_prec) +
  geom_boxplot(aes(y = prec_mean, fill = rel_dataset_agreement)) +
  facet_wrap(~KG_class_3, scales = 'free') +
  labs(fill = 'Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_fill_manual(values = colset_mid_qual[c(1, 4, 3, 2, 5)]) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(KG_class_datasets_cum[rel_dataset_agreement == 'high']) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = KG_class_3), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  theme_light()

ggplot(KG_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_OBS]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~KG_class_3, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'KG class')  +
  theme_light()

ggplot(KG_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REANAL]) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = KG_class_3), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  theme_light()

ggplot(KG_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REMOTE]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~KG_class_3, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'KG class')  +
  theme_light()
