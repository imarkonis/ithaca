# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/uncertainty_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_masks.rds"))
prec_mean_datasets <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_mean_datasets.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask[, unique(rel_dataset_agreement)], prec_mask[, unique(land_use_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'land_use_class')

### Weighted Average
prec_weights <- prec_mask[, .(lon, lat)] %>% spatial_weight()
prec_mask <- merge(prec_mask, prec_weights, by = c("lon", "lat"))
prec_mask <- prec_mask[, prec_weight := prec_mean * weight, by = .(lon, lat)
][, weight := NULL]

land_use_class_datasets <- merge(prec_mask[, .(lon, lat, land_use_class, rel_dataset_agreement)], prec_mean_datasets, by = c("lat", "lon"))
land_use_class_datasets <- land_use_class_datasets[, .(sum_land_use_class = sum(prec_mean)), .(land_use_class, rel_dataset_agreement, dataset)]
land_use_class_datasets <-  merge(land_use_class_datasets, needed_for_cumsum, by = c('land_use_class', "rel_dataset_agreement"), all.y = TRUE)
land_use_class_datasets <- land_use_class_datasets[order(land_use_class, rel_dataset_agreement, dataset), ]
land_use_class_datasets_cum <- land_use_class_datasets[, .(cumsum_land_use_class = cumsum(sum_land_use_class), 
                                                           rel_dataset_agreement), .(land_use_class, dataset)]
land_use_class_datasets_cum[, fraction_bias := cumsum_land_use_class  / sum(cumsum_land_use_class), .(rel_dataset_agreement, dataset)]

## Figures
ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'high']) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = land_use_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_OBS]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~land_use_class, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'KG class')  +
  theme_light()

ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REANAL]) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = land_use_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REMOTE]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~land_use_class, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'KG class')  +
  theme_light()

