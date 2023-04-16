# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask[, unique(rel_dataset_agreement)], prec_mask[, unique(elev_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'elev_class')

### Weighted Average
prec_weights <- prec_mask[, .(lon, lat)] %>% spatial_weight()
prec_mask <- merge(prec_mask, prec_weights, by = c("lon", "lat"))
prec_mask <- prec_mask[, prec_weight := prec_mean * weight, by = .(lon, lat)
][, weight := NULL]

## Analysis
dataset_agreement_elevation <- prec_mask[, .N, .(rel_dataset_agreement, elev_class)]
dataset_agreement_elevation <- dataset_agreement_elevation[complete.cases(dataset_agreement_elevation)]
dataset_agreement_elevation <- dataset_agreement_elevation[order(rel_dataset_agreement, elev_class), ]
dataset_agreement_elevation[, elev_sum := sum(N), elev_class]
dataset_agreement_elevation[, elev_fraction := N/elev_sum]


dataset_agreement_elevation_prec <- prec_mask[, .(prec_sum = sum(prec_mean)), .(rel_dataset_agreement, elev_class)]
dataset_agreement_elevation_prec <- dataset_agreement_elevation_prec[complete.cases(dataset_agreement_elevation_prec)]
dataset_agreement_elevation_prec <- dataset_agreement_elevation_prec[order(rel_dataset_agreement, elev_class), ]

elev_class <- prec_mask[, .(sum_elev_class = sum(prec_weight)), 
                      .(elev_class, rel_dataset_agreement)]
elev_class <-  merge(elev_class, needed_for_cumsum, by = c('elev_class', "rel_dataset_agreement"), all.y = TRUE)
elev_class <- elev_class[order(elev_class, rel_dataset_agreement), ]
elev_class[is.na(sum_elev_class), sum_elev_class := 0]
elev_class <- elev_class[!is.na(rel_dataset_agreement)]

elev_class_cum <- elev_class[, .(cumsum_elev_class = cumsum(sum_elev_class), 
                             rel_dataset_agreement), .(elev_class)]
elev_class_cum[, fraction_bias := cumsum_elev_class  / sum(cumsum_elev_class), rel_dataset_agreement]

elev_class_prec <- prec_mask[, .(lon, lat, prec_mean, elev_class, rel_dataset_agreement)]

elev_class_datasets <- merge(prec_mask[, .(lon, lat, elev_class, rel_dataset_agreement)], prec_mean_datasets, by = c("lat", "lon"))
elev_class_datasets <- elev_class_datasets[, .(sum_elev_class = sum(prec_mean)), .(elev_class, rel_dataset_agreement, dataset)]
elev_class_datasets <-  merge(elev_class_datasets, needed_for_cumsum, by = c('elev_class', "rel_dataset_agreement"), all.y = TRUE)
elev_class_datasets <- elev_class_datasets[order(elev_class, rel_dataset_agreement, dataset), ]
elev_class_datasets_cum <- elev_class_datasets[, .(cumsum_elev_class = cumsum(sum_elev_class), 
                                               rel_dataset_agreement), .(elev_class, dataset)]
elev_class_datasets_cum[, fraction_bias := cumsum_elev_class  / sum(cumsum_elev_class), .(rel_dataset_agreement, dataset)]

## Validation
ggplot() +
  geom_raster(data = prec_mask, aes(lon, lat, fill = elev_class)) +
  geom_point(data = prec_mask[rel_dataset_agreement == 'high', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual) +
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  labs(fill = 'Elevation')  +
  theme_light()

ggplot(prec_mask) +
  geom_bar(aes(x = elev_class)) + 
  theme_light()

## Figures
### Main: Partition (%) of total precipitation per elevation for different levels of dataset agreement 
ggplot(elev_class) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Elevation')  +
  ylab('Area fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light()

ggplot(elev_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = elev_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Elevation class')  +
  scale_fill_manual(values = colset_mid[rev(c(10, 8, 6, 5, 4, 1))]) +
  theme_light()

ggplot(elev_class_prec) +
  geom_boxplot(aes(y = prec_mean, fill = rel_dataset_agreement)) +
  facet_wrap(~elev_class, scales = 'free') +
  labs(fill = 'Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_fill_manual(values = colset_mid_qual[c(1, 4, 3, 2, 5)]) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(elev_class_datasets_cum[rel_dataset_agreement == 'high']) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = elev_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Elevation class')  +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(elev_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_OBS]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~elev_class, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'Elevation class')  +
  theme_light()

ggplot(elev_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REANAL]) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = elev_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Elevation class')  +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(elev_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REMOTE]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~elev_class, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'Elevation class')  +
  theme_light()

