# Partition precipitation to precipitation classes and quantify their uncertainty

source('source/blueprint.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_BLUEPRINT, "prec_masks.rds"))

prec_weights <- prec_mask[, .(lon, lat)] %>% spatial_weight()
prec_mask <- merge(prec_mask, prec_weights, by = c("lon", "lat"))
prec_mask <- prec_mask[, prec_weight := prec_mean * weight, by = .(lon, lat)
                       ][, weight := NULL]

prec_class <- prec_mask[, .(sum_prec_class = sum(prec_weight)), 
                                  .(prec_class, rel_dataset_agreement)]
prec_class <- prec_class[order(prec_class, rel_dataset_agreement), ]

prec_class_cum <- prec_class[, .(cumsum_prec_class = cumsum(sum_prec_class), 
                                           rel_dataset_agreement), .(prec_class)]
prec_class_cum[, fraction_bias := round(cumsum_prec_class / sum(cumsum_prec_class), 2), rel_dataset_agreement]

## Validation
ggplot() +
  geom_raster(data = prec_mask, aes(lon, lat, fill = prec_class)) +
  geom_point(data = prec_mask[rel_dataset_agreement == 'high' | rel_dataset_agreement == 'above average', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4]) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask, aes(lon, lat, fill = prec_class)) +
  geom_point(data = prec_mask[quant_ens_cv  == '0-0.2' | quant_ens_cv  == '0.2-0.4', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4]) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask) +
  geom_bar(aes(x = prec_class)) + 
  theme_light()

## Figures
ggplot(prec_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias, fill = prec_class), stat="identity") +
  xlab('Dataset agreement')  +
  ylab('Fraction of Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5]) +
  theme_light()

ggplot(prec_class) +
  geom_bar(aes(x = rel_dataset_agreement, y = sum_prec_class, fill = prec_class), stat="identity") +
  xlab('Dataset agreement')  +
  ylab('Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  theme_light()

## Needed?

prec_quant_size <- prec_mask[, .(total_grid_cells = .N), prec_class]
prec_quant_size_bias <-  prec_mask[, .N, .(prec_class, rel_dataset_agreement)]
prec_quant_size_bias <- merge(prec_quant_size, prec_quant_size_bias, by = 'prec_class')
prec_quant_size_bias[, fraction := round(N / total_grid_cells, 2)]
prec_quant_size_bias <- prec_quant_size_bias[order(prec_class, rel_dataset_agreement), ]

prec_quant_size_bias_cum <- prec_quant_size_bias[, .(grid_cell_cum = cumsum(N), rel_dataset_agreement), .(prec_class)]
prec_quant_size_bias_cum[, fraction_cv_bias := round(grid_cell_cum / sum(grid_cell_cum), 2), rel_dataset_agreement]