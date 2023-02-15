# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask[, unique(rel_dataset_agreement)], prec_mask[, unique(land_use_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'land_use_class')

### Weighted Average
prec_weights <- prec_mask[, .(lon, lat)] %>% spatial_weight()
prec_mask <- merge(prec_mask, prec_weights, by = c("lon", "lat"))
prec_mask <- prec_mask[, prec_weight := prec_mean * weight, by = .(lon, lat)
][, weight := NULL]

## Analysis
land_use_class <- prec_mask[, .(sum_land_use_class = sum(prec_weight)), 
                        .(land_use_class, rel_dataset_agreement)]
land_use_class <-  merge(land_use_class, needed_for_cumsum, by = c('land_use_class', "rel_dataset_agreement"), all.y = TRUE)
land_use_class <- land_use_class[order(land_use_class, rel_dataset_agreement), ]
land_use_class[is.na(sum_land_use_class), sum_land_use_class := 0]

land_use_class_cum <- land_use_class[, .(cumsum_land_use_class = cumsum(sum_land_use_class), 
                                 rel_dataset_agreement), .(land_use_class)]
land_use_class_cum[, fraction_bias := cumsum_land_use_class  / sum(cumsum_land_use_class), rel_dataset_agreement]
land_use_class_cum <- land_use_class_cum[complete.cases(land_use_class_cum)]

land_use_class_prec <- prec_mask[, .(lon, lat, prec_mean, land_use_class, rel_dataset_agreement)]
land_use_class_prec <- land_use_class_prec[complete.cases(land_use_class_prec)]

## Figures
ggplot(land_use_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = land_use_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = c(colset_mid_qual, colset_mid_qual)) +
  theme_light()


ggplot(land_use_class_prec) +
  geom_boxplot(aes(y = prec_mean, fill = rel_dataset_agreement), outlier.shape = NA) +
  facet_wrap(~ land_use_class, scales = 'free') +
  scale_y_continuous(limits = function(x) c(min(x), as.numeric(quantile(x, 0.9)))) +
  labs(fill = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_fill_manual(values = colset_mid_qual[c(1, 4, 3, 2, 5)]) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

