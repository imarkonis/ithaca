# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask[, unique(rel_dataset_agreement)], prec_mask[, unique(land_use_short_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'land_use_short_class')

### Weighted Average
prec_weights <- prec_mask[, .(lon, lat)] %>% spatial_weight()
prec_mask <- merge(prec_mask, prec_weights, by = c("lon", "lat"))
prec_mask <- prec_mask[, prec_weight := prec_mean * weight, by = .(lon, lat)
][, weight := NULL]

## Analysis
land_use_short_class <- prec_mask[, .(sum_land_use_short_class = sum(prec_weight)), 
                        .(land_use_short_class, rel_dataset_agreement)]
land_use_short_class <-  merge(land_use_short_class, needed_for_cumsum, by = c('land_use_short_class', "rel_dataset_agreement"), all.y = TRUE)
land_use_short_class <- land_use_short_class[order(land_use_short_class, rel_dataset_agreement), ]
land_use_short_class[is.na(sum_land_use_short_class), sum_land_use_short_class := 0]

land_use_short_class_cum <- land_use_short_class[, .(cumsum_land_use_short_class = cumsum(sum_land_use_short_class), 
                                 rel_dataset_agreement), .(land_use_short_class)]
land_use_short_class_cum[, fraction_bias := cumsum_land_use_short_class  / sum(cumsum_land_use_short_class), rel_dataset_agreement]
land_use_short_class_cum <- land_use_short_class_cum[complete.cases(land_use_short_class_cum)]

land_use_short_class_prec <- prec_mask[, .(lon, lat, prec_mean, land_use_short_class, rel_dataset_agreement)]
land_use_short_class_prec <- land_use_short_class_prec[complete.cases(land_use_short_class_prec)]

## Figures
ggplot(land_use_short_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = land_use_short_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Land cover class')  +
  scale_fill_manual(values = colset_land_use_short) +
  theme_light()

ggplot(land_use_short_class_prec) + #Change this to mean/sd point plot
  geom_boxplot(aes(y = prec_mean, fill = rel_dataset_agreement), outlier.shape = NA) +
  facet_wrap(~ land_use_short_class, scales = 'free') +
  labs(fill = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_fill_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

