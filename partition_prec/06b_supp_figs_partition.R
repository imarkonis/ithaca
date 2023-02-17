# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
load(paste0(PATH_SAVE_PARTITION_PREC, "partition_prec.Rdata"))

## Variables
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

## Analysis
### Land cover
land_use_short_class_prec <- prec_mask[, .(lon, lat, prec_mean, land_use_short_class, rel_dataset_agreement)]
land_use_short_class_prec <- land_use_short_class_prec[complete.cases(land_use_short_class_prec)]
land_use_short_class_prec <- land_use_short_class_prec[, .(prec_median = median(prec_mean), 
                                                           prec_q25 = quantile(prec_mean, 0.25),
                                                           prec_q75 = quantile(prec_mean, 0.75)), 
                                                       .(land_use_short_class, rel_dataset_agreement)]

land_use_agreement_cum <- land_use_agreement[, .(cumsum_land_use = cumsum(prec_sum), 
                                 rel_dataset_agreement), .(land_use_short_class)]
land_use_agreement_cum[, fraction_bias := cumsum_land_use  / sum(cumsum_land_use), rel_dataset_agreement]
land_use_agreement_cum <- land_use_agreement_cum[complete.cases(land_use_agreement_cum)]

### Biomes

### Elevation

### Precipitation Quantiles

## Figures
ggplot(land_use_short_class_prec) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = prec_q25, ymax = prec_q75, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = prec_median, col = rel_dataset_agreement)) +
  facet_wrap(~ land_use_short_class, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/land_cover_median_prec.png"), width = 8, height = 6)

ggplot(land_use_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = land_use_short_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Land cover class')  +
  scale_fill_manual(values = colset_land_use_short[c(3, 6, 2, 7, 4, 9, 1, 8, 5)]) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/land_cover_agreement_cum.png"), width = 8, height = 6)
