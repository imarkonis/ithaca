# Partition precipitation to different KG classes and quantify their uncertainty
library(ggthemes)
library(scales)

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.rds"))

## Variables
koppen_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, KG_class_1_name, land_cover_short_class)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))

## Analysis
dataset_agreement_koppen <- koppen_class[, .N, .(rel_dataset_agreement, KG_class_1_name)]
dataset_agreement_koppen <- dataset_agreement_koppen[complete.cases(dataset_agreement_koppen)]
dataset_agreement_koppen <- dataset_agreement_koppen[order(rel_dataset_agreement, KG_class_1_name), ]
dataset_agreement_koppen[, koppen_sum := sum(N), KG_class_1_name]
dataset_agreement_koppen[, koppen_fraction := N/koppen_sum]

dataset_agreement_koppen_prec <- koppen_class[, .(prec_sum = sum(prec_volume_year)), .(land_cover_short_class, KG_class_1_name)]
dataset_agreement_koppen_prec <- dataset_agreement_koppen_prec[complete.cases(dataset_agreement_koppen_prec)]
dataset_agreement_koppen_prec <- dataset_agreement_koppen_prec[order(land_cover_short_class, KG_class_1_name), ]

## Figures Supplementary
levels(dataset_agreement_koppen$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")

fig_koppen_partition_prec_volume <- ggplot(dataset_agreement_koppen_prec) +
  geom_bar(aes(x = reorder(KG_class_1_name, -(prec_sum)), y = prec_sum, fill = land_cover_short_class), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Koppen-Geiger climate type')  +
  ylab(bquote('Precipitation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_land_cover_short) +
  theme_light()

dataset_agreement_koppen$KG_class_1_name <- factor(dataset_agreement_koppen$KG_class_1_name, 
                                                   levels = c("Tropical", "Temperate", "Continental", "Dry", "Polar"))

fig_koppen_partition_fraction <- ggplot(dataset_agreement_koppen) +
  geom_bar(aes(x = KG_class_1_name, y = koppen_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Koppen-Geiger climate type')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light()

### Figure 1
gg_fig_KG <- ggarrange(fig_koppen_partition_prec_volume, fig_koppen_partition_fraction, 
                      labels = c('a', 'b'),
                      legend = 'right', 
                      nrow = 1, ncol = 2)

jpeg(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/KG_partition.png"), 
     width = 12, height = 6, units = 'in', res = 300)
gg_fig_KG
dev.off()

