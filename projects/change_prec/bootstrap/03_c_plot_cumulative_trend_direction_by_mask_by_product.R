# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# plot cumulative trend direction for each mask and product ----
source('source/changing_prec.R')

## Data ----
# Input Data generated in changing_prec/bootstrap/02_a
biome_trends <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_a_biome_cumulative_trend_direction_per_dataset_bootstrap.rds"))
ipcc_trends <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_a_ipcc_cumulative_trend_direction_per_dataset_bootstrap.rds"))
land_trends <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_a_land_cover_cumulative_trend_direction_per_dataset_bootstrap.rds"))
elev_trends <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_a_elevation_cumulative_trend_direction_per_dataset_bootstrap.rds"))
KG_class_3_trends <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_a_KG_3_cumulative_trend_direction_per_dataset_bootstrap.rds"))

### Biomes ----
ggplot(biome_trends) +
  geom_bar(aes(x = dataset, y = biome_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~biome_short_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_c_bar_biome_cumulative_fraction_prec_slope_direction_per_dataset_bootstrap.png"), 
       width = 8, height = 12)

### IPCC ----
ggplot(ipcc_trends) +
  geom_bar(aes(x = dataset, y = ipcc_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~IPCC_ref_region, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_c_bar_ipcc_cumulative_fraction_prec_slope_direction_per_dataset_bootstrap.png"), 
       width = 12, height = 16)

### Land cover ----
ggplot(land_trends) +
  geom_bar(aes(x = dataset, y = land_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~land_cover_short_class, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_c_bar_land_cover_cumulative_fraction_prec_slope_direction_per_dataset_bootstrap.png"), 
       width = 8, height = 6)

### Elevation class ----
ggplot(elev_trends) +
  geom_bar(aes(x = dataset, y = elev_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~elev_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_c_bar_elevation_cumulative_fraction_prec_slope_direction_per_dataset_bootstrap.png"), 
       width = 8, height = 12)

### Koeppen-Geiger ----
ggplot(KG_class_3_trends) +
  geom_bar(aes(x = dataset, y = KG_class_3_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~KG_class_3, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_c_bar_KG_3_cumulative_fraction_prec_slope_direction_per_dataset_bootstrap.png"), 
       width = 12, height = 16)
