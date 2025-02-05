# Plot mean slopes over masks ----
source('source/evap_trend.R')

## read data ----
### Input data generated in 02_b

land_cover_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_mean_slope.rds"))
biome_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biomes_mean_slope.rds"))
elev_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_mean_slope.rds"))
ipcc_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_mean_slope.rds"))
KG_class_3_global <- readRDS( paste0(PATH_SAVE_EVAP_TREND, "KG_3_mean_slope.rds"))

## plots ----

### Landcover ----
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~land_cover_short_class, scales = 'free', nrow = 1) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_land_cover_mean_slope_mm_year_year.png"), 
       width = 12, height = 8)

ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = evap_trend_percent)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [%/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~land_cover_short_class, scales = 'free', nrow = 1) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_land_cover_mean_slope_percent_year_year.png"), 
       width = 12, height = 8)

### biomes ----
ggplot(biome_class_global, aes(x = biome_short_class, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~biome_short_class, scales = 'free', nrow = 2) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_biome_mean_slope_mm_year_year.png"), 
       width = 12, height = 8)

ggplot(biome_class_global, aes(x = biome_short_class, y = evap_trend_percent)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [%%/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~biome_short_class, scales = 'free', nrow = 2) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_biome_mean_slope_percent_year_year.png"), 
       width = 12, height = 8)

### elevation ----
ggplot(elev_class_global, aes(x = elev_class, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~elev_class, scales = 'free', nrow = 1) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_elevation_mean_slope_mm_year_year.png"), 
       width = 8, height = 8)

ggplot(elev_class_global, aes(x = elev_class, y = evap_trend_percent)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [%/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~elev_class, scales = 'free', nrow = 1) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_elevation_mean_slope_percent_year_year.png"), 
       width = 8, height = 8)

### ipcc ----
ipcc_class_global[IPCC_ref_region %in% IPCC_Africa , continent := 'Africa'
][IPCC_ref_region %in% IPCC_Asia , continent := 'Asia'
][IPCC_ref_region %in% IPCC_Australasia , continent := 'Australasia'
][IPCC_ref_region %in% IPCC_Europe , continent := 'Europe'
][IPCC_ref_region %in% IPCC_Namerica , continent := 'Namerica'
][IPCC_ref_region %in% IPCC_Samerica , continent := 'Samerica']

ggplot(ipcc_class_global, aes(x = IPCC_ref_region, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 2) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~IPCC_ref_region, scales = 'free', nrow = 5) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_ipcc_mean_slope_mm_year_year.png"), 
       width = 12, height = 12)

ggplot(ipcc_class_global, aes(x = IPCC_ref_region, y = evap_trend_percent)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 2) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [%/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~IPCC_ref_region, scales = 'free', nrow = 5) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_ipcc_mean_slope_percent_year_year.png"), 
       width = 12, height = 12)

### Koeppen-Geiger
ggplot(KG_class_3_global, aes(x = KG_class_3, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 2) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~KG_class_3, scales = 'free', nrow = 5) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_KG_class_3_mean_slope_mm_year_year.png"), 
       width = 12, height = 12)

ggplot(KG_class_3_global, aes(x = KG_class_3, y = evap_trend_percent)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 2) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [%/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~KG_class_3, scales = 'free', nrow = 5) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_KG_class_3_mean_slope_percent_year_year.png"), 
       width = 12, height = 12)

