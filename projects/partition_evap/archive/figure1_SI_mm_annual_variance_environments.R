# Plots of interannual variance of  environments to support figure 1 ----
source('source/partition_evap.R')

## Data ----
### Landcover ----
land_cover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_land_cover.rds"))
land_cover[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
land_cover[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
land_cover[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
land_cover[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

### Biomes ----
biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_biomes.rds"))
biome[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
biome[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
biome[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
biome[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

### IPCC ----
ipcc <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ipcc.rds"))
ipcc[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
ipcc[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
ipcc[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
ipcc[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

### Elevation ----
elevation <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_elevation.rds"))
elevation[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
elevation[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
elevation[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
elevation[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

## Figure ----

ggplot(land_cover[land_cover_short_class != "Other" & land_cover_short_class != "Global"], aes(x = 0, y = evap_mean)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual evapotranspiration [mm]')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  facet_wrap(~land_cover_short_class, scales = 'free', ncol = 4)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_land_cover_mm.png"), 
       width = 8, height = 6)


ggplot(biome[!is.na(biome_short_class) & biome_short_class != "Global"], aes(x = 0, y = evap_mean)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual evapotranspiration [mm]')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  facet_wrap(~biome_short_class, scales = 'free', ncol = 4,
             labeller = label_wrap_gen(width = 10)) +
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_biomes_mm.png"), 
       width = 8, height = 10)

ggplot(ipcc[IPCC_ref_region != "Global"], aes(x = 0, y = evap_mean)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual evapotranspiration [mm]')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  facet_wrap(~IPCC_ref_region, scales = 'free', ncol = 5)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))+
  theme(axis.text.x = element_blank())

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_ipcc_mm.png"), 
       width = 8, height = 12)

ggplot(elevation[elev_class != "Global"], aes(x = 0, y = evap_mean)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual evapotranspiration [mm]')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  facet_wrap(~elev_class, scales = 'free', ncol = 4)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_elevation_mm.png"), 
       width = 8, height = 5)

