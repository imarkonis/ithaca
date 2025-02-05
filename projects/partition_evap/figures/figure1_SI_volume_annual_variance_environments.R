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



### Evap ----
evap <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_evap_quant.rds"))
evap[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
evap[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
evap[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
evap[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]



## Figure ----

### landcover ----
ggplot(land_cover[land_cover_short_class != "Other" & land_cover_short_class != "Global"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual volume [km'^3,']')))+
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
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_land_cover_vol.png"), 
       width = 8, height = 6)

### biome ----
ggplot(biome[!is.na(biome_short_class) & biome_short_class != "Global"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual volume [km'^3,']')))+
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
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_biomes_vol.png"), 
       width = 8, height = 10)


### ipcc ----

IPCC_Africa <- c("CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("ARP", "EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

ipcc[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
ipcc[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
ipcc[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
ipcc[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
ipcc[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
ipcc[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]


ipcc_Africa <- ggplot(ipcc[region %in% "Africa"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = "")+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset", nrow = 3), lty = guide_legend(title = "Dataset", nrow = 3)) +
  theme_bw() +
  facet_wrap(~IPCC_ref_region, scales = 'free', ncol = 5)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"), 
        plot.margin = unit(c(0,0.1,-0.1,0.8), 'lines'))+
  theme(axis.text.x = element_blank())+
  ggtitle(label = "IPCC Africa")

ipcc_Asia <- ggplot(ipcc[region %in% "Asia"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual volume [km'^3,']')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset", nrow = 3), lty = guide_legend(title = "Dataset", nrow = 3)) +
  theme_bw() +
  facet_wrap(~IPCC_ref_region, scales = 'free', ncol = 5)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"), 
        plot.margin = unit(c(0,0.1,-0.5,0.8), 'lines'))+
  theme(axis.text.x = element_blank())+
  ggtitle(label = "IPCC Asia")


ipcc_Australasia <- ggplot(ipcc[region %in% "Australasia"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = "")+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset", nrow = 3), lty = guide_legend(title = "Dataset", nrow = 3)) +
  theme_bw() +
  facet_wrap(~IPCC_ref_region, scales = 'free', ncol = 5)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"), 
        plot.margin = unit(c(0,0.1,-0.5,0.8), 'lines'))+
  #theme(axis.text.x = element_blank())+
  ggtitle(label = "IPCC Australasia")


ipcc_Europe <- ggplot(ipcc[region %in% "Europe"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = "")+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset", nrow = 3), lty = guide_legend(title = "Dataset", nrow = 3)) +
  theme_bw() +
  facet_wrap(~IPCC_ref_region, scales = 'free', ncol = 5)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"), 
        plot.margin = unit(c(0,0.1,-0.5,0.8), 'lines'))+
  theme(axis.text.x = element_blank())+
  ggtitle(label = "IPCC Europe")

ipcc_Namerica <- ggplot(ipcc[region %in% "North America"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual volume [km'^3,']')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset", nrow = 3), lty = guide_legend(title = "Dataset", nrow = 3)) +
  theme_bw() +
  facet_wrap(~IPCC_ref_region, scales = 'free', ncol = 5)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"), 
        plot.margin = unit(c(0,0.1,-0.5,0.8), 'lines'))+
  theme(axis.text.x = element_blank())+
  ggtitle(label = "IPCC North America")


ipcc_Samerica <- ggplot(ipcc[region %in% "South America"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = "")+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset", nrow = 3), lty = guide_legend(title = "Dataset", nrow = 3)) +
  theme_bw() +
  facet_wrap(~IPCC_ref_region, scales = 'free', ncol = 5)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"), 
        plot.margin = unit(c(0,0.1,-0.5,0.8), 'lines'))+
  #theme(axis.text.x = element_blank())+
  ggtitle(label = "IPCC South America")


ggarrange(ipcc_Africa, ipcc_Asia, ipcc_Australasia,
          nrow = 3, common.legend = T, heights = c(2.1,2.8,1.6), align = "v")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_ipcc_vol_a.png"), 
       width = 8, height = 11)

ggarrange(ipcc_Europe, ipcc_Namerica, ipcc_Samerica,
          nrow = 3, common.legend = T, heights = c(1.4,2.5,3.5), align = "v")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_ipcc_vol_b.png"), 
       width = 8, height = 10)

### elevation ----
ggplot(elevation[elev_class != "Global"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual volume [km'^3,']')))+
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
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_elevation_vol.png"), 
       width = 8, height = 5)

#### evap quantile
ggplot(evap[evap_quant != "Global"], aes(x = 0, y = environment_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Annual volume [km'^3,']')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  facet_wrap(~evap_quant, scales = 'free', ncol = 4)+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "right",        
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_interannual_variance_evap_quant_vol.png"), 
       width = 8, height = 8)
