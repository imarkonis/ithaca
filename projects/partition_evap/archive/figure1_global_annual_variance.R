# Boxplot of global interannual variance between products ----
source('source/partition_evap.R')

## Data ----
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))
evap_annual_vol[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
evap_annual_vol[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
evap_annual_vol[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydrological model"]
evap_annual_vol[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

evap_annual_vol <- evap_annual_vol[!(dataset == "etmonitor" & year == 2000), ]

## Figure ----


ggplot(evap_annual_vol, aes(x = 0, y = evap_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Global annual volume [km'^3,']')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig1_evap_datasets_global_annual_volume.png"), 
       width = 8, height = 8)


ggplot(evap_annual_vol, aes(x = 0, y = evap_annual_mean)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Global annual evapotranspiration [mm]')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig1_evap_datasets_global_annual_mm.png"), 
       width = 8, height = 8)

