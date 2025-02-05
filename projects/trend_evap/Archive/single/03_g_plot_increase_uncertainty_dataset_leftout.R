# plot increase of uncertainty with dataset leftout
source('source/evap_trend.R')

## read data ----
# Input generated in trend_evap/02_e
land_cover_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_uncertainty_dataset_leftout.rds"))
biome_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_uncertainty_dataset_leftout.rds"))
elev_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elev_uncertainty_dataset_leftout.rds"))
ipcc_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_uncertainty_dataset_leftout.rds"))
KG_3_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_uncertainty_dataset_leftout.rds"))

## plots ----
ggplot(land_cover_uncertainty)+
  geom_bar(aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100)), 
           stat = "identity")+
  labs(y = "Increase of uncertain area [%]", x = "")+
  theme_bw()+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggplot(land_cover_uncertainty)+
  geom_bar(aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100),
               fill = land_cover_short_class), 
           stat = "identity", position = "dodge")+
  labs(y = "Increase of uncertain area [%]", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

a <- ggplot(land_cover_uncertainty, aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100)))+
  geom_boxplot(aes())+
  labs(y = "Increase of uncertain area [%]", x = "Dataset")+
  theme_bw()+  
  ggtitle("Landcover")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_land_cover_increase_uncertainty.png"), 
       width = 8, height = 3)

b <- ggplot(biome_uncertainty, aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100)))+
  geom_boxplot(aes())+
  labs(y = "Increase of uncertain area[%]", x = "Dataset")+
  theme_bw()+  
  ggtitle("Biomes")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_biome_increase_uncertainty.png"), 
       width = 8, height = 3)

c <- ggplot(KG_3_uncertainty, aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100)))+
  geom_boxplot(aes())+
  labs(y = "Increase of uncertain area [%]", x = "Dataset")+
  theme_bw()+  
  ggtitle("Koeppen-Geiger classes")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "box_KG_3_increase_uncertainty.png"), 
       width = 8, height = 3)

ggplot(ipcc_uncertainty, aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100)))+
  geom_boxplot(aes())+
  labs(y = "Increase of uncertain area [%]", x = "Dataset")+
  theme_bw()+  
  scale_color_manual(values = cols_data) + 
  ggtitle("IPCC reference regions")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggplot(ipcc_uncertainty)+
  geom_bar(aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100)), 
           stat = "identity")+
  labs(y = "Increase of uncertain area [%]", x = "")+
  theme_bw()+
  facet_wrap(~IPCC_ref_region, ncol = 1)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggplot(ipcc_uncertainty, aes(x = IPCC_ref_region, y = ((ratio_area_fraction - 1)*100)))+
  geom_point(aes(col = dataset_leftout))+
  labs(y = "Increase of uncertain area [%]", x = "IPCC reference region")+
  theme_bw()+  
  scale_color_manual(values = cols_data) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggplot(ipcc_uncertainty, aes(x = dataset_leftout, y = ((ratio_area_fraction - 1)*100)))+
  geom_boxplot(aes())+
  labs(y = "Increase of uncertain area [%]", x = "Dataset")+
  theme_bw()+  
  scale_color_manual(values = cols_data) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
