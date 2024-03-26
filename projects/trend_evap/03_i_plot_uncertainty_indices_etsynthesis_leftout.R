# plot uncertainty barplots without etsynthesis
source('source/evap_trend.R')

## Data ----
# Input data generated in trend_evap/02_f
land_cover_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_probability_groups_dataset_leftout.rds"))
land_cover_uncertainty <- land_cover_uncertainty[dataset_leftout == "etsynthesis"]
biome_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_probability_groups_dataset_leftout.rds"))
biome_uncertainty <- biome_uncertainty[dataset_leftout == "etsynthesis"]
elev_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elev_probability_groups_dataset_leftout.rds"))
elev_uncertainty <- elev_uncertainty[dataset_leftout == "etsynthesis"]
ipcc_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_probability_groups_dataset_leftout.rds"))
ipcc_uncertainty <- ipcc_uncertainty[dataset_leftout == "etsynthesis"]
KG_3_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_probability_groups_dataset_leftout.rds"))
KG_3_uncertainty <- KG_3_uncertainty[dataset_leftout == "etsynthesis"]

## plots ----
ggplot(land_cover_uncertainty) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = trend), stat = "identity") +
  xlab('Land cover class')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_land_cover_probability_groups_probabilty_no_etsynthesis.png"), 
       width = 12, height = 8)

ggplot(biome_uncertainty) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = trend), stat = "identity") +
  xlab('Biome')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_biome_probability_groups_probabilty_no_etsynthesis.png"), 
       width = 12, height = 8)


ggplot(ipcc_uncertainty) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = trend), stat = "identity") +
  xlab('IPCC reference region')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_ipcc_ref_region_probability_groups_probabilty_no_etsynthesis.png"), 
       width = 12, height = 8)


