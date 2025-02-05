# barplots of probability groups considering only grids where all datasets are present ####
source('source/evap_trend.R')

## Data ----
# Input data generated in trend_evap/02_f
land_cover_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_probability_groups_dataset_leftout_N14.rds"))
biome_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_probability_groups_dataset_leftout_N14.rds"))
elev_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elev_probability_groups_dataset_leftout_N14.rds"))
ipcc_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_probability_groups_dataset_leftout_N14.rds"))
KG_3_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_probability_groups_dataset_leftout_N14.rds"))
global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_stats_probability_group_contribution_dataset_leftout.rds"))

global[, rank_data := rank(diff_area_fraction), .(trend)]
global[trend == "uncertain" & diff_area_fraction > 0.02, dataset := "opposing"]
global[trend == "positive likely" & diff_area_fraction > 0.0, dataset := "positive signal"]
global[trend == "positive likely" & diff_area_fraction < 0.0, dataset := "negative signal"]
global[, diff_sum := sum(diff_area_fraction), dataset_leftout]
## plots ---- 

ggplot(global) +
  geom_bar(aes(x = 1, y = diff_area_fraction*100, fill = trend), stat = "identity", position = "dodge") +
  xlab('Trend group')  +
  ylab('Change in area fraction due to dataset [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~dataset_leftout, ncol = 2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid")
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_global_probability_groups_probabilty_per_dataset_leftout.png"), 
       width = 12, height = 8)


ggplot(land_cover_uncertainty) +
  geom_bar(aes(x = land_cover_short_class, y = -diff_area_fraction*100, fill = trend), stat = "identity", position = "dodge") +
  xlab('Land cover class')  +
  ylab('Change in total area fraction due to dataset [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~dataset_leftout, ncol = 2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid")
        )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_land_cover_probability_groups_probabilty_per_dataset_leftout.png"), 
       width = 12, height = 8)

ggplot(biome_uncertainty) +
  geom_bar(aes(x = biome_short_class, y = -diff_area_fraction*100, fill = trend), stat = "identity", position = "dodge") +
  xlab('Biome')  +
  ylab('Change in total area fraction due to dataset [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~dataset_leftout, ncol = 2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid")
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_biome_probability_groups_probabilty_per_dataset_leftout.png"), 
       width = 12, height = 8)


ggplot(ipcc_uncertainty) +
  geom_bar(aes(x = IPCC_ref_region, y = -diff_area_fraction*100, fill = trend), stat = "identity", position = "dodge") +
  xlab('IPCC reference region')  +
  ylab('Change in total area fraction due to dataset [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~dataset_leftout, ncol = 1, scale = "free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid")
  )
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_ipcc_ref_region_probability_groups_probabilty_per_dataset_leftout.png"), 
       width = 12, height = 14)


ggplot(elev_uncertainty) +
  geom_bar(aes(x = elev_class, y = -diff_area_fraction*100, fill = trend), stat = "identity", position = "dodge") +
  xlab('Elevation')  +
  ylab('Change in total area fraction due to dataset [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~dataset_leftout, ncol = 4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid")
  )
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_elevation_probability_groups_probabilty_per_dataset_leftout.png"), 
       width = 8, height = 6)

