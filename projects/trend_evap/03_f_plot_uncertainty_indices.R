# barplots of of DCI and probability groups
source('source/evap_trend.R')

## Data ----

# Input data generated in trend_evap/02_c
land_cover_uncertainty <- readRDS( paste0(PATH_SAVE_EVAP_TREND, "land_cover_uncertainty.rds"))
biome_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biomes_uncertainty.rds"))
elev_uncertainty <- readRDS( paste0(PATH_SAVE_EVAP_TREND, "elevation_uncertainty.rds"))
ipcc_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_uncertainty.rds"))
KG_3_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_uncertainty.rds"))

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

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_land_cover_uncertainty_probabilty.png"), 
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

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_biome_uncertainty_probabilty.png"), 
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
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_ipcc_ref_region_uncertainty_probabilty.png"), 
       width = 12, height = 8)


