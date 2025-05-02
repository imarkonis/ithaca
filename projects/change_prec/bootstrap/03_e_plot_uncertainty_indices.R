# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# barplots of probability groups
source('source/changing_prec.R')

## Data ----

# Input data generated in changing_prec/bootstrap/02_b
land_cover_uncertainty <- readRDS( paste0(PATH_SAVE_CHANGING_PREC, "02_b_land_cover_uncertainty_bootstrap.rds"))
biome_uncertainty <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_biomes_uncertainty_bootstrap.rds"))
elev_uncertainty <- readRDS( paste0(PATH_SAVE_CHANGING_PREC, "02_b_elevation_uncertainty_bootstrap.rds"))
ipcc_uncertainty <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_ipcc_uncertainty_bootstrap.rds"))
KG_3_uncertainty <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_KG_3_uncertainty_bootstrap.rds"))

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

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_e_bar_land_cover_uncertainty_probabilty_bootstrap.png"), 
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

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_e_bar_biome_uncertainty_probabilty_bootstrap.png"), 
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
ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_e_bar_ipcc_ref_region_uncertainty_probabilty_bootstrap.png"), 
       width = 12, height = 8)


