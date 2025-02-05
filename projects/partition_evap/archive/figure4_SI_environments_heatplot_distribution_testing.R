# Heatplot figures of environment-based distribution testing to support Fig. 4 (and 2), SI support Fig 1. ----

source('source/partition_evap.R')

## data ----

landcover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_land_cover.rds"))
landcover[KS_test_p < 0.05, agreement := "Not Matching"]
landcover[KS_test_p >= 0.05, agreement := "Matching"]

ggplot(landcover[land_cover_short_class != "Other"], aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = c("Not Matching" = "black","Matching" = "red"))+
  facet_wrap(~land_cover_short_class, ncol = 4)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
  

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_land_cover.png"), 
       width = 8, height = 6)

biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_biomes.rds"))
biome[KS_test_p < 0.05, agreement := "Not Matching"]
biome[KS_test_p >= 0.05, agreement := "Matching"]

ggplot(biome[biome_short_class != "Other"], aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = c("Not Matching" = "black","Matching" = "red"))+
  facet_wrap(~biome_short_class, labeller = label_wrap_gen(width = 10), ncol = 4)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_biomes.png"), 
       width = 8, height = 8)

ipcc <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_ipcc.rds"))
ipcc[KS_test_p < 0.05, agreement := "Not Matching"]
ipcc[KS_test_p >= 0.05, agreement := "Matching"]

ggplot(ipcc, aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = c("Not Matching" = "black","Matching" = "red"))+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_ipcc.png"), 
       width = 8, height = 12)
