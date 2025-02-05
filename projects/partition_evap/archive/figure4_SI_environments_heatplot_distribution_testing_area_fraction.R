# Heatplot figures of environment-based distribution testing to support Fig. 4 (and 2), SI support Fig 1. ----
source('source/partition_evap.R')
source('source/graphics.R')

## data ----
landcover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_land_cover.rds"))
summary(landcover$area_fraction)
q10 <- quantile(landcover$area_fraction, c(0.1))
q30 <- quantile(landcover$area_fraction, c(0.3))
q70 <- quantile(landcover$area_fraction, c(0.7))
q90 <- quantile(landcover$area_fraction, c(0.9))

landcover[, area_fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                                labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

ggplot(landcover[land_cover_short_class != "Other"], aes(x = dataset.x, y = dataset.y, fill = area_fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~land_cover_short_class, ncol = 4)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_land_cover_area_fraction.png"), 
       width = 8, height = 4)

biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))
q10 <- quantile(biome$area_fraction, c(0.1))
q30 <- quantile(biome$area_fraction, c(0.3))
q70 <- quantile(biome$area_fraction, c(0.7))
q90 <- quantile(biome$area_fraction, c(0.9))

biome[, area_fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                            labels = c("Low", "Below Average", "Average", "Above Average", "High"))]


ggplot(biome[biome_short_class != "Other"], aes(x = dataset.x, y = dataset.y, fill = area_fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~biome_short_class, labeller = label_wrap_gen(width = 10), ncol = 4)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_biomes_area_fraction.png"), 
       width = 8, height = 8)

ipcc <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_ipcc.rds"))
q10 <- quantile(ipcc$area_fraction, c(0.1))
q30 <- quantile(ipcc$area_fraction, c(0.3))
q70 <- quantile(ipcc$area_fraction, c(0.7))
q90 <- quantile(ipcc$area_fraction, c(0.9))

ipcc[, area_fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

ggplot(ipcc, aes(x = dataset.x, y = dataset.y, fill = area_fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_ipcc_area_fraction.png"), 
       width = 8, height = 12)

elev <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_elev.rds"))
q10 <- quantile(elev$area_fraction, c(0.1))
q30 <- quantile(elev$area_fraction, c(0.3))
q70 <- quantile(elev$area_fraction, c(0.7))
q90 <- quantile(elev$area_fraction, c(0.9))

elev[, area_fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

ggplot(elev, aes(x = dataset.x, y = dataset.y, fill = area_fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~elev_class, labeller = label_wrap_gen(width = 10), ncol = 4)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_elev_area_fraction.png"), 
       width = 8, height = 4)


evap <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_evap.rds"))
q10 <- quantile(evap$area_fraction, c(0.1))
q30 <- quantile(evap$area_fraction, c(0.3))
q70 <- quantile(evap$area_fraction, c(0.7))
q90 <- quantile(evap$area_fraction, c(0.9))

evap[, area_fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

ggplot(evap, aes(x = dataset.x, y = dataset.y, fill = area_fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~evap_quant, labeller = label_wrap_gen(width = 10), ncol = 4)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_evap_area_fraction.png"), 
       width = 8, height = 6)


global <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products.rds"))
q10 <- quantile(global$area_fraction, c(0.1))
q30 <- quantile(global$area_fraction, c(0.3))
q70 <- quantile(global$area_fraction, c(0.7))
q90 <- quantile(global$area_fraction, c(0.9))

global[, area_fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                             labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

summary(global$area_fraction)
ggplot(global, aes(x = dataset.x, y = dataset.y, fill = area_fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_global_area_fraction.png"), 
       width = 4, height = 2.5)
