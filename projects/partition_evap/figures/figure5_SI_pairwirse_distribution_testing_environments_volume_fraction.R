# Heatplot figures of environment-based distribution testing to support Fig. 4 (and 2), SI support Fig 1. ----
source('source/partition_evap.R')
source('source/graphics.R')

## landcover ----
landcover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_land_cover.rds"))
summary(landcover$volume_fraction)
q10 <- quantile(landcover$volume_fraction, c(0.1))
q30 <- quantile(landcover$volume_fraction, c(0.3))
q70 <- quantile(landcover$volume_fraction, c(0.7))
q90 <- quantile(landcover$volume_fraction, c(0.9))

landcover[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                                labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

ggplot(landcover[land_cover_short_class != "Other"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~land_cover_short_class, ncol = 4)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_land_cover_volume_fraction.png"), 
       width = 8, height = 4)

## biome ----

biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))
q10 <- quantile(biome$volume_fraction, c(0.1))
q30 <- quantile(biome$volume_fraction, c(0.3))
q70 <- quantile(biome$volume_fraction, c(0.7))
q90 <- quantile(biome$volume_fraction, c(0.9))

biome[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                            labels = c("Low", "Below Average", "Average", "Above Average", "High"))]


ggplot(biome[biome_short_class != "Other"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~biome_short_class, labeller = label_wrap_gen(width = 10), ncol = 4)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_biomes_volume_fraction.png"), 
       width = 8, height = 8)

## ipcc ----

ipcc <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_ipcc.rds"))
q10 <- quantile(ipcc$volume_fraction, c(0.1))
q30 <- quantile(ipcc$volume_fraction, c(0.3))
q70 <- quantile(ipcc$volume_fraction, c(0.7))
q90 <- quantile(ipcc$volume_fraction, c(0.9))

ipcc[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

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

ipcc_africa <- ggplot(ipcc[region == "Africa"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")+
  ggtitle(label = "IPCC Africa")


ipcc_asia <- ggplot(ipcc[region == "Asia"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")+
  ggtitle(label = "IPCC Asia")

ipcc_australasia <- ggplot(ipcc[region == "Australasia"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")+
  ggtitle(label = "IPCC Australasia")

ipcc_europe <- ggplot(ipcc[region == "Europe"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")+
  ggtitle(label = "IPCC Europe")

ipcc_namerica <- ggplot(ipcc[region == "North America"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")+
  ggtitle(label = "IPCC North America")

ipcc_samerica <- ggplot(ipcc[region == "South America"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~IPCC_ref_region, labeller = label_wrap_gen(width = 10), ncol = 5)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "bottom")+
  ggtitle(label = "IPCC South America")


ggarrange(ipcc_africa, ipcc_asia, ipcc_australasia,
          nrow = 3, common.legend = T, heights = c(2.1,2.8,1.4))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_ipcc_volume_fraction_a.png"), 
       width = 8, height = 12)

ggarrange(ipcc_europe, ipcc_namerica, ipcc_samerica,
          nrow = 3, common.legend = T, heights = c(1.5,2.1,2.1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_ipcc_volume_fraction_b.png"), 
       width = 8, height = 11)

## elevation ----

elev <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_elev.rds"))
q10 <- quantile(elev$volume_fraction, c(0.1))
q30 <- quantile(elev$volume_fraction, c(0.3))
q70 <- quantile(elev$volume_fraction, c(0.7))
q90 <- quantile(elev$volume_fraction, c(0.9))

elev[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

ggplot(elev, aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~elev_class, labeller = label_wrap_gen(width = 10), ncol = 4)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_elev_volume_fraction.png"), 
       width = 8, height = 4)

## evap ----

evap <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_evap.rds"))
q10 <- quantile(evap$volume_fraction, c(0.1))
q30 <- quantile(evap$volume_fraction, c(0.3))
q70 <- quantile(evap$volume_fraction, c(0.7))
q90 <- quantile(evap$volume_fraction, c(0.9))

evap[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                           labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

ggplot(evap, aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  facet_wrap(~evap_quant, labeller = label_wrap_gen(width = 10), ncol = 4)+
  labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_evap_volume_fraction.png"), 
       width = 8, height = 6)


# ## global ----
# 
# global <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products.rds"))
# q10 <- quantile(global$volume_fraction, c(0.1))
# q30 <- quantile(global$volume_fraction, c(0.3))
# q70 <- quantile(global$volume_fraction, c(0.7))
# q90 <- quantile(global$volume_fraction, c(0.9))
# 
# global[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
#                              labels = c("Low", "Below Average", "Average", "Above Average", "High"))]
# 
# summary(global$volume_fraction)
# ggplot(global, aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
#   geom_tile(color = "white",lwd = 0.8,linetype = 1) +
#   scale_fill_manual(values = colset_RdBu_5)+
#   labs(fill = "Distribution   \nagreement   ", x = "", y = "")+
#   theme_bw()+
#   theme(text = element_text(size = 10),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         plot.title = element_text(hjust = 0.5))+
#   theme(strip.background = element_rect(fill = "white"))+
#   theme(strip.text = element_text(colour = 'black'), legend.position = "right")
# ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_heatmap_dist_match_global_volume_fraction.png"), 
#        width = 4, height = 2.5)
