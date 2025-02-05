# barplots of of DCI and probability groups
source('source/evap_trend.R')

## Data ----
# Input data generated in trend_evap/02_c
land_cover_uncertainty <- readRDS( paste0(PATH_SAVE_EVAP_TREND, "land_cover_uncertainty.rds"))
biome_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biomes_uncertainty.rds"))
elev_uncertainty <- readRDS( paste0(PATH_SAVE_EVAP_TREND, "elevation_uncertainty.rds"))
KG_3_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_uncertainty.rds"))
ipcc_uncertainty  <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_uncertainty.rds"))

## Analysis
land_cover_uncertainty[, rank_trends := rank(-land_cover_fraction), trend]
biome_uncertainty[, rank_trends := rank(-biome_fraction), trend] 
elev_uncertainty[, rank_trends := rank(-elev_fraction), trend] 
KG_3_uncertainty[, rank_trends := rank(-KG_3_fraction), trend] 
ipcc_uncertainty [, rank_trends := rank(-ipcc_fraction), trend] 

land_cover_uncertainty[, trend := factor(trend, levels = c("positive probable",
                                                           "positive likely",
                                                           "no trend",
                                                           "negative probable",
                                                           "negative likely",
                                                           "uncertain"))]

biome_uncertainty[, trend := factor(trend, levels = c("positive probable",
                                                           "positive likely",
                                                           "no trend",
                                                           "negative probable",
                                                           "negative likely",
                                                           "uncertain"))]

elev_uncertainty[, trend := factor(trend, levels = c("positive probable",
                                                      "positive likely",
                                                      "no trend",
                                                      "negative probable",
                                                      "negative likely",
                                                      "uncertain"))]

ipcc_uncertainty[, trend := factor(trend, levels = c("positive probable",
                                                      "positive likely",
                                                      "no trend",
                                                      "negative probable",
                                                      "negative likely",
                                                      "uncertain"))]

KG_3_uncertainty[, trend := factor(trend, levels = c("positive probable",
                                                      "positive likely",
                                                      "no trend",
                                                      "negative probable",
                                                      "negative likely",
                                                      "uncertain"))]
## plot ---

### landcover ----
positive <- land_cover_uncertainty[rank_trends <= 5 & trend == "positive likely", .(unique(land_cover_short_class), rank_trends)]
positive[, order := sort(positive$rank_trends, index.return = TRUE)$ix]

negative <- land_cover_uncertainty[rank_trends <= 5 & trend == "negative likely", .(unique(land_cover_short_class), rank_trends)]
negative[, order := sort(negative$rank_trends, index.return = TRUE)$ix]

uncertain <- land_cover_uncertainty[rank_trends <= 5 & trend == "uncertain", .(unique(land_cover_short_class), rank_trends)]
uncertain[, order := sort(uncertain$rank_trends, index.return = TRUE)$ix]

pos <- ggplot(land_cover_uncertainty[land_cover_short_class %in% positive$V1
                              & trend %in% c("positive probable", "positive likely")], aes(x = "", y = land_cover_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(land_cover_short_class, levels = c(positive$V1[positive$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))


neg <- ggplot(land_cover_uncertainty[land_cover_short_class %in% negative$V1
                              & trend %in% c("negative likely", "negative probable")], aes(x = "", y = land_cover_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(land_cover_short_class, levels = c(negative$V1[negative$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))

un <- ggplot(land_cover_uncertainty[land_cover_short_class %in% uncertain$V1
                              & trend %in% c("uncertain")], aes(x = "", y = land_cover_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(land_cover_short_class, levels = c(uncertain$V1[uncertain$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  coord_cartesian(ylim = c(0, 100))

gglandcover <- ggarrange(pos, neg, un, nrow = 1,  align = "hv")
a <- annotate_figure(gglandcover, top = text_grob("Landcover classes", 
                                      color = "black", face = "bold", size = 14),
                     fig.lab = "a)", fig.lab.pos = "top.left")

a
### biome----
positive <- biome_uncertainty[rank_trends <= 5 & trend == "positive likely", .(unique(biome_short_class), rank_trends)]
positive[, order := sort(positive$rank_trends, index.return = TRUE)$ix]

negative <- biome_uncertainty[rank_trends <= 5 & trend == "negative likely", .(unique(biome_short_class), rank_trends)]
negative[, order := sort(negative$rank_trends, index.return = TRUE)$ix]

uncertain <- biome_uncertainty[rank_trends <= 5 & trend == "uncertain", .(unique(biome_short_class), rank_trends)]
uncertain[, order := sort(uncertain$rank_trends, index.return = TRUE)$ix]

pos <- ggplot(biome_uncertainty[biome_short_class %in% positive$V1
                                     & trend %in% c("positive probable", "positive likely")], aes(x = "", y = biome_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(biome_short_class, levels = c(positive$V1[positive$order])), nrow = 1,
             labeller = label_wrap_gen(width=10))+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))


neg <- ggplot(biome_uncertainty[biome_short_class %in% negative$V1
                                     & trend %in% c("negative likely", "negative probable")], aes(x = "", y = biome_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(biome_short_class, levels = c(negative$V1[negative$order])), nrow = 1,
             labeller = label_wrap_gen(width=10))+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))

un <- ggplot(biome_uncertainty[biome_short_class %in% uncertain$V1
                                    & trend %in% c("uncertain")], aes(x = "", y = biome_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(biome_short_class, levels = c(uncertain$V1[uncertain$order])), nrow = 1,
             labeller = label_wrap_gen(width=10))+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  coord_cartesian(ylim = c(0, 100))

ggbiome <- ggarrange(pos, neg, un, nrow = 1, align = "hv")
b <- annotate_figure(ggbiome, top = text_grob("Biomes", 
                                                  color = "black", face = "bold", size = 14),
                     fig.lab = "b)", fig.lab.pos = "top.left")
b

### elevation ----

positive <- elev_uncertainty[rank_trends <= 5 & trend == "positive likely", .(unique(elev_class), rank_trends)]
positive[, order := sort(positive$rank_trends, index.return = TRUE)$ix]

negative <- elev_uncertainty[rank_trends <= 5 & trend == "negative likely", .(unique(elev_class), rank_trends)]
negative[, order := sort(negative$rank_trends, index.return = TRUE)$ix]

uncertain <- elev_uncertainty[rank_trends <= 5 & trend == "uncertain", .(unique(elev_class), rank_trends)]
uncertain[, order := sort(uncertain$rank_trends, index.return = TRUE)$ix]

pos <- ggplot(elev_uncertainty[elev_class %in% positive$V1
                                     & trend %in% c("positive probable", "positive likely")], aes(x = "", y = elev_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(elev_class, levels = c(positive$V1[positive$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))


neg <- ggplot(elev_uncertainty[elev_class %in% negative$V1
                                     & trend %in% c("negative likely", "negative probable")], aes(x = "", y = elev_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(elev_class, levels = c(negative$V1[negative$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))

un <- ggplot(elev_uncertainty[elev_class %in% uncertain$V1
                                    & trend %in% c("uncertain")], aes(x = "", y = elev_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(elev_class, levels = c(uncertain$V1[uncertain$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  coord_cartesian(ylim = c(0, 100))

ggelev <- ggarrange(pos, neg, un, nrow = 1, align = "hv")
c <- annotate_figure(ggelev, top = text_grob("Elevation classes", 
                                                  color = "black", face = "bold", size = 14),
                     fig.lab = "c)", fig.lab.pos = "top.left")
c



### KG_3 ----

positive <- KG_3_uncertainty [rank_trends <= 5 & trend == "positive likely", .(unique(KG_class_3), rank_trends)]
positive[, order := sort(positive$rank_trends, index.return = TRUE)$ix]

negative <- KG_3_uncertainty [rank_trends <= 5 & trend == "negative likely", .(unique(KG_class_3), rank_trends)]
negative[, order := sort(negative$rank_trends, index.return = TRUE)$ix]

uncertain <- KG_3_uncertainty [rank_trends <= 5 & trend == "uncertain", .(unique(KG_class_3), rank_trends)]
uncertain[, order := sort(uncertain$rank_trends, index.return = TRUE)$ix]

pos <- ggplot(KG_3_uncertainty[KG_class_3%in% positive$V1
                               & trend %in% c("positive probable", "positive likely")], aes(x = "", y = KG_3_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(KG_class_3, levels = c(positive$V1[positive$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))


neg <- ggplot(KG_3_uncertainty [KG_class_3%in% negative$V1
                               & trend %in% c("negative likely", "negative probable")], aes(x = "", y = KG_3_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(KG_class_3, levels = c(negative$V1[negative$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))

un <- ggplot(KG_3_uncertainty [KG_class_3%in% uncertain$V1
                              & trend %in% c("uncertain")], aes(x = "", y = KG_3_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(KG_class_3, levels = c(uncertain$V1[uncertain$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  coord_cartesian(ylim = c(0, 100))

ggKG_3 <- ggarrange(pos, neg, un, nrow = 1, align = "hv")
d <- annotate_figure(ggKG_3, top = text_grob("Koeppen-Geiger classes", 
                                             color = "black", face = "bold", size = 14),
                     fig.lab = "d)", fig.lab.pos = "top.left")
d



### IPCC ----

positive <- ipcc_uncertainty [rank_trends <= 5 & trend == "positive likely", .(unique(IPCC_ref_region), rank_trends)]
positive[, order := sort(positive$rank_trends, index.return = TRUE)$ix]

negative <- ipcc_uncertainty [rank_trends <= 5 & trend == "negative likely", .(unique(IPCC_ref_region), rank_trends)]
negative[, order := sort(negative$rank_trends, index.return = TRUE)$ix]

uncertain <- ipcc_uncertainty [rank_trends <= 5 & trend == "uncertain", .(unique(IPCC_ref_region), rank_trends)]
uncertain[, order := sort(uncertain$rank_trends, index.return = TRUE)$ix]

pos <- ggplot(ipcc_uncertainty [IPCC_ref_region%in% positive$V1
                               & trend %in% c("positive probable", "positive likely")], aes(x = "", y = ipcc_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(IPCC_ref_region, levels = c(positive$V1[positive$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))


neg <- ggplot(ipcc_uncertainty [IPCC_ref_region%in% negative$V1
                               & trend %in% c("negative likely", "negative probable")], aes(x = "", y = ipcc_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(IPCC_ref_region, levels = c(negative$V1[negative$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))

un <- ggplot(ipcc_uncertainty [IPCC_ref_region%in% uncertain$V1
                              & trend %in% c("uncertain")], aes(x = "", y = ipcc_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(IPCC_ref_region, levels = c(uncertain$V1[uncertain$order])), nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.position = "none", legend.spacing.x = unit(1.0, 'cm')
        
  )+
  coord_cartesian(ylim = c(0, 100))

ggipcc <- ggarrange(pos, neg, un, nrow = 1, align = "hv")

e <- annotate_figure(ggipcc, top = text_grob("IPCC reference regions", 
                                             color = "black", face = "bold", size = 14),
                     fig.lab = "e)", fig.lab.pos = "top.left")
e

all <- ggarrange(a,b,c,d,e, nrow = 5)
all


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_probability_groups_masks_top5.png"), 
       width = 14, height = 12)
