# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# barplots of of DCI and probability groups
source('source/changing_prec.R')

## Data ----
# Input data generated in changing_prec/02_b_
land_cover_uncertainty <- readRDS( paste0(PATH_SAVE_CHANGING_PREC, "02_b_land_cover_uncertainty_bootstrap.rds"))
biome_uncertainty <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_biomes_uncertainty_bootstrap.rds"))
elev_uncertainty <- readRDS( paste0(PATH_SAVE_CHANGING_PREC, "02_b_elevation_uncertainty_bootstrap.rds"))

## Analysis
land_cover_uncertainty <- land_cover_uncertainty[land_cover_short_class != "Other"]
land_cover_uncertainty[, rank_trends := rank(-land_cover_fraction), trend]
biome_uncertainty[, rank_trends := rank(-biome_fraction), trend] 
elev_uncertainty[, rank_trends := rank(-elev_fraction), trend] 


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

### landcover ----
positive <- land_cover_uncertainty[rank_trends <= 5 & trend == "positive likely", .(unique(land_cover_short_class), rank_trends)]
positive[, order := sort(positive$rank_trends, index.return = TRUE)$ix]

negative <- land_cover_uncertainty[rank_trends <= 5 & trend == "negative likely", .(unique(land_cover_short_class), rank_trends)]
negative[, order := sort(negative$rank_trends, index.return = TRUE)$ix]

uncertain <- land_cover_uncertainty[rank_trends <= 5 & trend == "uncertain", .(unique(land_cover_short_class), rank_trends)]
uncertain[, order := sort(uncertain$rank_trends, index.return = TRUE)$ix]

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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))



gglandcover <- ggarrange(un, nrow = 1,  align = "hv")
a <- annotate_figure(gglandcover, top = text_grob("Land cover", 
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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))

ggbiome <- ggarrange(un, nrow = 1, align = "hv")
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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  coord_cartesian(ylim = c(0, 100))


ggelev <- ggarrange(un,  nrow = 1, align = "hv")
c <- annotate_figure(ggelev, top = text_grob("Elevation", 
                                             color = "black", face = "bold", size = 14),
                     fig.lab = "c)", fig.lab.pos = "top.left")
c



all <- ggarrange(a,b,c, nrow = 3)
all


ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_l_bar_uncertain_masks_top5_bootstrap.png"), 
       width = 6, height = 6, dpi = 600)
