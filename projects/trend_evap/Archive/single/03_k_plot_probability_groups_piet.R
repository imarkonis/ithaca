# barplots of of DCI and probability groups
source('source/evap_trend.R')

## Data ----
# Input data generated in trend_evap/02_c
land_cover_uncertainty <- readRDS( paste0(PATH_SAVE_EVAP_TREND, "land_cover_uncertainty.rds"))
biome_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biomes_uncertainty.rds"))
elev_uncertainty <- readRDS( paste0(PATH_SAVE_EVAP_TREND, "elevation_uncertainty.rds"))
ipcc_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_reference_regions_uncertainty.rds"))
KG_3_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_uncertainty.rds"))

## Analysis
land_cover_uncertainty[, rank_trends := rank(-land_cover_fraction), trend]
biome_uncertainty[, rank_trends := rank(-biome_fraction), trend] 
elev_uncertainty[, rank_trends := rank(-elev_fraction), trend] 
ipcc_uncertainty[, rank_trends := rank(-ipcc_fraction), trend] 
KG_3_uncertainty[, rank_trends := rank(-KG_3_fraction), trend] 

## plot ---

positive <- land_cover_uncertainty[rank_trends <= 5 & trend == "positive likely", .(unique(land_cover_short_class), rank_trends)]
positive[, order := sort(positive$rank_trends, index.return = TRUE)$ix]

negative <- land_cover_uncertainty[rank_trends <= 5 & trend == "negative likely", .(unique(land_cover_short_class), rank_trends)]
negative[, order := sort(negative$rank_trends, index.return = TRUE)$ix]

uncertain <- land_cover_uncertainty[rank_trends <= 5 & trend == "uncertain", .(unique(land_cover_short_class), rank_trends)]
uncertain[, order := sort(uncertain$rank_trends, index.return = TRUE)$ix]

pos_data <- land_cover_uncertainty[land_cover_short_class %in% positive$V1]
pos_data[, domains := factor(land_cover_short_class, levels = c(positive$V1[positive$order]))]
setorder(pos_data[,.r := order(rep(rev(levels(trend)),5))], .r)[,.r := NULL]

ggplot(pos_data, aes(x = "", y = land_cover_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~domains, nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        axis.text = element_blank(),
        legend.position="bottom"
        
  )+
  geom_text(aes(x = 1.8, label = round(land_cover_fraction*100, digits = 1)),
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y", start = 0)

ggplot(pos_data, aes(x = "", y = land_cover_fraction*100))+
  geom_bar(aes(fill = trend), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~domains, nrow = 1)+
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        axis.text = element_blank()
        
  )

neg <- ggplot(land_cover_uncertainty[land_cover_short_class %in% negative$V1])+
  geom_bar(aes(x = "", y = land_cover_fraction*100, fill = trend), stat = "identity") +
  xlab('Land cover class')  +
  ylab('Area fraction [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(land_cover_short_class, levels = c(negative$V1[negative$order])), nrow = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid")
  )+
  coord_polar("y", start=0)

un <- ggplot(land_cover_uncertainty[land_cover_short_class %in% uncertain$V1])+
  geom_bar(aes(x = "", y = land_cover_fraction*100, fill = trend), stat = "identity") +
  xlab('Land cover class')  +
  ylab('Area fraction [%]')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  facet_wrap(~factor(land_cover_short_class, levels = c(uncertain$V1[uncertain$order])), nrow = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
        strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid")
  )+
  coord_polar("y", start=0)
