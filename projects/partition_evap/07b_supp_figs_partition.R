# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
load(paste0(PATH_SAVE_PARTITION_PREC, "partition_prec.Rdata"))

## Variables
prec_mask <- prec_mask[prec_stats[, .(lon, lat, prec_mean = ens_mean_mean)], on = .(lon, lat)]
prec_mask <- prec_mask[complete.cases(prec_mask)]
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

## Analysis
### Land cover
land_cover_short_class_prec <- prec_mask[, .(lon, lat, prec_mean,
                                           land_cover_short_class, rel_dataset_agreement)]
land_cover_short_class_prec <- land_cover_short_class_prec[complete.cases(land_cover_short_class_prec)]
dummie_area <- grid_area(land_cover_short_class_prec[, .(lon, lat)])
land_cover_short_class_prec <- merge(land_cover_short_class_prec, dummie_area,
                                   by = c("lon", "lat"))
land_cover_short_class_prec[, rel_area := sum(area),
                          by = .(land_cover_short_class, rel_dataset_agreement)
][, wprec_mean := prec_mean * area / rel_area
][, prec_wavg := sum(wprec_mean),
  by = .(land_cover_short_class, rel_dataset_agreement)
][, prec_dif := prec_mean - prec_wavg
][, prec_n := nrow(.SD),
  by = .(land_cover_short_class, rel_dataset_agreement)
][, prec_sd := sqrt(sum(prec_dif^2)/(prec_n -1)),
  by = .(land_cover_short_class, rel_dataset_agreement)]

land_cover_short_class_prec <- land_cover_short_class_prec[, .(prec_wavg, prec_sd,
                                                           prec_median = median(prec_mean),
                                                           prec_q25 = quantile(prec_mean, 0.25),
                                                           prec_q75 = quantile(prec_mean, 0.75)), 
                                                       .(land_cover_short_class, rel_dataset_agreement)]
land_cover_short_class_prec <- unique(land_cover_short_class_prec)

land_cover_agreement_cum <- land_cover_agreement[, .(cumsum_land_cover = cumsum(prec_sum), 
                                                 rel_dataset_agreement), .(land_cover_short_class)]
land_cover_agreement_cum[, fraction_bias := cumsum_land_cover  / sum(cumsum_land_cover), rel_dataset_agreement]
land_cover_agreement_cum <- land_cover_agreement_cum[complete.cases(land_cover_agreement_cum)]

### Biomes
biome_short_class_prec <- prec_mask[, .(lon, lat, prec_mean, biome_short_class, rel_dataset_agreement)]
biome_short_class_prec <- biome_short_class_prec[complete.cases(biome_short_class_prec)]
dummie_area <- grid_area(biome_short_class_prec[, .(lon, lat)])
biome_short_class_prec <- merge(biome_short_class_prec, dummie_area,
                                by = c("lon", "lat"))
biome_short_class_prec[, rel_area := sum(area),
                       by = .(biome_short_class, rel_dataset_agreement)
][, wprec_mean := prec_mean*area/rel_area
][, prec_wavg := sum(wprec_mean),
  by = .(biome_short_class, rel_dataset_agreement)
][, prec_dif := prec_mean - prec_wavg
][, prec_n := nrow(.SD),
  by = .(biome_short_class, rel_dataset_agreement)
][, prec_sd := sqrt(sum(prec_dif^2)/(prec_n -1)),
  by = .(biome_short_class, rel_dataset_agreement)]

biome_short_class_prec <- biome_short_class_prec[, .(prec_wavg, prec_sd,
                                                     prec_median = median(prec_mean),
                                                     prec_q25 = quantile(prec_mean, 0.25),
                                                     prec_q75 = quantile(prec_mean, 0.75)), 
                                                 .(biome_short_class, rel_dataset_agreement)]
biome_short_class_prec <- unique(biome_short_class_prec)

biome_agreement_cum <- biome_agreement[, .(cumsum_biome = cumsum(prec_sum), 
                                           rel_dataset_agreement), .(biome_short_class)]
biome_agreement_cum[, fraction_bias := cumsum_biome  / sum(cumsum_biome), rel_dataset_agreement]
biome_agreement_cum <- biome_agreement_cum[complete.cases(biome_agreement_cum)]

### Elevation
elev_class_prec <- prec_mask[, .(lon, lat, prec_mean, elev_class, rel_dataset_agreement)]
elev_class_prec <- elev_class_prec[complete.cases(elev_class_prec)]
dummie_area <- grid_area(elev_class_prec[, .(lon, lat)])
elev_class_prec <- merge(elev_class_prec, dummie_area,
                         by = c("lon", "lat"))
elev_class_prec[, rel_area := sum(area),
                by = .(elev_class, rel_dataset_agreement)
][, wprec_mean := prec_mean*area/rel_area
][, prec_wavg := sum(wprec_mean),
  by = .(elev_class, rel_dataset_agreement)
][, prec_dif := prec_mean - prec_wavg
][, prec_n := nrow(.SD),
  by = .(elev_class, rel_dataset_agreement)
][, prec_sd := sqrt(sum(prec_dif^2)/(prec_n -1)),
  by = .(elev_class, rel_dataset_agreement)]

elev_class_prec <- elev_class_prec[, .(prec_wavg, prec_sd,
                                       prec_median = median(prec_mean),
                                       prec_q25 = quantile(prec_mean, 0.25),
                                       prec_q75 = quantile(prec_mean, 0.75)), 
                                   .(elev_class, rel_dataset_agreement)]
elev_class_prec <- unique(elev_class_prec)

elev_agreement_cum <- elevation_agreement[, .(cumsum_elev = cumsum(prec_sum), 
                                              rel_dataset_agreement), .(elev_class)]
elev_agreement_cum[, fraction_bias := cumsum_elev  / sum(cumsum_elev), rel_dataset_agreement]
elev_agreement_cum <- elev_agreement_cum[complete.cases(elev_agreement_cum)]

### Precipitation Quantiles
prec_quant_class_prec <- prec_mask[, .(lon, lat, prec_mean, prec_quant, rel_dataset_agreement)]
prec_quant_class_prec <- prec_quant_class_prec[complete.cases(prec_quant_class_prec)]
dummie_area <- grid_area(prec_quant_class_prec[, .(lon, lat)])
prec_quant_class_prec <- merge(prec_quant_class_prec, dummie_area,
                               by = c("lon", "lat"))
prec_quant_class_prec[, rel_area := sum(area),
                      by = .(prec_quant, rel_dataset_agreement)
][, wprec_mean := prec_mean*area/rel_area
][, prec_wavg := sum(wprec_mean),
  by = .(prec_quant, rel_dataset_agreement)
][, prec_dif := prec_mean - prec_wavg
][, prec_n := nrow(.SD),
  by = .(prec_quant, rel_dataset_agreement)
][, prec_sd := sqrt(sum(prec_dif^2)/(prec_n -1)),
  by = .(prec_quant, rel_dataset_agreement)]

prec_quant_class_prec <- prec_quant_class_prec[, .(prec_wavg, prec_sd,
                                                   prec_median = median(prec_mean),
                                                   prec_q25 = quantile(prec_mean, 0.25),
                                                   prec_q75 = quantile(prec_mean, 0.75)), 
                                               .(prec_quant, rel_dataset_agreement)]
prec_quant_class_prec <- unique(prec_quant_class_prec)

prec_agreement_cum <- prec_quant_agreement[, .(cumsum_prec = cumsum(prec_sum), 
                                               rel_dataset_agreement), .(prec_quant)]
prec_agreement_cum[, fraction_bias := cumsum_prec  / sum(cumsum_prec), rel_dataset_agreement]
prec_agreement_cum <- prec_agreement_cum[complete.cases(prec_agreement_cum)]


## Figures

#land_cover
ggplot(land_cover_short_class_prec) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = prec_wavg - prec_sd, ymax = prec_wavg + prec_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = prec_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ land_cover_short_class, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual precipitation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/land_cover_median_prec.png"), width = 8, height = 6)

ggplot(land_cover_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = land_cover_short_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Land cover class')  +
  scale_fill_manual(values = colset_land_cover_short) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/land_cover_agreement_cum.png"), width = 8, height = 6)


#biome
ggplot(biome_short_class_prec) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = prec_wavg - prec_sd, ymax = prec_wavg + prec_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = prec_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ biome_short_class, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual precipitation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/biome_median_prec.png"), width = 8, height = 6)

ggplot(biome_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = biome_short_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Biome category')  +
  scale_fill_manual(values = colset_biome_short[c(2, 8, 10, 9, 6, 1, 7, 5, 4, 3)]) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/biome_agreement_cum.png"), width = 8, height = 6)

#elevation
ggplot(elev_class_prec) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = prec_wavg - prec_sd, ymax = prec_wavg + prec_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = prec_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ elev_class, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual precipitation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/elev_median_prec.png"), width = 8, height = 6)

ggplot(elev_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = elev_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Elevation zone')  +
  scale_fill_manual(values = rev(c("dark red", colset_RdBu_5))) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/elev_agreement_cum.png"), width = 8, height = 6)

#precipitation_quantiles
ggplot(prec_quant_class_prec) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = prec_wavg - prec_sd, ymax = prec_wavg + prec_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = prec_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ prec_quant, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual precipitation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_median_prec.png"), width = 8, height = 6)

ggplot(prec_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = prec_quant), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'Prec. quantile')  +
  scale_fill_manual(values = colset_prec_quant) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_agreement_cum.png"), width = 8, height = 6)
