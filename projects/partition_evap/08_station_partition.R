# Plot global map of dataset agreement classses 
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(ggthemes)
library(scales)
library(ggpattern)

## Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(prec_mask$prec_quant_dataset_agreement) <- c("High", "Above average",
                                                    "Average", "Below average",
                                                    "Low")
levels(prec_mask$abs_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(prec_mask$land_cover_short_class) <- c("Forests", "Savannas", "Croplands",
                                           "Shrublands", "Grasslands", "Water",
                                           "Barren", "Snow/Ice", "Other")
levels(prec_mask$biome_short_class) <- c("T/S Forests", "T/S Grasslands",
                                         "T. Forests", "B. Forests", 
                                         "T. Grasslands", "Deserts", "Tundra", 
                                         "Flooded", "M. Grasslands",
                                         "Mediterranean", NA)
## Cross-reference with stations
stations <- raster(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                          "prec_station_grid.nc")) %>%
  as.data.frame(xy = TRUE, na.rm = TRUE) %>% data.table()
setnames(stations, c("x", "y", "layer"), c("lon", "lat", "stn_count"))

prec_mask <- merge(prec_mask, stations, by = c("lon", "lat"),
                   all.x = TRUE)

prec_mask <- prec_mask[stn_count >= 1, stn_log := TRUE
                       ][is.na(stn_count), stn_log := FALSE
                         ][, n_land_cover := nrow(.SD), by = land_cover_short_class
                           ][, land_cover_frac := sum(stn_log)/n_land_cover, by = .(land_cover_short_class, rel_dataset_agreement)
                             ][stn_log == FALSE, land_cover_frac := sum(!stn_log)/n_land_cover, by = .(land_cover_short_class, rel_dataset_agreement)
                               ][, n_biome := nrow(.SD), by = biome_short_class
                                 ][, biome_frac := sum(stn_log)/n_biome, by = .(biome_short_class, rel_dataset_agreement)
                                   ][stn_log == FALSE, biome_frac := sum(!stn_log)/n_biome, by = .(biome_short_class, rel_dataset_agreement)
                                     ][, n_elev := nrow(.SD), by = elev_class
                                       ][, elev_frac := sum(stn_log)/n_elev, by = .(elev_class, rel_dataset_agreement)
                                         ][stn_log == FALSE, elev_frac := sum(!stn_log)/n_elev, by = .(elev_class, rel_dataset_agreement)
                                           ][, n_quant := nrow(.SD), by = prec_quant
                                             ][, prec_quant_frac := sum(stn_log)/n_quant, by = .(prec_quant, rel_dataset_agreement)
                                               ][stn_log == FALSE, prec_quant_frac := sum(!stn_log)/n_quant, by = .(prec_quant, rel_dataset_agreement)]

prec_mask[stn_log == TRUE, table(land_cover_short_class)] / prec_mask[stn_log == FALSE, table(land_cover_short_class)]

prec_mask[stn_log == TRUE, table(rel_dataset_agreement)]/nrow(prec_mask[stn_log == TRUE])
prec_mask[stn_log == FALSE, table(rel_dataset_agreement)]/nrow(prec_mask[stn_log == FALSE])

## Figures
p01 <- ggplot(unique(prec_mask[land_cover_short_class != "Other", .(land_cover_short_class, land_cover_frac, stn_log, rel_dataset_agreement)])) +
  geom_bar_pattern(aes(x = land_cover_short_class, y = land_cover_frac, fill = rel_dataset_agreement, pattern = stn_log),
                   color = "black",
                   stat = "identity", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.01,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("none", "stripe")) +
  xlab('Land Cover Type')  +
  ylab(bquote('Grid Cell Fraction'))  +
  labs(fill = 'Dataset agreement', pattern = 'Stations present')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p02 <- ggplot(unique(prec_mask[biome_short_class != "Other", .(biome_short_class, biome_frac, stn_log, rel_dataset_agreement)])) +
  geom_bar_pattern(aes(x = biome_short_class, y = biome_frac, fill = rel_dataset_agreement, pattern = stn_log),
                   color = "black",
                   stat = "identity", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.01,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("none", "stripe")) +
  xlab('Biome Class')  +
  ylab(bquote('Grid Cell Fraction'))  +
  labs(fill = 'Dataset agreement', pattern = 'Stations present')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p03 <- ggplot(unique(prec_mask[elev_class != "Other", .(elev_class, elev_frac, stn_log, rel_dataset_agreement)])) +
  geom_bar_pattern(aes(x = elev_class, y = elev_frac, fill = rel_dataset_agreement, pattern = stn_log),
                   color = "black",
                   stat = "identity", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.01,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("none", "stripe")) +
  xlab('Elevation [m]')  +
  ylab(bquote('Grid Cell Fraction'))  +
  labs(fill = 'Dataset agreement', pattern = 'Stations present')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p04 <- ggplot(unique(prec_mask[prec_quant != "Other", .(prec_quant, prec_quant_frac, stn_log, rel_dataset_agreement)])) +
  geom_bar_pattern(aes(x = prec_quant, y = prec_quant_frac, fill = rel_dataset_agreement, pattern = stn_log),
                   color = "black",
                   stat = "identity", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.01,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("none", "stripe")) +
  xlab('Precipitation Quantile')  +
  ylab(bquote('Grid Cell Fraction'))  +
  labs(fill = 'Dataset agreement', pattern = 'Stations present')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p00 <- ggarrange(p01, p02, p03, p04, labels = c('a', 'b', 'c', 'd'), align = 'hv',
          common.legend = T, legend = 'right', nrow = 2, ncol = 2)

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "station_fraction_agreement.png"), p00, width = 10, height = 10)
