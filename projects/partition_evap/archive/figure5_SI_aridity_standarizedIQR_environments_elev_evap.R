# Figure of standardized quartile range for various standaridized indices ---- 
## evap and Land use 
# Aridity: precipitation 
## a and b precipitation
## c and d soil moisture
# Temperature: 
## e and f 
# ENSO
## g and h

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Precipitation ----

### Data ----
evap_stats_dry <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_spi.rds"))
evap_stats_wet <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_spi.rds"))

### Figure prep  ----

evap_stats <- merge(evap_stats_dry, evap_stats_wet, by = c("lon", "lat"), suffixes = c(".dry", ".wet"))

evap_stats[, ratio_std_quantile := std_quant_range.dry/std_quant_range.wet]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11,500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("wet > dry             ",
                                                         "dry = wet             ",
                                                         "dry > wet             "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Elevation ----
elev <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, elev_class)]
elev <- elev[complete.cases(elev)]
elev[, land_area:= sum(stats_area), .( elev_class)]
elev[, land_fraction:= stats_area/land_area]

data_high <- elev[ratio_std_quantile_brk == "dry > wet             ",
                     .(fraction = sum(land_fraction)), elev_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

elev[, elev_class := factor(elev_class, levels = data_high$elev_class)]


fig_a_spi_elev <- ggplot(elev[elev_class != "Other"]) +
  geom_bar(aes(x = elev_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Elevation')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI\nstandardized IQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                               "wet > dry             " = colset_RdBu_5[5],
                               "dry = wet             " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### Evaporation quantile ----
evap_stats <- evap_stats_masks[,.(sqr_group_area = sum(area)),.(ratio_std_quantile_brk, evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]
evap_stats[, evap_area:= sum(sqr_group_area), .(evap_quant)]
evap_stats[, evap_fraction:= sqr_group_area/evap_area]
evap_stats[, evap_quant := factor(evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]

data_high <- evap_stats[ratio_std_quantile_brk == "dry > wet             ",
                         .(fraction = sum(evap_fraction)), evap_quant]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

evap_stats[, evap_quant:= factor(evap_quant, levels = data_high$evap_quant)]

fig_b_spi_evap  <- ggplot(evap_stats) +
  geom_bar(aes(x = evap_quant, y = evap_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Evaporation quantile')  +
  ylab('Area fraction')  +
  labs(fill = "SPI\nstandardized IQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                               "wet > dry             " = colset_RdBu_5[5],
                               "dry = wet             " = "gray90"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Soil moisture ----
### Data ----
evap_stats_dry <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_ssi_era5-land.rds"))
evap_stats_wet <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_ssi_era5-land.rds"))

### Figure prep  ----

evap_stats <- merge(evap_stats_dry, evap_stats_wet, by = c("lon", "lat"), suffixes = c(".dry", ".wet"))

evap_stats[, ratio_std_quantile := std_quant_range.dry/std_quant_range.wet]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11,500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("wet > dry             ",
                                                         "dry = wet             ",
                                                         "dry > wet             "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Elevation ----
elev <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, elev_class)]
elev <- elev[complete.cases(elev)]
elev[, land_area:= sum(stats_area), .( elev_class)]
elev[, land_fraction:= stats_area/land_area]
data_high <- elev[ratio_std_quantile_brk == "dry > wet             ",
                     .(fraction = sum(land_fraction)), elev_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

elev[, elev_class := factor(elev_class, levels = data_high$elev_class)]


fig_c_ssi_elev <- ggplot(elev[elev_class != "Other"]) +
  geom_bar(aes(x = elev_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Elevation')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI\nstandardized IQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                               "wet > dry             " = colset_RdBu_5[5],
                               "dry = wet             " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### Evaporation quantile ----
evap_stats <- evap_stats_masks[,.(sqr_group_area = sum(area)),.(ratio_std_quantile_brk, evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]
evap_stats[, evap_area:= sum(sqr_group_area), .(evap_quant)]
evap_stats[, evap_fraction:= sqr_group_area/evap_area]
evap_stats[, evap_quant := factor(evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]
data_high <- evap_stats[ratio_std_quantile_brk == "dry > wet             ",
                         .(fraction = sum(evap_fraction)), evap_quant]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

evap_stats[, evap_quant:= factor(evap_quant, levels = data_high$evap_quant)]
fig_d_ssi_evap  <- ggplot(evap_stats) +
  geom_bar(aes(x = evap_quant, y = evap_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Evaporation quantile')  +
  ylab('Area fraction')  +
  labs(fill = "SSI\nstandardized IQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                               "wet > dry             " = colset_RdBu_5[5],
                               "dry = wet             " = "gray90"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


## Temperature ----
### Data ----

evap_stats_cold <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_cold_sti_era5-land.rds"))
evap_stats_warm <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_warm_sti_era5-land.rds"))

### Figure prep ----
evap_stats <- merge(evap_stats_cold, evap_stats_warm, by = c("lon", "lat"), suffixes = c(".cold", ".warm"))

evap_stats[, ratio_std_quantile := std_quant_range.warm/std_quant_range.cold]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11, 500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("cold > warm       ",
                                                         "warm = cold        ",
                                                         "warm > cold        "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Elevation ---- 

elev <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, elev_class)]
elev <- elev[complete.cases(elev)]
elev[, land_area:= sum(stats_area), .( elev_class)]
elev[, land_fraction:= stats_area/land_area]
data_high <- elev[ratio_std_quantile_brk == "warm > cold        ",
                     .(fraction = sum(land_fraction)), elev_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

elev[, elev_class := factor(elev_class, levels = data_high$elev_class)]

fig_e_sti_elev <- ggplot(elev[elev_class != "Other"]) +
  geom_bar(aes(x = elev_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Elevation')  +
  ylab('Area Fraction')  +
  labs(fill = 'STI\nstandardized IQR')  +
  scale_fill_manual(values = c("cold > warm       " = colset_RdBu_5[5], 
                               "warm > cold        " = colset_RdBu_5[1],
                               "warm = cold        " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### Evaporation quantile ----
evap_stats <- evap_stats_masks[,.(sqr_group_area = sum(area)),.(ratio_std_quantile_brk, evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]
evap_stats[, evap_area:= sum(sqr_group_area), .(evap_quant)]
evap_stats[, evap_fraction:= sqr_group_area/evap_area]
evap_stats[, evap_quant := factor(evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]
data_high <- evap_stats[ratio_std_quantile_brk == "warm > cold        ",
                         .(fraction = sum(evap_fraction)), evap_quant]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

evap_stats[, evap_quant:= factor(evap_quant, levels = data_high$evap_quant)]

fig_f_sti_evap <- ggplot(evap_stats) +
  geom_bar(aes(x = evap_quant, y = evap_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Evaporation quantile')  +
  ylab('Area fraction')  +
  labs(fill = 'STI\nstandardized IQR')  +
  scale_fill_manual(values = c("cold > warm       " = colset_RdBu_5[5], 
                               "warm > cold        " = colset_RdBu_5[1],
                               "warm = cold        " = "gray90"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## ENSO ----
### Data ----

evap_stats_el_nino <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_el_nino.rds"))
evap_stats_la_nina <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_la_nina.rds"))

### Figure prep ----
evap_stats <- merge(evap_stats_el_nino, evap_stats_la_nina, by = c("lon", "lat"), suffixes = c(".el_nino", ".la_nina"))
evap_stats[, ratio_std_quantile := std_quant_range.el_nino/std_quant_range.la_nina]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11, 500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("la nina > el nino ",
                                                         "la nina = el nino ","el nino > la nina  "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Land use ----

elev <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, elev_class)]
elev <- elev[complete.cases(elev)]
elev[, land_area:= sum(stats_area), .(elev_class)]
elev[, land_fraction:= stats_area/land_area]

data_high <- elev[ratio_std_quantile_brk == "el nino > la nina  ",
                     .(fraction = sum(land_fraction)), elev_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

elev[, elev_class := factor(elev_class, levels = data_high$elev_class)]

fig_g_ninos_elev <- ggplot(elev[elev_class != "Other"]) +
  geom_bar(aes(x = elev_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Elevation')  +
  ylab('Area Fraction')  +
  labs(fill = 'ENSO\nstandardized IQR')  +
  scale_fill_manual(values = c("la nina > el nino " = colset_RdBu_5[5], 
                               "el nino > la nina  " = colset_RdBu_5[1],
                               "la nina = el nino " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


### Evaporation quantile ----
evap_stats <- evap_stats_masks[,.(sqr_group_area = sum(area)),.(ratio_std_quantile_brk, evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]
evap_stats[, evap_area:= sum(sqr_group_area), .(evap_quant)]
evap_stats[, evap_fraction:= sqr_group_area/evap_area]
evap_stats[, evap_quant := factor(evap_quant)]
evap_stats <- evap_stats[complete.cases(evap_stats)]

data_high <- evap_stats[ratio_std_quantile_brk == "el nino > la nina  ",
                         .(fraction = sum(evap_fraction)), evap_quant]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

evap_stats[, evap_quant:= factor(evap_quant, levels = data_high$evap_quant)]

fig_h_ninos_evap <- ggplot(evap_stats) +
  geom_bar(aes(x = evap_quant, y = evap_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Evaporation quantile')  +
  ylab('Area Fraction')  +
  labs(fill = 'ENSO\nstandardized IQR')  +
  scale_fill_manual(values = c("la nina > el nino " = colset_RdBu_5[5], 
                               "el nino > la nina  " = colset_RdBu_5[1],
                               "la nina = el nino " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


## composite figures ----

ab <- ggarrange(fig_a_spi_elev, fig_b_spi_evap, common.legend = T,
                legend = "right", labels = c("a", "b"), align = "hv", widths = c(0.75, 1.1))

cd <- ggarrange(fig_c_ssi_elev, fig_d_ssi_evap,  common.legend = T,
                legend = "right", labels = c("c", "d"), align = "hv", widths = c(0.75, 1.1))

ef <- ggarrange(fig_e_sti_elev, fig_f_sti_evap, common.legend = T,
                legend = "right", labels = c("e", "f"), align = "hv", widths = c(0.75, 1.1))

gh <- ggarrange(fig_g_ninos_elev, fig_h_ninos_evap, common.legend = T,
                legend = "right", labels = c("g", "h"), align = "hv", widths = c(0.75, 1.1))


ggarrange(ab, cd, ef, gh, align = "hv", nrow = 4)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig5_SI_SQR_aridity_indices_elev_evap.png"), 
       width = 8, height = 12)

