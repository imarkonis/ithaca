# Figure SI
# IPCC (6 continents)
## 6 x 4 
# Aridity:  
## 1 precipitation
## 2 soil moisture
# Temperature: 
## 3
# ENSO
## 4

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

# IPCC ####
IPCC_Africa <- c("ARP", "CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

## Precipitation ----

### Data ----
evap_stats_dry <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_spi.rds"))
evap_stats_wet <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_spi.rds"))

### Figure prep  ----

evap_stats <- merge(evap_stats_dry, evap_stats_wet, by = c("lon", "lat"), suffixes = c(".dry", ".wet"))

evap_stats[, ratio_std_quantile := std_quant_range.dry/std_quant_range.wet]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11,500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("wet > dry           ",
                                                         "dry = wet           ",
                                                         "dry > wet           "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### IPCC ----
ipcc_stats <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, IPCC_ref_region)]
ipcc_stats <- ipcc_stats[complete.cases(ipcc_stats)]
ipcc_stats[, ipcc_area:= sum(stats_area), .(IPCC_ref_region)]
ipcc_stats[, ipcc_fraction:= stats_area/ipcc_area]


data_high <- ipcc_stats[ratio_std_quantile_brk == "dry > wet           ",
                        .(fraction = sum(ipcc_fraction)), IPCC_ref_region]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

ipcc_stats[, IPCC_ref_region := factor(IPCC_ref_region, levels = data_high$IPCC_ref_region)]


fig_spi_africa <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Africa]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_spi_asia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Asia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_spi_australasia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Australasia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_spi_europe <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Europe]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_spi_namerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Namerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_spi_samerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Samerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
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
                                              labels = c("wet > dry           ",
                                                         "dry = wet           ",
                                                         "dry > wet           "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### IPCC ----
ipcc_stats <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, IPCC_ref_region)]
ipcc_stats <- ipcc_stats[complete.cases(ipcc_stats)]
ipcc_stats[, ipcc_area:= sum(stats_area), .(IPCC_ref_region)]
ipcc_stats[, ipcc_fraction:= stats_area/ipcc_area]

data_high <- ipcc_stats[ratio_std_quantile_brk == "dry > wet           ",
                        .(fraction = sum(ipcc_fraction)), IPCC_ref_region]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

ipcc_stats[, IPCC_ref_region := factor(IPCC_ref_region, levels = data_high$IPCC_ref_region)]


fig_ssi_africa <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Africa]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_ssi_asia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Asia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_ssi_australasia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Australasia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_ssi_europe <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Europe]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_ssi_namerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Namerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_ssi_samerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Samerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet           " = colset_RdBu_5[1], 
                               "wet > dry           " = colset_RdBu_5[5],
                               "dry = wet           " = "gray90"))+
  theme_light()+
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
                                              labels = c("cold > warm     ",
                                                         "warm = cold      ",
                                                         "warm > cold      "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### IPCC ----
ipcc_stats <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, IPCC_ref_region)]
ipcc_stats <- ipcc_stats[complete.cases(ipcc_stats)]
ipcc_stats[, ipcc_area:= sum(stats_area), .(IPCC_ref_region)]
ipcc_stats[, ipcc_fraction:= stats_area/ipcc_area]

data_high <- ipcc_stats[ratio_std_quantile_brk == "warm > cold      ",
                        .(fraction = sum(ipcc_fraction)), IPCC_ref_region]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

ipcc_stats[, IPCC_ref_region := factor(IPCC_ref_region, levels = data_high$IPCC_ref_region)]


fig_sti_africa <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Africa]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "STI SQR")  +
  scale_fill_manual(values = c("cold > warm     " = colset_RdBu_5[5], 
                               "warm > cold      " = colset_RdBu_5[1],
                               "warm = cold      " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_sti_asia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Asia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "STI SQR")  +
  scale_fill_manual(values = c("cold > warm     " = colset_RdBu_5[5], 
                               "warm > cold      " = colset_RdBu_5[1],
                               "warm = cold      " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_sti_australasia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Australasia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "STI SQR")  +
  scale_fill_manual(values = c("cold > warm     " = colset_RdBu_5[5], 
                               "warm > cold      " = colset_RdBu_5[1],
                               "warm = cold      " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_sti_europe <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Europe]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "STI SQR")  +
  scale_fill_manual(values = c("cold > warm     " = colset_RdBu_5[5], 
                               "warm > cold      " = colset_RdBu_5[1],
                               "warm = cold      " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_sti_namerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Namerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "STI SQR")  +
  scale_fill_manual(values = c("cold > warm     " = colset_RdBu_5[5], 
                               "warm > cold      " = colset_RdBu_5[1],
                               "warm = cold      " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_sti_samerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Samerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "STI SQR")  +
  scale_fill_manual(values = c("cold > warm     " = colset_RdBu_5[5], 
                               "warm > cold      " = colset_RdBu_5[1],
                               "warm = cold      " = "gray90"))+
  theme_light()+
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
                                              labels = c("la nina > el nino",
                                                         "la nina = el nino","el nino > la nina"), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### IPCC ----
ipcc_stats <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, IPCC_ref_region)]
ipcc_stats <- ipcc_stats[complete.cases(ipcc_stats)]
ipcc_stats[, ipcc_area:= sum(stats_area), .(IPCC_ref_region)]
ipcc_stats[, ipcc_fraction:= stats_area/ipcc_area]

data_high <- ipcc_stats[ratio_std_quantile_brk == "el nino > la nina",
                        .(fraction = sum(ipcc_fraction)), IPCC_ref_region]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

ipcc_stats[, IPCC_ref_region := factor(IPCC_ref_region, levels = data_high$IPCC_ref_region)]


fig_ninos_africa <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Africa]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "ENSO SQR")  +
  scale_fill_manual(values = c("la nina > el nino" = colset_RdBu_5[5], 
                               "el nino > la nina" = colset_RdBu_5[1],
                               "la nina = el nino" = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_nino_asia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Asia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "ENSO SQR")  +
  scale_fill_manual(values = c("la nina > el nino" = colset_RdBu_5[5], 
                               "el nino > la nina" = colset_RdBu_5[1],
                               "la nina = el nino" = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_nino_australasia <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Australasia]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "ENSO SQR")  +
  scale_fill_manual(values = c("la nina > el nino" = colset_RdBu_5[5], 
                               "el nino > la nina" = colset_RdBu_5[1],
                               "la nina = el nino" = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_nino_europe <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Europe]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "ENSO SQR")  +
  scale_fill_manual(values = c("la nina > el nino" = colset_RdBu_5[5], 
                               "el nino > la nina" = colset_RdBu_5[1],
                               "la nina = el nino" = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


fig_nino_namerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Namerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "ENSO SQR")  +
  scale_fill_manual(values = c("la nina > el nino" = colset_RdBu_5[5], 
                               "el nino > la nina" = colset_RdBu_5[1],
                               "la nina = el nino" = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig_nino_samerica <- ggplot(ipcc_stats[IPCC_ref_region %in% IPCC_Samerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC regions')  +
  ylab('Area Fraction')  +
  labs(fill = "ENSO SQR")  +
  scale_fill_manual(values = c("la nina > el nino" = colset_RdBu_5[5], 
                               "el nino > la nina" = colset_RdBu_5[1],
                               "la nina = el nino" = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## composite figures ----

africa <- ggarrange(fig_spi_africa, fig_ssi_africa, fig_sti_africa, fig_ninos_africa, 
                    labels = c("a", "b", "c", "d"), align = "hv", nrow = 1)

africa_an <- annotate_figure(africa, top = text_grob("Africa", 
                                                     face = "bold", size = 14))
asia <- ggarrange(fig_spi_asia, fig_ssi_asia, fig_sti_asia, fig_nino_asia, 
                  labels = c("e", "f", "g", "h"), align = "hv", nrow = 1)

asia_an <- annotate_figure(asia, top = text_grob("Asia", 
                                                 face = "bold", size = 14))
australasia <- ggarrange(fig_spi_australasia, fig_ssi_australasia, fig_sti_australasia, fig_nino_australasia, 
                         labels = c("i", "j", "k", "l"), align = "hv", nrow = 1)

australasia_an <- annotate_figure(australasia, top = text_grob("Australasia", 
                                                               face = "bold", size = 14))

europe <- ggarrange(fig_spi_europe, fig_ssi_europe, fig_sti_europe, fig_nino_europe, 
                    labels = c("m", "n", "o", "p"), align = "hv", nrow = 1)

europe_an <- annotate_figure(europe, top = text_grob("Europe", 
                                                     face = "bold", size = 14))

namerica <- ggarrange(fig_spi_namerica, fig_ssi_namerica, fig_sti_namerica, fig_nino_namerica, 
                      labels = c("q", "r", "s", "t"), align = "hv", nrow = 1)

namerica_an <- annotate_figure(namerica, top = text_grob("North America", 
                                                       face = "bold", size = 14))
samerica <- ggarrange(fig_spi_samerica, fig_ssi_samerica, fig_sti_samerica, fig_nino_samerica, 
                      labels = c("u", "v", "w", "x"), align = "hv", nrow = 1)
samerica_an <- annotate_figure(samerica, top = text_grob("South America", 
                                                         face = "bold", size = 14))
ggarrange(africa_an, asia_an, australasia_an, europe_an, namerica_an, samerica_an, align = "hv", nrow = 6)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig5_SI_IPCC_SQR_aridity_indices.png"), 
       width = 14, height = 16)


