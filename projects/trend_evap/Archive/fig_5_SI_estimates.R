# Ensemble trend and then product trends with significance in tile format ----
source('source/evap_trend.R')


## Landcover ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_trend_bootstrap.rds"))  
ensemble_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_ensemble_trend_bootstrap.rds"))  

ensemble_trend[, dataset := "ensemble"]

data_trend <- merge(data_trend, ensemble_trend, 
                    by = c("dataset", "land_cover_short_class", "p", "slope", "lower", "upper"),  all = T)


data_trend[slope >= 0 , trend_direction_detailed :=   "pos. p <= 1   "]
data_trend[slope > 0 & p <= 0.1 , trend_direction_detailed :=   "pos. p <= 0.1   "]
data_trend[slope > 0 & p <= 0.05 , trend_direction_detailed :=   "pos. p <= 0.05   "]
data_trend[slope > 0 & p <= 0.01 , trend_direction_detailed :=   "pos. p <= 0.01   "]
data_trend[slope < 0  , trend_direction_detailed :=   "neg. p <= 1   "]
data_trend[slope < 0 & p <= 0.1 , trend_direction_detailed :=   "neg. p <= 0.1   "]
data_trend[slope < 0 & p <= 0.05 , trend_direction_detailed :=   "neg. p <= 0.05   "]
data_trend[slope < 0 & p <= 0.01 , trend_direction_detailed :=   "neg. p <= 0.01   "]
data_trend[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                 level = c("pos. p <= 0.01   ",   "pos. p <= 0.05   ",   "pos. p <= 0.1   ",
                                                           "pos. p <= 1   ",
                                                           "neg. p <= 1   ",   "neg. p <= 0.1   ",   "neg. p <= 0.05   ",
                                                           "neg. p <= 0.01   "), ordered = T),]

data_trend_slopes <- data_trend[, .(mean_slope = mean(slope)), dataset]
data_trend_slopes[, rank := rank(mean_slope)]
data_trend_slopes[dataset == "ensemble", rank := 0]
data_trend_slopes <- data_trend_slopes[order(rank)]
data_trend[, dataset := factor(dataset, levels = data_trend_slopes$dataset)]


ggplot(data_trend[land_cover_short_class != "Other"])+
  geom_tile(aes(x = dataset, 
                y = land_cover_short_class, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                                          y = land_cover_short_class, 
                col = trend_direction_detailed))+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
    "neg. p <= 0.05   " =   "royalblue3", 
    "neg. p <= 0.01   " =   "darkblue", 
    "pos. p <= 0.01   " =   "#330000",
    "pos. p <= 0.05   " =   "darkred",
    "pos. p <= 0.1   " =   "lightcoral",
    "pos. p <= 1   " =   "orange"))+  
  scale_color_manual(values = c(
    "neg. p <= 1   " =   "black",
    "neg. p <= 0.1   " =   "black", 
    "neg. p <= 0.05   " =   "white", 
    "neg. p <= 0.01   " =   "white", 
    "pos. p <= 0.01   " =   "white",
    "pos. p <= 0.05   " =   "white",
    "pos. p <= 0.1   " =   "black",
    "pos. p <= 1   " =   "black"),
    guide="none")+  
  theme_light()+
  labs(fill = 'Trend significance   ', x = "Dataset", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_estimates_landcover.png"), 
       width = 8, height = 6)

## Biomes ----

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_trend_bootstrap.rds"))  
ensemble_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_trend_ensemble_bootstrap.rds"))  

ensemble_trend[, dataset := "ensemble"]

data_trend <- merge(data_trend, ensemble_trend, 
                    by = c("dataset", "biome_class", "p", "slope", "lower", "upper"),  all = T)

data_trend[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
data_trend[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
data_trend[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
data_trend[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
data_trend[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
data_trend[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
data_trend[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
data_trend[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
data_trend[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
data_trend[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
data_trend[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
data_trend[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
data_trend[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
data_trend[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
data_trend[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
data_trend[, biome_short_class := factor(biome_short_class)]
data_trend <- data_trend[complete.cases(data_trend)]
data_trend[slope >= 0 , trend_direction_detailed :=   "pos. p <= 1   "]
data_trend[slope > 0 & p <= 0.1 , trend_direction_detailed :=   "pos. p <= 0.1   "]
data_trend[slope > 0 & p <= 0.05 , trend_direction_detailed :=   "pos. p <= 0.05   "]
data_trend[slope > 0 & p <= 0.01 , trend_direction_detailed :=   "pos. p <= 0.01   "]
data_trend[slope < 0  , trend_direction_detailed :=   "neg. p <= 1   "]
data_trend[slope < 0 & p <= 0.1 , trend_direction_detailed :=   "neg. p <= 0.1   "]
data_trend[slope < 0 & p <= 0.05 , trend_direction_detailed :=   "neg. p <= 0.05   "]
data_trend[slope < 0 & p <= 0.01 , trend_direction_detailed :=   "neg. p <= 0.01   "]
data_trend[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                 level = c("pos. p <= 0.01   ",   "pos. p <= 0.05   ",   "pos. p <= 0.1   ",
                                                           "pos. p <= 1   ",
                                                           "neg. p <= 1   ",   "neg. p <= 0.1   ",   "neg. p <= 0.05   ",
                                                           "neg. p <= 0.01   "), ordered = T),]

data_trend_slopes <- data_trend[, .(mean_slope = mean(slope)), dataset]
data_trend_slopes[, rank := rank(mean_slope)]
data_trend_slopes[dataset == "ensemble", rank := 0]
data_trend_slopes <- data_trend_slopes[order(rank)]
data_trend[, dataset := factor(dataset, levels = data_trend_slopes$dataset)]


ggplot(data_trend[biome_short_class != "N/A"])+
  geom_tile(aes(x = dataset, 
                y = biome_short_class, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = biome_short_class, 
                col = trend_direction_detailed))+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
    "neg. p <= 0.05   " =   "royalblue3", 
    "neg. p <= 0.01   " =   "darkblue", 
    "pos. p <= 0.01   " =   "#330000",
    "pos. p <= 0.05   " =   "darkred",
    "pos. p <= 0.1   " =   "lightcoral",
    "pos. p <= 1   " =   "orange"))+  
  scale_color_manual(values = c(
    "neg. p <= 1   " =   "black",
    "neg. p <= 0.1   " =   "black", 
    "neg. p <= 0.05   " =   "white", 
    "neg. p <= 0.01   " =   "white", 
    "pos. p <= 0.01   " =   "white",
    "pos. p <= 0.05   " =   "white",
    "pos. p <= 0.1   " =   "black",
    "pos. p <= 1   " =   "black"),
    guide="none")+  
  theme_light()+
  labs(fill = 'Trend significance   ', x = "Dataset", y = "Biome")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_estimates_biomes.png"), 
       width = 8, height = 8)

## IPCC reference regions ----

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_trend_bootstrap.rds"))  
ensemble_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_ensemble_trend_bootstrap.rds"))  

ensemble_trend[, dataset := "ensemble"]

data_trend <- merge(data_trend, ensemble_trend, 
                    by = c("dataset", "IPCC_ref_region", "p", "slope", "lower", "upper"),  all = T)


data_trend[slope >= 0 , trend_direction_detailed :=   "pos. p <= 1   "]
data_trend[slope > 0 & p <= 0.1 , trend_direction_detailed :=   "pos. p <= 0.1   "]
data_trend[slope > 0 & p <= 0.05 , trend_direction_detailed :=   "pos. p <= 0.05   "]
data_trend[slope > 0 & p <= 0.01 , trend_direction_detailed :=   "pos. p <= 0.01   "]
data_trend[slope < 0  , trend_direction_detailed :=   "neg. p <= 1   "]
data_trend[slope < 0 & p <= 0.1 , trend_direction_detailed :=   "neg. p <= 0.1   "]
data_trend[slope < 0 & p <= 0.05 , trend_direction_detailed :=   "neg. p <= 0.05   "]
data_trend[slope < 0 & p <= 0.01 , trend_direction_detailed :=   "neg. p <= 0.01   "]
data_trend[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                 level = c("pos. p <= 0.01   ",   "pos. p <= 0.05   ",   "pos. p <= 0.1   ",
                                                           "pos. p <= 1   ",
                                                           "neg. p <= 1   ",   "neg. p <= 0.1   ",   "neg. p <= 0.05   ",
                                                           "neg. p <= 0.01   "), ordered = T),]

data_trend_slopes <- data_trend[, .(mean_slope = mean(slope)), dataset]
data_trend_slopes[, rank := rank(mean_slope)]
data_trend_slopes[dataset == "ensemble", rank := 0]
data_trend_slopes <- data_trend_slopes[order(rank)]
data_trend[, dataset := factor(dataset, levels = data_trend_slopes$dataset)]

data_trend[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
data_trend[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
data_trend[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
data_trend[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
data_trend[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
data_trend[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]
data_trend <- data_trend[!is.na(IPCC_ref_region)]

ggplot(data_trend)+
  geom_tile(aes(x = dataset, 
                y = IPCC_ref_region, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = IPCC_ref_region, 
                col = trend_direction_detailed))+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
    "neg. p <= 0.05   " =   "royalblue3", 
    "neg. p <= 0.01   " =   "darkblue", 
    "pos. p <= 0.01   " =   "#330000",
    "pos. p <= 0.05   " =   "darkred",
    "pos. p <= 0.1   " =   "lightcoral",
    "pos. p <= 1   " =   "orange"))+  
  scale_color_manual(values = c(
    "neg. p <= 1   " =   "black",
    "neg. p <= 0.1   " =   "black", 
    "neg. p <= 0.05   " =   "white", 
    "neg. p <= 0.01   " =   "white", 
    "pos. p <= 0.01   " =   "white",
    "pos. p <= 0.05   " =   "white",
    "pos. p <= 0.1   " =   "black",
    "pos. p <= 1   " =   "black"),
    guide="none")+  
  theme_bw()+
  labs(fill = 'Trend significance   ', x = "Dataset", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  facet_grid(rows = vars(region), scales = "free", space = "free")

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_estimates_ipcc.png"), 
       width = 8, height = 12)


## Elevation ----

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_trend_bootstrap.rds"))  
ensemble_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_ensemble_trend_bootstrap.rds"))  

ensemble_trend[, dataset := "ensemble"]

data_trend <- merge(data_trend, ensemble_trend, 
                    by = c("dataset", "elev_class", "p", "slope", "lower", "upper"),  all = T)


data_trend[slope >= 0 , trend_direction_detailed :=   "pos. p <= 1   "]
data_trend[slope > 0 & p <= 0.1 , trend_direction_detailed :=   "pos. p <= 0.1   "]
data_trend[slope > 0 & p <= 0.05 , trend_direction_detailed :=   "pos. p <= 0.05   "]
data_trend[slope > 0 & p <= 0.01 , trend_direction_detailed :=   "pos. p <= 0.01   "]
data_trend[slope < 0  , trend_direction_detailed :=   "neg. p <= 1   "]
data_trend[slope < 0 & p <= 0.1 , trend_direction_detailed :=   "neg. p <= 0.1   "]
data_trend[slope < 0 & p <= 0.05 , trend_direction_detailed :=   "neg. p <= 0.05   "]
data_trend[slope < 0 & p <= 0.01 , trend_direction_detailed :=   "neg. p <= 0.01   "]
data_trend[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                 level = c("pos. p <= 0.01   ",   "pos. p <= 0.05   ",   "pos. p <= 0.1   ",
                                                           "pos. p <= 1   ",
                                                           "neg. p <= 1   ",   "neg. p <= 0.1   ",   "neg. p <= 0.05   ",
                                                           "neg. p <= 0.01   "), ordered = T),]

data_trend_slopes <- data_trend[, .(mean_slope = mean(slope)), dataset]
data_trend_slopes[, rank := rank(mean_slope)]
data_trend_slopes[dataset == "ensemble", rank := 0]
data_trend_slopes <- data_trend_slopes[order(rank)]
data_trend[, dataset := factor(dataset, levels = data_trend_slopes$dataset)]


ggplot(data_trend)+
  geom_tile(aes(x = dataset, 
                y = elev_class, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = elev_class, 
                col = trend_direction_detailed))+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
    "neg. p <= 0.05   " =   "royalblue3", 
    "neg. p <= 0.01   " =   "darkblue", 
    "pos. p <= 0.01   " =   "#330000",
    "pos. p <= 0.05   " =   "darkred",
    "pos. p <= 0.1   " =   "lightcoral",
    "pos. p <= 1   " =   "orange"))+  
  scale_color_manual(values = c(
    "neg. p <= 1   " =   "black",
    "neg. p <= 0.1   " =   "black", 
    "neg. p <= 0.05   " =   "white", 
    "neg. p <= 0.01   " =   "white", 
    "pos. p <= 0.01   " =   "white",
    "pos. p <= 0.05   " =   "white",
    "pos. p <= 0.1   " =   "black",
    "pos. p <= 1   " =   "black"),
    guide="none")+  
  theme_light()+
  labs(fill = 'Trend significance   ', x = "Dataset", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_estimates_elevation.png"), 
       width = 8, height = 4)

## KG classes ----

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_trend_bootstrap.rds"))  
ensemble_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_ensemble_trend_bootstrap.rds"))  

ensemble_trend[, dataset := "ensemble"]

data_trend <- merge(data_trend, ensemble_trend, 
                    by = c("dataset", "KG_class_3", "p", "slope", "lower", "upper"),  all = T)


data_trend[slope >= 0 , trend_direction_detailed :=   "pos. p <= 1   "]
data_trend[slope > 0 & p <= 0.1 , trend_direction_detailed :=   "pos. p <= 0.1   "]
data_trend[slope > 0 & p <= 0.05 , trend_direction_detailed :=   "pos. p <= 0.05   "]
data_trend[slope > 0 & p <= 0.01 , trend_direction_detailed :=   "pos. p <= 0.01   "]
data_trend[slope < 0  , trend_direction_detailed :=   "neg. p <= 1   "]
data_trend[slope < 0 & p <= 0.1 , trend_direction_detailed :=   "neg. p <= 0.1   "]
data_trend[slope < 0 & p <= 0.05 , trend_direction_detailed :=   "neg. p <= 0.05   "]
data_trend[slope < 0 & p <= 0.01 , trend_direction_detailed :=   "neg. p <= 0.01   "]
data_trend[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                 level = c("pos. p <= 0.01   ",   "pos. p <= 0.05   ",   "pos. p <= 0.1   ",
                                                           "pos. p <= 1   ",
                                                           "neg. p <= 1   ",   "neg. p <= 0.1   ",   "neg. p <= 0.05   ",
                                                           "neg. p <= 0.01   "), ordered = T),]

data_trend_slopes <- data_trend[, .(mean_slope = mean(slope)), dataset]
data_trend_slopes[, rank := rank(mean_slope)]
data_trend_slopes[dataset == "ensemble", rank := 0]
data_trend_slopes <- data_trend_slopes[order(rank)]
data_trend[, dataset := factor(dataset, levels = data_trend_slopes$dataset)]
data_trend <- data_trend[!is.na(KG_class_3)]

data_trend[KG_class_3 %in% c("Af", "Am", "As", "Aw"), climate := "Equatorial"]
data_trend[KG_class_3 %in% c("BSh", "BSk", "BWh", "BWk"), climate := "Arid"]
data_trend[KG_class_3 %in% c("Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc"), climate := "Warm temperate"]
data_trend[KG_class_3 %in% c("Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd"), climate := "Boreal"]
data_trend[KG_class_3 %in% c("EF",  "ET"), climate := "Polar"]

ggplot(data_trend)+
  geom_tile(aes(x = dataset, 
                y = KG_class_3, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = KG_class_3, 
                col = trend_direction_detailed))+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
    "neg. p <= 0.05   " =   "royalblue3", 
    "neg. p <= 0.01   " =   "darkblue", 
    "pos. p <= 0.01   " =   "#330000",
    "pos. p <= 0.05   " =   "darkred",
    "pos. p <= 0.1   " =   "lightcoral",
    "pos. p <= 1   " =   "orange"))+  
  scale_color_manual(values = c(
    "neg. p <= 1   " =   "black",
    "neg. p <= 0.1   " =   "black", 
    "neg. p <= 0.05   " =   "white", 
    "neg. p <= 0.01   " =   "white", 
    "pos. p <= 0.01   " =   "white",
    "pos. p <= 0.05   " =   "white",
    "pos. p <= 0.1   " =   "black",
    "pos. p <= 1   " =   "black"),
    guide="none")+  
  theme_bw()+
  labs(fill = 'Trend significance   ', x = "Dataset", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  facet_grid(rows = vars(climate), scales = "free", space = "free")

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_estimates_KG_class_3.png"), 
       width = 8, height = 12)
