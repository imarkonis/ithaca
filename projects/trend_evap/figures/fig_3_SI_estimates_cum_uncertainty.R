# Ensemble trend and then product trends with significance in tile format ----
source('source/evap_trend.R')
source('source/geo_functions.R')
### colors
cols_problem <- c("Direction and Magnitude" = "#330000", "Direction" = "darkred","Magnitude" = "orange2", 
                  "Small trend - Direction" ="royalblue2", 
                  "Small trend - Magnitude" = "lightblue", "None" = "darkblue")

### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "different sign", problem := "Direction and Magnitude"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "different sign", problem := "Direction"] 

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "same sign" & (abs(Q25) >= 1 | abs(Q75) >= 1), problem := "Magnitude"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "same sign", problem := "None"] 

evap_trend_stats[sign == "different sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Direction"] 

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "same sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Magnitude"] 

evap_trend_stats[, problem:= as.factor(problem)]

evap_trend_masks <- merge(evap_trend_stats, evap_mask, all.x = T, by = c("lon", "lat"))

grid_cell_area <- unique(evap_trend_masks[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_masks <- grid_cell_area[evap_trend_masks, on = .(lon, lat)]


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



land_slopes <- ggplot(data_trend[land_cover_short_class != "Other"])+
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
  theme_bw()+
  labs(fill = 'Trend significance   ', x = "Dataset", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

### uncertainty for entire rgion ----

data_trend_env <- data_trend[dataset != "ensemble",.(Q25 = quantile(slope, 0.25),
                                                     Q75 = quantile(slope, 0.75)), land_cover_short_class]
data_trend_env[, fold := abs(Q75)/abs(Q25)]
data_trend_env[abs(Q25) > abs(Q75), fold := abs(Q25)/abs(Q75)]
data_trend_env[Q75/Q25 < 0, sign := "different sign"]
data_trend_env[Q75/Q25 >= 0, sign := "same sign"]

data_trend_env[fold > 3.2 & sign == "different sign", problem := "Direction and Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "different sign", problem := "Direction"] 
data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) >= 1 | abs(Q75) >= 1), problem := "Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "same sign", problem := "None"] 
data_trend_env[sign == "different sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Direction"] 

data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Magnitude"] 

data_trend_env[, problem:= as.factor(problem)]

land_problems_agg <- ggplot(data_trend_env[land_cover_short_class != "Other"])+
  geom_bar(aes(x = 1, 
               y = land_cover_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = "none")

### problems  ----
land_trends <- evap_trend_masks[,.(problem_area = sum(area)),.(problem, land_cover_short_class)]
land_trends <- land_trends[complete.cases(land_cover_short_class)]
land_trends[, land_area:= sum(problem_area), .(land_cover_short_class)]
land_trends[, land_fraction:= problem_area/land_area]
land_trends <- land_trends[!is.na(land_cover_short_class)]

land_problems <- ggplot(land_trends[land_cover_short_class != "Other"])+
  geom_bar(aes(x = land_fraction, 
               y = land_cover_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")))+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

### plot ----
ggarrange(land_slopes, land_problems, land_problems_agg, align = "h", legend = "bottom", widths = c(1, 0.8,0.13), 
          labels = c("a", "b", "c"), ncol = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_problem_landcover.png"), 
       width = 13, height = 6)


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


biome_slopes <- ggplot(data_trend[biome_short_class != "N/A"])+
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
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.margin = unit(c(0.5,0.0,0,0.5), "cm"))+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

### uncertainty for entire region ----

data_trend_env <- data_trend[dataset != "ensemble",.(Q25 = quantile(slope, 0.25),
                                                     Q75 = quantile(slope, 0.75)), biome_short_class]
data_trend_env[, fold := abs(Q75)/abs(Q25)]
data_trend_env[abs(Q25) > abs(Q75), fold := abs(Q25)/abs(Q75)]
data_trend_env[Q75/Q25 < 0, sign := "different sign"]
data_trend_env[Q75/Q25 >= 0, sign := "same sign"]

data_trend_env[fold > 3.2 & sign == "different sign", problem := "Direction and Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "different sign", problem := "Direction"] 
data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) >= 1 | abs(Q75) >= 1), problem := "Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "same sign", problem := "None"] 
data_trend_env[sign == "different sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Direction"] 

data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Magnitude"] 

data_trend_env[, problem:= as.factor(problem)]

biome_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = biome_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  guides(fill = "none")

### problems  ----
biome_levels <- levels(data_trend$biome_short_class)
biome_trends <- evap_trend_masks[,.(problem_area = sum(area)),.(problem, biome_short_class)]
biome_trends <- biome_trends[complete.cases(biome_short_class)]
biome_trends[, biome_area:= sum(problem_area), .(biome_short_class)]
biome_trends[, biome_fraction:= problem_area/biome_area]
biome_trends <- biome_trends[!is.na(biome_short_class)]
biome_trends[, biome_short_class := factor(biome_short_class, levels = biome_levels)]

biome_problems <- ggplot(biome_trends)+
  geom_bar(aes(x = biome_fraction, 
               y = biome_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction", y = "Biome")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")))+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

### plot ----
ggarrange(biome_slopes, biome_problems, biome_problems_agg, align = "h", legend = "bottom",
          widths = c(1,0.8, 0.13), labels = c("a", "b", "c"), ncol = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_problem_biome.png"), 
       width = 14, height = 10)

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

ipcc_slopes <- ggplot(data_trend)+
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
  labs(fill = 'Trend \nsignificance   ', x = "Dataset", y = "IPCC region")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  facet_grid(rows = vars(region), scales = "free", space = "free")+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

### uncertainty for entire region ----

data_trend_env <- data_trend[dataset != "ensemble",.(Q25 = quantile(slope, 0.25),
                                                     Q75 = quantile(slope, 0.75)), IPCC_ref_region]
data_trend_env[, fold := abs(Q75)/abs(Q25)]
data_trend_env[abs(Q25) > abs(Q75), fold := abs(Q25)/abs(Q75)]
data_trend_env[Q75/Q25 < 0, sign := "different sign"]
data_trend_env[Q75/Q25 >= 0, sign := "same sign"]

data_trend_env[fold > 3.2 & sign == "different sign", problem := "Direction and Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "different sign", problem := "Direction"] 
data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) >= 1 | abs(Q75) >= 1), problem := "Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "same sign", problem := "None"] 
data_trend_env[sign == "different sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Direction"] 

data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Magnitude"] 

data_trend_env[, problem:= as.factor(problem)]
data_trend_env[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
data_trend_env[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
data_trend_env[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
data_trend_env[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
data_trend_env[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
data_trend_env[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]

ipcc_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = IPCC_ref_region, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(rows = vars(region), scales = "free", space = "free")+
  guides(fill = "none")

### problems  ----
ipcc_trends <- evap_trend_masks[,.(problem_area = sum(area)),.(problem, IPCC_ref_region)]
ipcc_trends <- ipcc_trends[complete.cases(ipcc_trends)]
ipcc_trends[, ipcc_area:= sum(problem_area), .(IPCC_ref_region)]
ipcc_trends[, ipcc_fraction:= problem_area/ipcc_area]
ipcc_trends[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
ipcc_trends[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
ipcc_trends[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
ipcc_trends[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
ipcc_trends[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
ipcc_trends[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]
ipcc_trends <- ipcc_trends[!is.na(IPCC_ref_region)]

ipcc_problems <- ggplot(ipcc_trends)+
  geom_bar(aes(x = ipcc_fraction, 
                y = IPCC_ref_region, 
                fill = problem), 
            color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction", y = "IPCC region")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")))+
  facet_grid(rows = vars(region), scales = "free", space = "free")+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

### plot ----
ggarrange(ipcc_slopes, ipcc_problems, ipcc_problems_agg, align = "h", legend = "bottom", widths = c(1,0.8, 0.13), 
          labels = c("a", "b", "c"), ncol = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_problem_ipcc.png"), 
       width = 13, height = 12)


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


elevation_slopes <- ggplot(data_trend)+
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

### uncertainty for entire region ----

data_trend_env <- data_trend[dataset != "ensemble",.(Q25 = quantile(slope, 0.25),
                                                     Q75 = quantile(slope, 0.75)), elev_class]
data_trend_env[, fold := abs(Q75)/abs(Q25)]
data_trend_env[abs(Q25) > abs(Q75), fold := abs(Q25)/abs(Q75)]
data_trend_env[Q75/Q25 < 0, sign := "different sign"]
data_trend_env[Q75/Q25 >= 0, sign := "same sign"]

data_trend_env[fold > 3.2 & sign == "different sign", problem := "Direction and Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "different sign", problem := "Direction"] 
data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) >= 1 | abs(Q75) >= 1), problem := "Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "same sign", problem := "None"] 
data_trend_env[sign == "different sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Direction"] 

data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Magnitude"] 

data_trend_env[, problem:= as.factor(problem)]

elevation_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = elev_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = "none")

### problems  ----
elev_levels <- levels(data_trend$elev_class)
elev_trends <- evap_trend_masks[,.(problem_area = sum(area)),.(problem, elev_class)]
elev_trends <- elev_trends[complete.cases(elev_class)]
elev_trends[, elev_area:= sum(problem_area), .(elev_class)]
elev_trends[, elev_fraction:= problem_area/elev_area]
elev_trends <- elev_trends[!is.na(elev_class)]

elevation_problems <- ggplot(elev_trends)+
  geom_bar(aes(x = elev_fraction, 
               y = elev_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction", y = "Elevation")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")))+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))


### plot ----
ggarrange(elevation_slopes, elevation_problems, elevation_problems_agg , align = "h", legend = "bottom", 
          widths = c(1,0.8, 0.13), labels = c("a", "b", "c"), ncol = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_problem_elevation.png"), 
       width = 13, height = 5)
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

KG_slopes <- ggplot(data_trend)+
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

### uncertainty for entire region ----

data_trend_env <- data_trend[dataset != "ensemble",.(Q25 = quantile(slope, 0.25),
                                                     Q75 = quantile(slope, 0.75)), KG_class_3]
data_trend_env[, fold := abs(Q75)/abs(Q25)]
data_trend_env[abs(Q25) > abs(Q75), fold := abs(Q25)/abs(Q75)]
data_trend_env[Q75/Q25 < 0, sign := "different sign"]
data_trend_env[Q75/Q25 >= 0, sign := "same sign"]

data_trend_env[fold > 3.2 & sign == "different sign", problem := "Direction and Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "different sign", problem := "Direction"] 
data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) >= 1 | abs(Q75) >= 1), problem := "Magnitude"] 
data_trend_env[fold <= 3.2 & sign == "same sign", problem := "None"] 
data_trend_env[sign == "different sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Direction"] 

data_trend_env[fold > 3.2 & sign == "same sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Magnitude"] 

data_trend_env[, problem:= as.factor(problem)]

data_trend_env[KG_class_3 %in% c("Af", "Am", "As", "Aw"), climate := "Equatorial"]
data_trend_env[KG_class_3 %in% c("BSh", "BSk", "BWh", "BWk"), climate := "Arid"]
data_trend_env[KG_class_3 %in% c("Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc"), climate := "Warm temperate"]
data_trend_env[KG_class_3 %in% c("Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd"), climate := "Boreal"]
data_trend_env[KG_class_3 %in% c("EF",  "ET"), climate := "Polar"]

KG_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = KG_class_3, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = "none")+
  facet_grid(rows = vars(climate), scales = "free", space = "free")

### problems  ----
KG_trends <- evap_trend_masks[,.(problem_area = sum(area)),.(problem, KG_class_3)]
KG_trends <- KG_trends[complete.cases(KG_trends)]
KG_trends[, KG_area:= sum(problem_area), .(KG_class_3)]
KG_trends[, KG_fraction:= problem_area/KG_area]
KG_trends[KG_class_3 %in% c("Af", "Am", "As", "Aw"), climate := "Equatorial"]
KG_trends[KG_class_3 %in% c("BSh", "BSk", "BWh", "BWk"), climate := "Arid"]
KG_trends[KG_class_3 %in% c("Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc"), climate := "Warm temperate"]
KG_trends[KG_class_3 %in% c("Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd"), climate := "Boreal"]
KG_trends[KG_class_3 %in% c("EF",  "ET"), climate := "Polar"]

KG_trends <- KG_trends[!is.na(KG_class_3)]

KG_problems <- ggplot(KG_trends)+
  geom_bar(aes(x = KG_fraction, 
               y = KG_class_3, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction", y = "Koeppen-Geiger")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")))+
  facet_grid(rows = vars(climate), scales = "free", space = "free")+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

### plot ----
ggarrange(KG_slopes, KG_problems,  KG_problems_agg, align = "h", legend = "bottom", 
          widths = c(1,0.8, 0.13), labels = c("a", "b", "c"), ncol = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_slope_problem_KG_class_3.png"), 
       width = 13, height = 12)
