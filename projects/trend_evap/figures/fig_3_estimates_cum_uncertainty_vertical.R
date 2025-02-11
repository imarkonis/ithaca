# Figure 3 ----
source('source/evap_trend.R')

## colors ----
cols_problem <- c("Direction\nmagnitude" = "#330000", "Direction" = "darkred","Magnitude" = "orange2", 
                  "Small trend:\ndirection" ="royalblue2", 
                  "Small trend:\nmagnitude" = "lightblue", "None" = "darkblue")


## landcover ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_a_land_cover_trends_by_product.rds"))
land_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_b_land_cover_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_c_land_cover_problem_aggregated.rds"))


### trends ----
land_slopes <- ggplot(data_trend)+
  geom_tile(aes(y = dataset, 
                x = land_cover_short_class, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), y = dataset, 
                x = land_cover_short_class, 
                col = trend_direction_detailed), size = 6)+
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
  labs(fill = 'Trend significance   ', y = "Dataset", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
        )+
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

### uncertainty for entire region ----

land_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(y = 1, 
               x = land_cover_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "", x = "Landcover")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  guides(fill = "none")

### problems  ----
land_problems <- ggplot(land_trends)+
  geom_bar(aes(y = land_fraction, 
               x = land_cover_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "Area fraction", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 18,
          margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 18),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

### plot all ----
ggarrange(land_slopes, land_problems, land_problems_agg, align = "v", legend = "right", heights = c(1, 0.7,0.25), 
          labels = c("a", "b", "c"), font.label = list(size = 20), nrow = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig3_slope_problem_landcover_vertical.png"), 
       width = 12, height = 16)



## Biomes ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_biome_trends_by_product.rds"))
biome_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_biome_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_biome_problem_aggregated.rds"))

### trends ----
biome_slopes <- ggplot(data_trend[biome_short_class != "N/A"])+
  geom_tile(aes(y = dataset, 
                x = biome_short_class, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), y = dataset, 
                x = biome_short_class, 
                col = trend_direction_detailed), size = 4.5)+
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
  labs(fill = 'Trend significance   ', y = "Dataset", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.0,0,0.5), "cm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

### uncertainty for entire region ----

biome_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(y = 1, 
               x = biome_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "", x = "Biome")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 10, unit = "pt")),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  guides(fill = "none")

### problems  ----
biome_problems <- ggplot(biome_trends)+
  geom_bar(aes(y = biome_fraction, 
               x = biome_short_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "Area fraction", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

### plot all ----
ggarrange(biome_slopes, biome_problems, biome_problems_agg, align = "v", legend = "right", heights = c(1, 0.7,0.33), 
          labels = c("a", "b", "c"), font.label = list(size = 20), nrow = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig3_SI_slope_problem_biome_vertical.png"), 
       width = 11, height = 16)

## IPCC reference regions ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_IPCC_ref_regions_trends_by_product.rds"))
ipcc_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_IPCC_ref_regions_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_IPCC_ref_regions_problem_aggregated.rds"))


### trends ----
ipcc_slopes <- ggplot(data_trend)+
  geom_tile(aes(y = dataset, 
                x = IPCC_ref_region, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), y = dataset, 
                x = IPCC_ref_region, 
                col = trend_direction_detailed), size = 3.5)+
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
  labs(fill = 'Trend \nsignificance   ', x = "", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16,
                                   margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16))+
  facet_grid(cols = vars(region), scales = "free", space = "free")+
  guides(fill = guide_legend(ncol = 4, byrow = TRUE))

### uncertainty for entire region ----

ipcc_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(y = 1, 
               x = IPCC_ref_region, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "", x = "IPCC reference region")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 15, unit = "pt")),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16))+
  facet_grid(cols = vars(region), scales = "free", space = "free")+
  guides(fill = "none")

### problems  ----
ipcc_problems <- ggplot(ipcc_trends)+
  geom_bar(aes(y = ipcc_fraction, 
               x = IPCC_ref_region, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "Area fraction", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 20, unit = "pt")),        
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16))+
  facet_grid(cols = vars(region), scales = "free", space = "free")+
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))

### plot all ----
ggarrange(ipcc_slopes, ipcc_problems, ipcc_problems_agg, align = "v", legend = "bottom", heights = c(1, 0.7,0.25), 
          labels = c("a", "b", "c"), font.label = list(size = 20), nrow = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig3_SI_slope_problem_ipcc_vertical.png"), 
       width = 16, height = 16)


## Elevation ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_elevation_trends_by_product.rds"))
elev_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_elevation_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_elevation_problem_aggregated.rds"))


### trends ----
elevation_slopes <- ggplot(data_trend)+
  geom_tile(aes(y = dataset, 
                x = elev_class, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), y = dataset, 
                x = elev_class, 
                col = trend_direction_detailed), size = 5)+
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
  labs(fill = 'Trend significance   ', y = "Dataset", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### uncertainty for entire region ----
elevation_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(y = 1, 
               x = elev_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Elevation")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 10, unit = "pt")),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  guides(fill = "none")

### problems  ----
elevation_problems <- ggplot(elev_trends)+
  geom_bar(aes(y = elev_fraction, 
               x = elev_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "Area fraction", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))


### plot all ----
ggarrange(elevation_slopes, elevation_problems, elevation_problems_agg, align = "v", legend = "right", 
          heights = c(1, 0.7, 0.35), 
          labels = c("a", "b", "c"), font.label = list(size = 20), nrow = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig3_SI_slope_problem_elevation_vertical.png"), 
       width = 10, height = 12)

## KG classes ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_Koeppen_Geiger_trends_by_product.rds"))
KG_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_Koeppen_Geiger_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_Koeppen_Geiger_problem_aggregated.rds"))


KG_slopes <- ggplot(data_trend)+
  geom_tile(aes(y = dataset, 
                x = KG_class_3, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), y = dataset, 
                x = KG_class_3, 
                col = trend_direction_detailed), size = 4.5)+
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
  labs(fill = 'Trend significance   ', x = "", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16, 
                                   margin = margin(r = 20, unit = "pt")),
        strip.text = element_text(size = 16))+
  facet_grid(cols = vars(climate), scales = "free", space = "free")

### uncertainty for entire region ----

KG_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(y = 1, 
               x = KG_class_3, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Koeppen-Geiger", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16))+
  guides(fill = "none")+
  facet_grid(cols = vars(climate), scales = "free", space = "free")

### problems  ----

KG_problems <- ggplot(KG_trends)+
  geom_bar(aes(y = KG_fraction, 
               x = KG_class_3, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', y = "Area fraction", x = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
          margin = margin(r = 20, unit = "pt")),
        legend.title = element_text(size = 16), axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16))+
  facet_grid(cols = vars(climate), scales = "free", space = "free")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

### plot all ----
ggarrange(KG_slopes, KG_problems,  KG_problems_agg, align = "v", legend = "bottom", heights = c(1, 0.7, 0.25), 
          labels = c("a", "b", "c"), font.label = list(size = 20), nrow = 3)
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig3_SI_slope_problem_KG_class_3_vertical.png"), 
       width = 14, height = 13)
