source('source/evap_trend.R')
source('source/geo_functions.R')

library(scatterpie)
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "different sign", problem := "High variety & different sign"] 

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "same sign", problem := "High variety & same sign"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "different sign" & abs(Q25) > 0.5 & abs(Q75) > 0.5, problem := "Low variety & different sign & large trend"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "same sign", problem := "Low variety & same sign"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "different sign" & (abs(Q25) < 0.5 | abs(Q75) < 0.5), problem := "Low variety & different sign & small trend"] 

evap_trend_stats[, problem:= as.factor(problem)]

evap_trend_masks <- merge(evap_trend_stats, evap_mask, all.x = T, by = c("lon", "lat"))

grid_cell_area <- unique(evap_trend_masks[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_masks <- grid_cell_area[evap_trend_masks, on = .(lon, lat)]

### ipcc ref regions  ----
ipcc_trends <- evap_trend_masks[,.(problem_area = sum(area)),.(problem, IPCC_ref_region)]
ipcc_trends <- ipcc_trends[complete.cases(ipcc_trends)]
ipcc_trends[, ipcc_area:= sum(problem_area), .(IPCC_ref_region)]
ipcc_trends[, ipcc_fraction:= problem_area/ipcc_area]

ipcc_trends_cast <- dcast(ipcc_trends, IPCC_ref_region ~ problem, value.var = "ipcc_fraction")
ipcc_trends_cast[is.na(ipcc_trends_cast)] <- 0

ipcc_hexagon <- as.data.table(read.csv(paste0(PATH_IPCC_data,"/gloabl_ipcc_ref_hexagons.csv"))) #don't use fread
ipcc_mean_lon_lat <- unique(ipcc_hexagon[,.(V1,V2,Acronym)])

ipcc_trends_cast <- merge(ipcc_trends, ipcc_mean_lon_lat, by.x = c("IPCC_ref_region"), by.y = "Acronym")
ipcc_trends_cast[, value := ipcc_fraction]
ipcc_trends_cast[, radius := 11.5]

cols_problem <- c("#330000","orange", "darkred", "lightblue", "darkblue")

ggplot(ipcc_trends_cast)+
   geom_scatterpie(aes(x = V1, y = V2, r = radius), 
                   long_format = TRUE, 
                   data = ipcc_trends_cast,
                   cols = "problem", 
                   donut_radius=.5)+
  geom_text(aes(V1, V2, label = IPCC_ref_region), size = 3.5, color = "Black") +
  theme_bw()+
  coord_fixed()+
  labs(x = "lon", y = "lat", fill = "Problem")+
  scale_fill_manual(values = cols_problem)

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig_1_SI_ipcc_problems.png"),
       width = 15, height = 7, units = "in", dpi = 600)
