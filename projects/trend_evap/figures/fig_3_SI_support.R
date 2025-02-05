# SI support for figure 3
source('source/evap_trend.R')
source('source/geo_functions.R')


## Data ----
### Input data generated in trend_evap/bootstrap/01_c 
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND,   "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend <- evap_trend[dataset_count >= 14]

grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend <- grid_cell_area[evap_trend, on = .(lon, lat)]

evap_trend[slope >= 0 , trend_direction_detailed :=   "pos. p <= 1   "]
evap_trend[slope > 0 & p <= 0.1 , trend_direction_detailed :=   "pos. p <= 0.1   "]
evap_trend[slope > 0 & p <= 0.05 , trend_direction_detailed :=   "pos. p <= 0.05   "]
evap_trend[slope > 0 & p <= 0.01 , trend_direction_detailed :=   "pos. p <= 0.01   "]
evap_trend[slope < 0  , trend_direction_detailed :=   "neg. p <= 1   "]
evap_trend[slope < 0 & p <= 0.1 , trend_direction_detailed :=   "neg. p <= 0.1   "]
evap_trend[slope < 0 & p <= 0.05 , trend_direction_detailed :=   "neg. p <= 0.05   "]
evap_trend[slope < 0 & p <= 0.01 , trend_direction_detailed :=   "neg. p <= 0.01   "]
evap_trend[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                       level = c("pos. p <= 0.01   ",   "pos. p <= 0.05   ",   "pos. p <= 0.1   ",
                                                                   "pos. p <= 1   ",
                                                                   "neg. p <= 1   ",   "neg. p <= 0.1   ",   "neg. p <= 0.05   ",
                                                                    "neg. p <= 0.01   "), ordered = T),]


evap_trend_area <- evap_trend[,.(trend_area = sum(area)),.(trend_direction_detailed, dataset)]
evap_trend_area <- evap_trend_area[complete.cases(evap_trend_area)]
evap_trend_area[, total_area:= sum(trend_area), .(dataset)]
evap_trend_area[, fraction:= trend_area/total_area]
random <- evap_trend_area[dataset ==   "bess"]
random[, dataset :=   "random"]

random$fraction <- c(0.5-0.1, 0.1-0.05, 0.5-0.1, 0.05-0.01, 0.1-0.05, 0.05-0.01, 0.01, 0.01)
random[, trend_area := total_area * fraction]

evap_trend_area <- merge(evap_trend_area, random, by = c("dataset",   "total_area",   "trend_area","fraction",   "trend_direction_detailed"), all = T)
saveRDS(evap_trend_area, paste0(PATH_SAVE_EVAP_TREND_TABLES,   "data_SI_fig_3_area_fraction_trend_significance_by_product.rds"))

fig_datasets <- ggplot(evap_trend_area[dataset !=   "random"])+
  geom_bar(aes(x = dataset, y = fraction, fill = trend_direction_detailed), stat =   "identity") +
  xlab('Dataset')  +
  ylab('Area fraction [-]')  +
  labs(fill = 'Trend significance   ')  +
  scale_fill_manual(values = c(
      "neg. p <= 1   " =   "lightblue",
      "neg. p <= 0.1   " =   "royalblue1", 
      "neg. p <= 0.05   " =   "royalblue3", 
      "neg. p <= 0.01   " =   "darkblue", 
      "pos. p <= 0.01   " =   "#330000",
      "pos. p <= 0.05   " =   "darkred",
      "pos. p <= 0.1   " =   "lightcoral",
      "pos. p <= 1   " =   "orange"))+  theme_light() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0,   "cm"),
        panel.grid.major = element_line(colour =   "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

fig_random <- ggplot(evap_trend_area[dataset ==   "random"])+
  geom_bar(aes(x = dataset, y = fraction, fill = trend_direction_detailed), stat =   "identity") +
  xlab('Dataset')  +
  ylab('Area fraction [-]')  +
  labs(fill = 'Trend significance   ')  +
  scale_fill_manual(values = c(
      "neg. p <= 1   " =   "lightblue",
      "neg. p <= 0.1   " =   "royalblue1", 
      "neg. p <= 0.05   " =   "royalblue3", 
      "neg. p <= 0.01   " =   "darkblue", 
      "pos. p <= 0.01   " =   "#330000",
      "pos. p <= 0.05   " =   "darkred",
      "pos. p <= 0.1   " =   "lightcoral",
      "pos. p <= 1   " =   "orange"))+  theme_light() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0,   "cm"),
        panel.grid.major = element_line(colour =   "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggarrange(fig_datasets, fig_random, common.legend = T, labels = c("a",   "b"), widths = c(0.9, 0.125), align =   "h")

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP,   "SI_fig_3_support_evap_trend_direction_detailed_per_dataset_bootstrap.png"), 
       width = 12, height = 8)
