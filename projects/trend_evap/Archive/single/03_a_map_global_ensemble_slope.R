# Maps of global ensemble slope
source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/evap_trend/01_a
evap_trend_grid_ensemble <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_trend_ensemble_grid.rds"))  

## Global map of ensemble trend and significant trend ----
evap_trend_grid_ensemble[, theil_sen_slope_sig_brk:= cut(theil_sen_slope*significant_theil_sen, breaks = c(min(theil_sen_slope), -5, -3, -0.1, 0.1, 3, 5, max(theil_sen_slope)))]
evap_trend_grid_ensemble[, theil_sen_slope_brk:= cut(theil_sen_slope, breaks = c(min(theil_sen_slope), -5, -3, -0.1, 0.1, 3, 5, max(theil_sen_slope)))]
evap_trend_grid_ensemble[, slope_percent_brk:= cut(slope_percent, breaks = c(-5, -2, -1, -0.1, 0.1, 1, 2, 10))]

cols_grid_ensemble <- c("darkblue", 
                                  "steelblue3",
                                  "steelblue1", 
                                  "gray80",
                                  "gold",
                                  "darkorange",
                                  "darkred"
)

## Global map of significant trend ----
ggplot(evap_trend_grid_ensemble)+
  geom_tile(aes(x = lon, y = lat, fill = theil_sen_slope_sig_brk))+
  scale_fill_manual(values = cols_grid_ensemble)+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_EXPLORE, "map_evap_ensemble_trend_theil_sen_significant.png"), 
       width = 12, height = 8)

## Global map of all trends ----
ggplot(evap_trend_grid_ensemble)+
  geom_tile(aes(x = lon, y = lat, fill = theil_sen_slope_brk))+
  scale_fill_manual(values = cols_grid_ensemble)+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_ensemble_trend_theil_sen.png"), 
       width = 12, height = 8)

ggplot(evap_trend_grid_ensemble)+
  geom_tile(aes(x = lon, y = lat, fill = slope_percent_brk))+
  scale_fill_manual(values = cols_grid_ensemble)+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "map_evap_ensemble_trend_theil_sen_percent.png"), 
       width = 12, height = 8)
