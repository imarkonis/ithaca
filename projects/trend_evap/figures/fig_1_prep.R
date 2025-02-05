# Figure 1 - prep the global overview ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Variation in global trends ----
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend_bootstrap.rds"))  
evap_annual_trend[p > 0.1 , trend_significance := "p > 0.1"]
evap_annual_trend[slope > 0 & p <= 0.1 , trend_significance := "positive p <= 0.1"]
evap_annual_trend[slope > 0 & p < 0.05 , trend_significance := "positive p < 0.05"]
evap_annual_trend[slope > 0 & p < 0.01 , trend_significance := "positive p < 0.01"]
evap_annual_trend[slope < 0 & p <= 0.1 , trend_significance := "negative p <= 0.1"]
evap_annual_trend[slope < 0 & p < 0.05 , trend_significance := "negative p < 0.05"]
evap_annual_trend[slope < 0 & p < 0.01 , trend_significance := "negative p < 0.01"]

### Table of range of trends ----
evap_annual_trend_fig_1 <- evap_annual_trend[, .(dataset, slope, trend_significance, lower, upper)][order(-slope),]


### Save evap data
saveRDS(evap_annual_trend_fig_1, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_a_global_evap_trend.rds"))


Q25_global <- evap_annual_trend[, quantile(slope, 0.25)]
Q75_global <- evap_annual_trend[, quantile(slope, 0.75)]
global_fold <- round(Q75_global/Q25_global, digit = 1)

## Quartile fold ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend_min_max <- evap_trend[dataset_count >= 12,.(max = max(slope), min = min(slope),
                                                       Q75 = quantile(slope, 0.75), Q25 = quantile(slope, 0.25)), .(lat,lon)]


evap_trend_min_max[abs(Q25) > Q75, fold := abs(Q25)/abs(Q75)]
evap_trend_min_max[abs(Q25) <= Q75, fold := abs(Q75)/abs(Q25)]
evap_trend_min_max[, fold_brk := cut(fold, breaks = c(1, global_fold, Inf))]
evap_trend_min_max[, fold_brk_detailed := cut(fold, breaks = c(1, 2, global_fold, 4, 5, 6, 7, 8, 9, 10, 20, Inf))]

## Quartile sign disagreement ----
evap_trend_min_max[Q75/Q25 >= 0 , sign := "same sign" ]
evap_trend_min_max[Q75/Q25 < 0 , sign := "different sign"]
evap_trend_min_max[, sign := factor(sign, levels = c("same sign", "different sign"))]

evap_sel <- subset(evap_trend_min_max, select = c("lon", "lat", "min", "max", "Q25", "Q75" ,"fold_brk", "fold_brk_detailed", "sign"))
saveRDS(evap_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

