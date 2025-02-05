# Figure 1 - prep the global overview ----
source('source/changing_prec.R')
source('source/geo_functions.R')

## Variation in global trends ----
prec_annual_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_b_prec_annual_trend_bootstrap.rds"))  
prec_annual_trend[p > 0.1 , trend_significance := "p > 0.1"]
prec_annual_trend[slope > 0 & p <= 0.1 , trend_significance := "positive p <= 0.1"]
prec_annual_trend[slope > 0 & p < 0.05 , trend_significance := "positive p < 0.05"]
prec_annual_trend[slope > 0 & p < 0.01 , trend_significance := "positive p < 0.01"]
prec_annual_trend[slope < 0 & p <= 0.1 , trend_significance := "negative p <= 0.1"]
prec_annual_trend[slope < 0 & p < 0.05 , trend_significance := "negative p < 0.05"]
prec_annual_trend[slope < 0 & p < 0.01 , trend_significance := "negative p < 0.01"]

### Table of range of trends ----
prec_annual_trend_fig_1 <- prec_annual_trend[, .(dataset, slope, trend_significance, lower, upper)][order(-slope),]

### Save prec data
saveRDS(prec_annual_trend_fig_1, paste0(PATH_SAVE_CHANGING_PREC_TABLES, "data_fig_1_a_global_prec_trend.rds"))

## Quartile fold ----
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  
prec_trend_min_max <- prec_trend[dataset_count >= 12,.(max = max(slope), min = min(slope),
                                                       Q75 = quantile(slope, 0.75), Q25 = quantile(slope, 0.25)), .(lat,lon)]


prec_trend_min_max[abs(Q25) > Q75, fold := abs(Q25)/abs(Q75)]
prec_trend_min_max[abs(Q25) <= Q75, fold := abs(Q75)/abs(Q25)]
prec_trend_min_max[, fold_brk := cut(fold, breaks = c(1, 3.4, Inf))]
prec_trend_min_max[, fold_brk_detailed := cut(fold, breaks = c(1, 2, 4, 8, Inf))]

## Quartile sign disagreement ----
prec_trend_min_max[Q75/Q25 >= 0 , sign := "same sign" ]
prec_trend_min_max[Q75/Q25 < 0 , sign := "different sign"]
prec_trend_min_max[, sign := factor(sign, levels = c("same sign", "different sign"))]

prec_sel <- subset(prec_trend_min_max, select = c("lon", "lat", "min", "max", "Q25", "Q75" ,"fold_brk", "fold_brk_detailed", "sign"))
saveRDS(prec_sel, paste0(PATH_SAVE_CHANGING_PREC_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

