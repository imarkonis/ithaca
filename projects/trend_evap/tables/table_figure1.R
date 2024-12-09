# Tables for figure 1 for Zenodo ----
source('source/evap_trend.R')

evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_a_global_evap_trend.rds"))

write.table(evap_annual_trend, paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_evap_trends_by_product.csv"), 
                                      sep = ",", dec = ".", row.names = FALSE)


evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

write.table(evap_trend_stats, paste0(PATH_SAVE_EVAP_TREND_TABLES, "grid_evap_trend_stats_for_maps.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
