# Tables for figure 2 for Zenodo ----

source('source/evap_trend.R')

## Data ----
### Maps ----
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_a_c_d_grid_trend_stats.rds"))

write.table(evap_index, paste0(PATH_SAVE_EVAP_TREND_TABLES, "grid_evap_trend_indices_for_maps.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

### bar plots ----
DCI_area_fraction <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_f_area_stats_DCI_all_trends.rds"))

write.table(DCI_area_fraction, paste0(PATH_SAVE_EVAP_TREND_TABLES, "DCI_area_fraction_across_p_value.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

trend_direction_area_fraction <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_d_area_stats_trend_direction.rds"))

write.table(trend_direction_area_fraction, paste0(PATH_SAVE_EVAP_TREND_TABLES, "trend_direction_area_fraction_across_p_value.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

N_sig_area <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_b_area_stats_significant_trend_count.rds"))

write.table(N_sig_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "number_significant_trends_area_fraction_across_p_value.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
