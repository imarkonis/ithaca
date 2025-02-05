# Figure 2 - results depend product selection ----
source('source/evap_trend.R')
source('source/geo_functions.R')

# Data ----
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_opposing_trends_p_thresholds_bootstrap_opposers_allowed.rds"))

# Maps ---
evap_index[p_val_opposing == "1", p_val_opposing := "no opposing\ntrends"]
evap_index[p_val_opposing == ">0.2", p_val_opposing := "p <= 1"]
evap_index[p_val_opposing == "<=0.01", p_val_opposing := "p <= 0.01"]
evap_index[p_val_opposing == "<=0.05", p_val_opposing := "p <= 0.05"]
evap_index[p_val_opposing == "<=0.1", p_val_opposing := "p <= 0.1"]
evap_index[p_val_opposing == "<=0.2", p_val_opposing := "p <= 0.2"]

evap_index[, p_val_opposing := as.factor(p_val_opposing)]

evap_index[, p_val_opposing := factor(p_val_opposing, levels = 
                                        c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1", "no opposing\ntrends"))]

evap_index[N_none_0_2 >= 7, more_sig_trends := "p <= 1" ]
evap_index[N_none_0_2 < 7, more_sig_trends := "p <= 0.2" ]
evap_index[N_none_0_1 < 7, more_sig_trends := "p <= 0.1" ]
evap_index[N_none_0_05 < 7, more_sig_trends := "p <= 0.05" ]
evap_index[N_none_0_01 < 7, more_sig_trends := "p <= 0.01" ]
evap_index[, more_sig_trends := as.factor(more_sig_trends) ]
evap_index[, more_sig_trends := factor(more_sig_trends, levels = 
                                         c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))]

evap_index[, DCI_all_brk := cut(DCI_all, c(-1.01,-0.5,-0.07, 0.07,0.5,1))]
evap_index[DCI_all_brk == "(-1.01,-0.5]", DCI_all_brk := "[-1,-0.5]"]
evap_index[, DCI_all_brk:= factor(DCI_all_brk, levels = c("[-1,-0.5]", 
                                                          "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                          "(0.5,1]"))]


## Save subset of grid data ----
evap_index_sel <- subset(evap_index, select = c("lon", "lat", "DCI_all_brk", "more_sig_trends", "p_val_opposing"))
saveRDS(evap_index_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_a_c_d_grid_trend_stats.rds"))


# Area fractions ----
grid_cell_area <- unique(evap_index[, .(lon, lat)]) %>% grid_area() # m2

data_sel <- subset(evap_index, select = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all", "lon", "lat"))
setnames(data_sel, old = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_sel_melt <- melt(data_sel, 
                      measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"), 
                      id.vars = c("lon", "lat"))

data_sel_melt[, DCI_brk := cut(value, c(-1.01,-0.5,-0.07, 0.07,0.5,1))]
data_sel_melt <- grid_cell_area[data_sel_melt, on = .(lon, lat)]

data_dci_area <- data_sel_melt[, .(DCI_area = sum(area)), .(DCI_brk, variable)]
data_dci_area[, total_area := sum(DCI_area), variable]
data_dci_area[, fraction := DCI_area/total_area]
data_dci_area[DCI_brk == "(-1.01,-0.5]", DCI_brk := "[-1,-0.5]"]
data_dci_area[, DCI_brk:= factor(DCI_brk, levels = c("[-1,-0.5]", 
                                                     "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                     "(0.5,1]"))]

saveRDS(data_dci_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_f_area_stats_DCI_all_trends.rds"))


data_sel_trend <- subset(evap_index, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
data_sel_trend  <- grid_cell_area[data_sel_trend, on = .(lon, lat)]

setnames(data_sel_trend, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_sel_trend_melt <- melt(data_sel_trend, measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_sel_trend_area <- data_sel_trend_melt[, .(trend_area = sum(area)), .(value, variable)] 
data_sel_trend_area[, total_area := sum(trend_area), variable]
data_sel_trend_area[, fraction := trend_area/total_area]

saveRDS(data_sel_trend_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_d_area_stats_trend_direction.rds"))

evap_index[, N_sum_0_01:= N_pos_0_01+N_neg_0_01]
evap_index[, N_sum_0_05:= N_pos_0_05+N_neg_0_05]
evap_index[, N_sum_0_1:= N_pos_0_1+N_neg_0_1]
evap_index[, N_sum_0_2:= N_pos_0_2+N_neg_0_2]
evap_index[, N_sum_all:= N_pos_all+N_neg_all]

evap_sel <- subset(evap_index, select = c("N_sum_0_01", "N_sum_0_05", 
                                          "N_sum_0_1", "N_sum_0_2", "N_sum_all",
                                          "lat", "lon"))
evap_sel  <- grid_cell_area[evap_sel, on = .(lon, lat)]

setnames(evap_sel , old = c("N_sum_0_01", "N_sum_0_05", 
                            "N_sum_0_1", "N_sum_0_2", "N_sum_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_melt <- melt(evap_sel, measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_melt[, N_sum_brk := cut(round(value), c(-0.1, 0.9, 4, 7, 11, 14))]
data_melt[N_sum_brk == "(-0.1,0.9]", N_sum_brk := "[0,1)"]
data_melt[N_sum_brk == "(0.9,4]", N_sum_brk := "[1,4]"]

N_sig_area <- data_melt[,.(N_sig_area = sum(area)), .(N_sum_brk, variable)]
N_sig_area[, variable_area := sum(N_sig_area), variable]
N_sig_area[, fraction := N_sig_area/variable_area]
N_sig_area[, N_sum_brk:= factor(N_sum_brk, levels = c("[0,1)", "[1,4]",
                                                      "(4,7]", 
                                                      "(7,11]","(11,14]"))]

saveRDS(N_sig_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_b_area_stats_significant_trend_count.rds"))
