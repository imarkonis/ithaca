# Stats for text ----

source('source/evap_trend.R')
source('source/geo_functions.R')


# Figure 1 ----
## Global quartile fold ----
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_a_global_evap_trend.rds"))

Q25_global <- evap_annual_trend[, quantile(slope, 0.25)]
Q75_global <- evap_annual_trend[, quantile(slope, 0.75)]
Q75_global/Q25_global


## Area stats ----

evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "different sign", problem := "Direction and Magnitude"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "different sign", problem := "Direction"] 

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "same sign" & (abs(Q25) >= 1 | abs(Q75) >= 1), problem := "Magnitude"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "same sign", problem := "None"] 

evap_trend_stats[sign == "different sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Direction"] 

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "same sign" & (abs(Q25) < 1 & abs(Q75) < 1), problem := "Small trend - Magnitude"] 

evap_trend_stats[, problem:= as.factor(problem)]

evap_trend_stats[Q25 < 0, Q25_sign:= "negative"]
evap_trend_stats[Q25 >= 0, Q25_sign:= "positive"]

grid_cell_area <- unique(evap_trend_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_stats <- grid_cell_area[evap_trend_stats, on = .(lon, lat)]
total_area <- evap_trend_stats[, sum(area)]

### Fold summaries ----
evap_trend_stats[, sum(area)/total_area, .(fold_brk)]
evap_trend_stats[, sum(area)/total_area, .(fold_brk_detailed)]

levels(evap_trend_stats$fold_brk_detailed)
area_folds <- evap_trend_stats[, .(area_fraction = round(sum(area)/total_area*100, 1)), .(fold_brk_detailed)]
area_folds <- area_folds[area_folds[, order(fold_brk_detailed)],]

saveRDS(area_folds, paste0(PATH_SAVE_EVAP_TREND_TABLES, "area_fraction_folds.rds"))

### MAD summaries ----

evap_trend_stats[, sum(area)/total_area, .(mad_brk)]
evap_trend_stats[, sum(area)/total_area, .(mad_brk_detailed)]

levels(evap_trend_stats$mad_brk_detailed)
area_folds <- evap_trend_stats[, .(area_fraction = round(sum(area)/total_area*100, 1)), .(mad_brk_detailed)]
area_folds <- area_folds[area_folds[, order(mad_brk_detailed)],]

saveRDS(area_folds, paste0(PATH_SAVE_EVAP_TREND_TABLES, "area_fraction_mad.rds"))

### sign difference ----
evap_trend_stats[, sum(area)/total_area, .(sign)]

evap_trend_stats[min/max > 0, min_max_sign := "same sign", ]
evap_trend_stats[min/max < 0, min_max_sign := "different sign", ]
evap_trend_stats[, sum(area)/total_area, .(min_max_sign)]


### problem areas

problem_dir <- evap_trend_stats[,  sum(area)/total_area, .(problem, Q25_sign)]
problem <- evap_trend_stats[, sum(area)/total_area, .(problem)]

problem_dir <- merge(problem_dir, problem, by = "problem", all = T)
problem_dir[, fraction := V1.x/V1.y]
problem_dir
# Figure 2  ----
## Opposing trends ----
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_a_c_d_grid_trend_stats.rds"))

evap_index <- grid_cell_area[evap_index, on = .(lon, lat)]

## combining DCI and problem regions
combined <- evap_index[evap_trend_stats[, .(problem, lon, lat)], on = .(lon, lat)]
combined_frac <- combined[,.(p_area = sum(area)), .(DCI_all_brk, problem)]
combined_frac[, total_area_problem := sum(p_area), problem]
combined_frac[, fraction_problem := p_area/total_area_problem ]

combined_frac[, total_area_DCI := sum(p_area), DCI_all_brk]
combined_frac[, fraction_DCI := p_area/total_area_DCI]

p_val_opposing <- evap_index[,.(p_area = sum(area)), (p_val_opposing)]
p_val_opposing[, total_area := sum(p_area)]
p_val_opposing[, fraction := p_area/total_area]

p_val_opposing
p_val_opposing[order(p_val_opposing)]

# most trends are significant ----
sig_trends <- evap_index[,.(p_area = sum(area)), (more_sig_trends)]
sig_trends[, total_area := sum(p_area)]
sig_trends[, fraction := p_area/total_area]

sig_trends
sig_trends[order(sig_trends)]


N_sig_area <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_b_area_stats_significant_trend_count.rds"))


# DCI
DCI_all <- evap_index[,.(p_area = sum(area)), (DCI_all_brk)]
DCI_all[, total_area := sum(p_area)]
DCI_all[, fraction := p_area/total_area]

DCI_all
DCI_all[order(DCI_all)]

## Figure 3 ----
evap_trend_area_dataset <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_SI_fig_3_area_fraction_trend_significance_by_product.rds"))

