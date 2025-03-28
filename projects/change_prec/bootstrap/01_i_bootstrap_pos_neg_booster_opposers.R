# Rank datasets according to signal ----
source('source/changing_prec.R')
source('source/geo_functions.R')

## Data ----
## Created in changing_prec/01_g
prec_trend_g <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_g_global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

## Created in changing_prec/01_h
prec_trend_h <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_h_global_grid_DCI_trend_groups_p_thresholds_bootstrap_dataset_leftout.rds"))


### Input data generated in changing_prec/bootstrap/01_c 
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  
prec_trend <- prec_trend[dataset_count >= 10]

prec_trend_pos <- prec_trend[p <= 0.01 & slope >= 0, .(N_pos_0_01 = .N), .(lat, lon, dataset)]
prec_trend_neg <- prec_trend[p <= 0.01 & slope < 0, .(N_neg_0_01 = .N), .(lat, lon, dataset)]
prec_trend_none <- prec_trend[p > 0.01, .(N_none_0_01 = .N), .(lat, lon, dataset)]

prec_trend_summary <- merge(prec_trend_pos, prec_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### p value as 0.05 as threshold ----
prec_trend_pos <- prec_trend[p <= 0.05 & slope >= 0, .(N_pos_0_05 = .N), .(lat, lon, dataset)]
prec_trend_neg <- prec_trend[p <= 0.05 & slope < 0, .(N_neg_0_05 = .N), .(lat, lon, dataset)]
prec_trend_none <- prec_trend[p > 0.05, .(N_none_0_05 = .N), .(lat, lon, dataset)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### p value as 0.1 as threshold ----
prec_trend_pos <- prec_trend[p <= 0.1 & slope >= 0, .(N_pos_0_1 = .N), .(lat, lon, dataset)]
prec_trend_neg <- prec_trend[p <= 0.1 & slope < 0, .(N_neg_0_1 = .N), .(lat, lon, dataset)]
prec_trend_none <- prec_trend[p > 0.1, .(N_none_0_1 = .N), .(lat, lon, dataset)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### p value as 0.2 as threshold ----
prec_trend_pos <- prec_trend[p <= 0.2 & slope >= 0, .(N_pos_0_2 = .N), .(lat, lon, dataset)]
prec_trend_neg <- prec_trend[p <= 0.2 & slope < 0, .(N_neg_0_2 = .N), .(lat, lon, dataset)]
prec_trend_none <- prec_trend[p > 0.2, .(N_none_0_2 = .N), .(lat, lon, dataset)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### all slopes ----
prec_trend_pos <- prec_trend[slope >= 0, .(N_pos_all = .N), .(lat, lon, dataset)]
prec_trend_neg <- prec_trend[slope < 0, .(N_neg_all = .N), .(lat, lon, dataset)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)


### Fill NA as 0 for count
prec_trend_summary[is.na(prec_trend_summary)] <- 0

grid_cell_area <- unique(prec_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
prec_trend_summary <- grid_cell_area[prec_trend_summary, on = .(lon, lat)]

saveRDS(prec_trend_summary, paste0(PATH_SAVE_CHANGING_PREC, "01_i_global_trends_summary_p_val_lon_lat_dataproducts.rds"))


prec_sums_area <- prec_trend_summary[, .(sum_N_pos_0_01 = sum(N_pos_0_01*area),
                                    sum_N_pos_0_05 = sum(N_pos_0_05*area),
                                    sum_N_pos_0_1 = sum(N_pos_0_1*area),
                                    sum_N_pos_0_2 = sum(N_pos_0_2*area),
                                    sum_N_pos_all = sum(N_pos_all*area),
                                    sum_N_neg_0_01 = sum(N_neg_0_01*area),
                                    sum_N_neg_0_05 = sum(N_neg_0_05*area),
                                    sum_N_neg_0_1 = sum(N_neg_0_1*area),
                                    sum_N_neg_0_2 = sum(N_neg_0_2*area),
                                    sum_N_neg_all = sum(N_neg_all*area),
                                    sum_N_none_0_01 = sum(N_none_0_01*area),
                                    sum_N_none_0_05 = sum(N_none_0_05*area),
                                    sum_N_none_0_1 = sum(N_none_0_1*area),
                                    sum_N_none_0_2 = sum(N_none_0_2*area)
), .(dataset)]


prec_sums_melt <- melt(prec_sums_area,
                       measure.vars = c("sum_N_pos_0_01",
                                        "sum_N_pos_0_05",
                                        "sum_N_pos_0_1",
                                        "sum_N_pos_0_2",
                                        "sum_N_pos_all",
                                        "sum_N_neg_0_01",
                                        "sum_N_neg_0_05",
                                        "sum_N_neg_0_1",
                                        "sum_N_neg_0_2",
                                        "sum_N_neg_all",
                                        "sum_N_none_0_01",
                                        "sum_N_none_0_05",
                                        "sum_N_none_0_1",
                                        "sum_N_none_0_2"
                       ))


prec_sums_melt[, rank_datasets := rank(-value), .(variable)]

saveRDS(prec_sums_melt, paste0(PATH_SAVE_CHANGING_PREC, "01_i_global_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))


## Estimate opposing fraction of all data ----

prec_sel <- subset(prec_trend_g, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
prec_sel  <- grid_cell_area[prec_sel, on = .(lon, lat)]
setnames(prec_sel, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

prec_sel_melt <- melt(prec_sel, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

total_area <- prec_sel[, sum(area)]
prec_sum_opposing <- prec_sel_melt[value == "opposing", .(sum = sum(area)/total_area), .(variable)]


## Estimate opposing fraction dataset leftout

prec_leftout_sel <- subset(prec_trend_h, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon", "dataset_leftout"))
prec_leftout_sel  <- grid_cell_area[prec_leftout_sel, on = .(lon, lat)]
setnames(prec_leftout_sel, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

prec_leftout_sel_melt <- melt(prec_leftout_sel, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

prec_leftout_sum_opposing <- prec_leftout_sel_melt[value == "opposing", .(sum_leftout = sum(area)/total_area), .(variable, dataset_leftout)]

prec_opposing <- merge(prec_leftout_sum_opposing, prec_sum_opposing, by = "variable")

prec_opposing[, sum_diff := sum - sum_leftout]

prec_opposing[, rank_opp := rank(-sum_diff), .(variable)]

## Save data ----
saveRDS(prec_opposing, paste0(PATH_SAVE_CHANGING_PREC, "01_i_global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))
