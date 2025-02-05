# Rank datasets according to signal ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
## Created in trend_evap/01_g
evap_trend_g <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

## Created in trend_evap/01_h
evap_trend_h <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap_dataset_leftout.rds"))


### Input data generated in trend_evap/bootstrap/01_c 
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  

evap_trend_pos <- evap_trend[p <= 0.01 & slope >= 0, .(N_pos_0_01 = .N), .(lat, lon, dataset)]
evap_trend_neg <- evap_trend[p <= 0.01 & slope < 0, .(N_neg_0_01 = .N), .(lat, lon, dataset)]
evap_trend_none <- evap_trend[p > 0.01, .(N_none_0_01 = .N), .(lat, lon, dataset)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### p value as 0.05 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.05 & slope >= 0, .(N_pos_0_05 = .N), .(lat, lon, dataset)]
evap_trend_neg <- evap_trend[p <= 0.05 & slope < 0, .(N_neg_0_05 = .N), .(lat, lon, dataset)]
evap_trend_none <- evap_trend[p > 0.05, .(N_none_0_05 = .N), .(lat, lon, dataset)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### p value as 0.1 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.1 & slope >= 0, .(N_pos_0_1 = .N), .(lat, lon, dataset)]
evap_trend_neg <- evap_trend[p <= 0.1 & slope < 0, .(N_neg_0_1 = .N), .(lat, lon, dataset)]
evap_trend_none <- evap_trend[p > 0.1, .(N_none_0_1 = .N), .(lat, lon, dataset)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### p value as 0.2 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.2 & slope >= 0, .(N_pos_0_2 = .N), .(lat, lon, dataset)]
evap_trend_neg <- evap_trend[p <= 0.2 & slope < 0, .(N_neg_0_2 = .N), .(lat, lon, dataset)]
evap_trend_none <- evap_trend[p > 0.2, .(N_none_0_2 = .N), .(lat, lon, dataset)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat", "dataset"), all = TRUE)

### all slopes ----
evap_trend_pos <- evap_trend[slope >= 0, .(N_pos_all = .N), .(lat, lon, dataset)]
evap_trend_neg <- evap_trend[slope < 0, .(N_neg_all = .N), .(lat, lon, dataset)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat", "dataset"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat", "dataset"), all = TRUE)


### Fill NA as 0 for count
evap_trend_summary[is.na(evap_trend_summary)] <- 0

grid_cell_area <- unique(evap_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_summary <- grid_cell_area[evap_trend_summary, on = .(lon, lat)]

saveRDS(evap_trend_summary, paste0(PATH_SAVE_EVAP_TREND, "global_trends_summary_p_val_lon_lat_dataproducts.rds"))


evap_sums_area <- evap_trend_summary[, .(sum_N_pos_0_01 = sum(N_pos_0_01*area),
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


evap_sums_melt <- melt(evap_sums_area,
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


evap_sums_melt[, rank_datasets := rank(-value), .(variable)]

saveRDS(evap_sums_melt, paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))


## Estimate opposing fraction of all data ----

evap_sel <- subset(evap_trend_g, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
evap_sel  <- grid_cell_area[evap_sel, on = .(lon, lat)]
setnames(evap_sel, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

evap_sel_melt <- melt(evap_sel, measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area <- evap_sel[, sum(area)]
evap_sum_opposing <- evap_sel_melt[value == "opposing", .(sum = sum(area)/total_area), .(variable)]


## Estimate opposing fraction dataset leftout

evap_leftout_sel <- subset(evap_trend_h, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon", "dataset_leftout"))
evap_leftout_sel  <- grid_cell_area[evap_leftout_sel, on = .(lon, lat)]
setnames(evap_leftout_sel, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

evap_leftout_sel_melt <- melt(evap_leftout_sel, measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

evap_leftout_sum_opposing <- evap_leftout_sel_melt[value == "opposing", .(sum_leftout = sum(area)/total_area), .(variable, dataset_leftout)]

evap_opposing <- merge(evap_leftout_sum_opposing, evap_sum_opposing, by = "variable")

evap_opposing[, sum_diff := sum - sum_leftout]

evap_opposing[, rank_opp := rank(-sum_diff), .(variable)]

## Save data ----
saveRDS(evap_opposing, paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))
