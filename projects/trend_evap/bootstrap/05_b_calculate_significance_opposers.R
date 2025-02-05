# Opposing datasets based on model democracy ----
# Only considering grids with complete data coverage ----
source('source/evap_trend.R')
source('source/geo_functions.R')

evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap.rds"))

evap_trend <- evap_trend[, .(lat, lon, dataset, slope, p, lower, upper)]
grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend <- grid_cell_area[evap_trend, on = .(lon, lat)]
evap_index <- evap_index[,.(lat, lon, N_pos_0_01, N_neg_0_01, N_none_0_01,
                            N_pos_0_05, N_neg_0_05, N_none_0_05,
                            N_pos_0_1, N_neg_0_1, N_none_0_1,
                            N_pos_0_2, N_neg_0_2, N_none_0_2,
                            N_pos_all, N_neg_all)]

evap_index[N_none_0_01 > N_pos_0_01 & N_none_0_01 > N_neg_0_01, majority_0_01 := "none"]
evap_index[N_pos_0_01 > N_none_0_01 & N_pos_0_01 > N_neg_0_01, majority_0_01 := "positive"]
evap_index[N_neg_0_01 > N_pos_0_01 & N_neg_0_01 > N_none_0_01, majority_0_01 := "negative"]

evap_index[N_none_0_05 > N_pos_0_05 & N_none_0_05 > N_neg_0_05, majority_0_05 := "none"]
evap_index[N_pos_0_05 > N_none_0_05 & N_pos_0_05 > N_neg_0_05, majority_0_05 := "positive"]
evap_index[N_neg_0_05 > N_pos_0_05 & N_neg_0_05 > N_none_0_05, majority_0_05 := "negative"]

evap_index[N_none_0_1 > N_pos_0_1 & N_none_0_1 > N_neg_0_1, majority_0_1 := "none"]
evap_index[N_pos_0_1 > N_none_0_1 & N_pos_0_1 > N_neg_0_1, majority_0_1 := "positive"]
evap_index[N_neg_0_1 > N_pos_0_1 & N_neg_0_1 > N_none_0_1, majority_0_1 := "negative"]

evap_index[N_none_0_2 > N_pos_0_2 & N_none_0_2 > N_neg_0_2, majority_0_2 := "none"]
evap_index[N_pos_0_2 > N_none_0_2 & N_pos_0_2 > N_neg_0_2, majority_0_2 := "positive"]
evap_index[N_neg_0_2 > N_pos_0_2 & N_neg_0_2 > N_none_0_2, majority_0_2 := "negative"]

evap_index[N_pos_all > N_neg_all, majority_all := "positive"]
evap_index[N_neg_all > N_pos_all, majority_all := "negative"]


evap_trend <- evap_index[evap_trend, on = .(lat, lon)]
evap_trend[, opposing_0_01 := 0]
evap_trend[(majority_0_01 == "none" & p <= 0.05) | (majority_0_01 == "positive" & p > 0.05 | majority_0_01 == "positive" & slope < 0)
           | (majority_0_01 == "negative" & p > 0.05 | majority_0_01 == "negative" & slope > 0), opposing_0_01 := 1]


evap_trend[, opposing_0_05 := 0]
evap_trend[(majority_0_05 == "none" & p <= 0.05) | (majority_0_05 == "positive" & p > 0.05 | majority_0_05 == "positive" & slope < 0)
           | (majority_0_05 == "negative" & p > 0.05 | majority_0_05 == "negative" & slope > 0), opposing_0_05 := 1]

evap_trend[, opposing_0_1 := 0]
evap_trend[(majority_0_1 == "none" & p <= 0.05) | (majority_0_1 == "positive" & p > 0.05 | majority_0_1 == "positive" & slope < 0)
           | (majority_0_1 == "negative" & p > 0.05 | majority_0_1 == "negative" & slope > 0), opposing_0_1 := 1]

evap_trend[, opposing_0_2 := 0]
evap_trend[(majority_0_2 == "none" & p <= 0.05) | (majority_0_2 == "positive" & p > 0.05 | majority_0_2 == "positive" & slope < 0)
           | (majority_0_2 == "negative" & p > 0.05 | majority_0_2 == "negative" & slope > 0), opposing_0_2 := 1]

evap_trend[, opposing_all := 0]
evap_trend[ (majority_all == "positive" & slope < 0)
           | (majority_all == "negative" & slope > 0), opposing_all := 1]

area_opposing_0_01 <- evap_trend[, .(area_opposing_0_01 = sum(area)), .(opposing_0_01, dataset)]
area_opposing_0_05 <- evap_trend[, .(area_opposing_0_05 = sum(area)), .(opposing_0_05, dataset)]
area_opposing_0_1 <- evap_trend[, .(area_opposing_0_1 = sum(area)), .(opposing_0_1, dataset)]
area_opposing_0_2 <- evap_trend[, .(area_opposing_0_2 = sum(area)), .(opposing_0_2, dataset)]
area_opposing_all <- evap_trend[, .(area_opposing_all = sum(area)), .(opposing_all, dataset)]

area_opposing <- merge(area_opposing_0_01, area_opposing_0_05, by.x = c("opposing_0_01", "dataset"), by.y = c("opposing_0_05", "dataset"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_1, by.x = c("opposing_0_01", "dataset"), by.y = c("opposing_0_1", "dataset"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_2, by.x = c("opposing_0_01", "dataset"), by.y = c("opposing_0_2", "dataset"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_all, by.x = c("opposing_0_01", "dataset"), by.y = c("opposing_all", "dataset"), all = TRUE)


opposing_melt <- melt(area_opposing,
                      measure.vars = c("area_opposing_0_01",
                                       "area_opposing_0_05",
                                       "area_opposing_0_1",
                                       "area_opposing_0_2",
                                       "area_opposing_all"
                      ))


opposing_melt[, rank_datasets := rank(-value), .(variable, opposing_0_01)]

opposing_melt[variable == "area_opposing_0_01", variable := "p <= 0.01", ]
opposing_melt[variable == "area_opposing_0_05", variable := "p <= 0.05", ]
opposing_melt[variable == "area_opposing_0_1", variable := "p <= 0.1", ]
opposing_melt[variable == "area_opposing_0_2", variable := "p <= 0.2", ]
opposing_melt[variable == "area_opposing_all", variable := "p <= 1", ]

saveRDS(opposing_melt, paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_significance.rds")) 


