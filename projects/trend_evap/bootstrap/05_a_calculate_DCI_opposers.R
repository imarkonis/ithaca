# Opposing datasets based on model democracy ----
# Only considering grids with complete data coverage ----
source('source/evap_trend.R')
source('source/geo_functions.R')

evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap.rds"))

evap_trend <- evap_trend[, .(lat, lon, dataset, slope, p, lower, upper)]
grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend <- grid_cell_area[evap_trend, on = .(lon, lat)]
evap_index <- evap_index[,.(lat, lon, DCI_0_01, DCI_0_05, DCI_0_1, DCI_0_2, DCI_all)]

evap_trend <- evap_index[evap_trend, on = .(lat, lon)]

evap_trend[DCI_0_01/slope < 0 & DCI_0_01 != 0, opposing_0_01 := 1]
evap_trend[DCI_0_01/slope >= 0 , opposing_0_01 := 0]
evap_trend[slope == 0, opposing_0_01 := 3]
evap_trend[DCI_0_01 == 0 , opposing_0_01 := 2]

evap_trend[DCI_0_05/slope < 0 & DCI_0_05 != 0, opposing_0_05 := 1]
evap_trend[DCI_0_05/slope >= 0 , opposing_0_05 := 0]
evap_trend[slope == 0, opposing_0_05 := 3]
evap_trend[DCI_0_05 == 0, opposing_0_05 := 2]

evap_trend[DCI_0_1/slope < 0 & DCI_0_1 != 0, opposing_0_1 := 1]
evap_trend[DCI_0_1/slope >= 0 , opposing_0_1 := 0]
evap_trend[slope == 0, opposing_0_1 := 3]
evap_trend[DCI_0_1 == 0, opposing_0_1 := 2]

evap_trend[DCI_0_2/slope < 0 & DCI_0_2 != 0, opposing_0_2 := 1]
evap_trend[DCI_0_2/slope >= 0 , opposing_0_2 := 0]
evap_trend[slope == 0, opposing_0_2 := 3]
evap_trend[DCI_0_2 == 0 , opposing_0_2 := 2]

evap_trend[DCI_all/slope < 0 & DCI_all != 0, opposing_all := 1]
evap_trend[DCI_all/slope >= 0 , opposing_all := 0]
evap_trend[slope == 0, opposing_all := 3]
evap_trend[DCI_0_2 == 0 , opposing_all := 2]

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

saveRDS(opposing_melt, paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_DCI.rds")) 

