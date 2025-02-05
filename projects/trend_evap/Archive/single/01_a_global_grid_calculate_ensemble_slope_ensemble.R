# Trend (Theil-Sen Slope) for each grid for global ensemble of all data products ----
source('source/evap_trend.R')
source('source/geo_functions.R')

library("Kendall")
library("RobustLinearReg")

## Data ----
### Input Data generated in projects/partition_evap/01_b
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))

## Analysis ----
evap_datasets[, year := as.numeric(as.character(year))]
evap_datasets_grid_mean <- evap_datasets[, .(evap = mean(evap, na.rm = T), count = .N), .(lat, lon, year)]
evap_datasets_grid_mean <- evap_datasets_grid_mean[count >= 12]
mean_evap <- evap_datasets_grid_mean[, .(mean_evap = mean(evap, na.rm = T)), .(lat, lon)]

evap_trend_grid_ensemble <- evap_trends_lon_lat(evap_datasets_grid_mean)
evap_trend_grid_ensemble[theil_sen_p_value > 0.05, significant_theil_sen := FALSE] 
evap_trend_grid_ensemble[theil_sen_p_value <= 0.05, significant_theil_sen := TRUE] 

evap_trend_grid_ensemble <- merge(evap_trend_grid_ensemble, mean_evap, by = c("lon", "lat"))

evap_trend_grid_ensemble[, slope_percent := theil_sen_slope/mean_evap*100]

## Save ----
saveRDS(evap_trend_grid_ensemble, paste0(PATH_SAVE_EVAP_TREND, "evap_trend_ensemble_grid.rds"))  
