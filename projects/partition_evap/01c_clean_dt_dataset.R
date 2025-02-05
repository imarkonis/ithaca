# Clean dataset ----
# Only include grids with minimum coverage of 13 
# Only include grid with complete time series
source('source/partition_evap.R')
source('source/geo_functions.R')
## Load data ----
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_datasets[, year_count := .N, .(dataset, lon, lat)]
evap_datasets <- evap_datasets[year_count == 20]

evap_datasets[, data_count := .N, .(lon, lat, year)]
evap_datasets <- evap_datasets[data_count >= 13]

evap_datasets <- evap_datasets[!(dataset == "etmonitor" & year == "2000")]

evap_datasets[, data_count := NULL]
evap_datasets[, year_count := NULL]
grid_cell_area <- unique(evap_datasets[, .(lon, lat)]) %>% grid_area() # m2
evap_datasets <- grid_cell_area[evap_datasets, on = .(lon, lat)]
evap_datasets[, evap_volume := evap*M2_TO_KM2*MM_TO_KM*area]
evap_datasets[dataset == "etsynthesis", dataset := "synthesizedet"]
evap_datasets <- evap_datasets[order(dataset)] 
saveRDS(evap_datasets, paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))


## Global annual evap mean ----

### Getting area for weights ----


total_area <- sum(grid_cell_area$area)
total_area/GLOBAL_AREA

evap_datasets[, area_weights := area/sum(area), .(dataset, year)]
evap_datasets[, all_area_weights := sum(area_weights), .(dataset, year)]

evap_datasets_global_annual_mean <- evap_datasets[, .(evap_annual_mean = sum(area_weights*evap)), .(dataset, year)]
evap_datasets_global_annual_mean[, year := as.numeric(as.character(year))]

total_area <-  evap_datasets[, .(total_area = sum(area)), .(dataset, year)]
total_area[, year := as.numeric(as.character(year))]
evap_datasets_global_annual_mean <- total_area[evap_datasets_global_annual_mean, on = .(dataset, year)]

evap_datasets_global_annual_mean[, evap_volume := evap_annual_mean*M2_TO_KM2*MM_TO_KM*total_area]

saveRDS(evap_datasets_global_annual_mean, paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))

### table for Zenodo ----
write.table(evap_datasets_global_annual_mean, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "interannual_variance_global.csv"), row.names = F, sep = ",")

## Grid-wise mean ----
evap_datasets_grid_mean <- evap_datasets[, .(evap_mean = mean(evap), evap_sd = sd(evap)), .(lon, lat, dataset)]
evap_datasets_grid_mean <- grid_cell_area[evap_datasets_grid_mean, on = .(lon, lat)] 
evap_datasets_grid_mean[, evap_volume := evap_mean*M2_TO_KM2*MM_TO_KM*area]

saveRDS(evap_datasets_grid_mean, paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

ggplot(evap_datasets_global_annual_mean)+
  geom_point(aes(x = year, y = evap_annual_mean, col = dataset))+
  scale_color_manual(values = cols_data)+
  theme_bw()
  