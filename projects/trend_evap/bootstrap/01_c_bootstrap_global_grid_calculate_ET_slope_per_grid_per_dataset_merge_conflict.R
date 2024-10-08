# Calculate evap slopes and significance for each grid for all datasets using and p-values using bootstrap ----
# Significant slopes have p-value <= 0.05 derived from bootstrap ----

## source and libraries ----
source('source/evap_trend.R')
library("openair")

## Data ----
### Input Data generated in projects/partition_evap/01_b
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))
evap_datasets[, year := as.numeric(as.character(year))]
evap_datasets <- evap_datasets[!(dataset == "etmonitor" & year == 2000), ]
evap_datasets[, date := paste0(year, "-01-01 00:00:00")]
evap_datasets[, date := as.POSIXct(date)]

## Slope Analysis ----

global_datasets <- evap_datasets[, unique(dataset)]

### Calculate slopes and save ----
for (dataset_num in global_datasets){
  print(dataset_num)
  evap_dataset_sel <- evap_datasets[dataset == dataset_num,]
  evap_dataset_sel_trend <- evap_trends_boot(evap_dataset_sel)
  
  evap_dataset_sel_trend[p > 0.05, significant_theil_sen:= FALSE] 
  evap_dataset_sel_trend[p <= 0.05, significant_theil_sen:= TRUE] 
  
  saveRDS(evap_dataset_sel_trend, paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap.rds"))  
}

### Assemble data ----

for (dataset_num in global_datasets){
  dummy_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap.rds"))  
  if(dataset_num != EVAP_GLOBAL_DATASETS[1]){
    evap_trend <- rbind( evap_trend, dummy_trend)
  }else{
    evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap.rds"))  
  }
}

### Trend direction
evap_trend[significant_theil_sen*slope > 0, trend_direction := "positive significant" ]
evap_trend[significant_theil_sen*slope < 0, trend_direction := "negative significant"  ]
evap_trend[significant_theil_sen == FALSE & slope > 0, trend_direction := "positive" ]
evap_trend[significant_theil_sen == FALSE & slope <= 0, trend_direction := "negative"  ]

### Calculate and merge grid stats with trend ----
evap_datasets[ year < 2010, time_period := "2000-2009", ]
evap_datasets[ year >= 2010, time_period := "2010-2019", ]

evap_grid_stats_dummy <- evap_datasets[ time_period == "2000-2009", .(mean_evap_dec1 = mean(evap), median_evap_dec1 = median(evap), sd_evap_dec1 =  sd(evap)) , .(dataset, lat, lon)]

evap_trend <- merge(evap_trend, evap_grid_stats_dummy, by = c("dataset", "lon", "lat"))

evap_grid_stats_dummy <- evap_datasets[ time_period == "2010-2019", .(mean_evap_dec2 = mean(evap), median_evap_dec2 = median(evap), sd_evap_dec2 =  sd(evap)) , .(dataset, lat, lon)]

evap_trend <- merge(evap_trend, evap_grid_stats_dummy, by = c("dataset", "lon", "lat"))

evap_grid_stats_dummy <- evap_datasets[, .(mean_evap = mean(evap), median_evap = median(evap), sd_evap =  sd(evap)) , .(dataset, lat, lon)]

evap_trend <- merge(evap_trend, evap_grid_stats_dummy, by = c("dataset", "lon", "lat"))

evap_trend[, slope_percent := slope/mean_evap*100]

evap_trend <- evap_trend[complete.cases(evap_trend)]
evap_trend <- evap_trend[,dataset_count := .N,.(lon, lat)]
evap_trend_complete_only <- evap_trend[dataset_count >= n_datasets_2000_2019,]

## save data----
saveRDS(evap_trend, paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
saveRDS(evap_trend_complete_only , paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap.rds"))  
