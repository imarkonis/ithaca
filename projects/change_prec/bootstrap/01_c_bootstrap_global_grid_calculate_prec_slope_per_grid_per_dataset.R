# Calculate prec slopes and significance for each grid for all datasets using and p-values using bootstrap (large memory requirements)----
# Significant slopes have p-value <= 0.05 derived from bootstrap ----

## source and libraries ----
source('source/changing_prec.R')
library("openair")
load("~/shared/data_projects/ithaca/changing_prec/prec_names_2000_2019.Rdata") # Created by database/06b_dataset_fnames_prec_change.R

## Data ----
### Input Data generated in projects/partition_prec/01_b

prec_datasets <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "prec_datasets.rds"))
prec_datasets[, year := as.numeric(as.character(year))]
#prec_datasets <- prec_datasets[!(dataset == "etmonitor" & year == 2000), ]
prec_datasets[, date := paste0(year, "-01-01 00:00:00")]
prec_datasets[, date := as.POSIXct(date)]

unique(prec_datasets, by = "dataset")

# Count the number of unique years for each (lon, lat, dataset) combination
year_counts <- prec_datasets[, .N, by = .(lon, lat, dataset)]
filtered_year_counts <- year_counts[N >= 10]
# Merge back with the original data to keep only the valid rows
prec_datasets <- prec_datasets[filtered_year_counts, on = .(lon, lat, dataset)]

prec_datasets[N < 10]
summary(prec_datasets)

n_datasets_2000_2019 <- length(PREC_FNAMES_SHORT_2000_2019)

## Slope Analysis ----

### Calculate slopes and save ----
for (dataset_num in PREC_FNAMES_SHORT_2000_2019){
  print(dataset_num)
  prec_dataset_sel <- prec_datasets[dataset == dataset_num,]
  prec_dataset_sel_trend <- prec_trends_boot(prec_dataset_sel)

  prec_dataset_sel_trend[p > 0.05, significant_theil_sen:= FALSE]
  prec_dataset_sel_trend[p <= 0.05, significant_theil_sen:= TRUE]

  saveRDS(prec_dataset_sel_trend, paste0(PATH_SAVE_CHANGING_PREC, "01_c_prec_dataset_",dataset_num,"_trend_bootstrap.rds"))
}


### Assemble data ----
for (dataset_num in PREC_FNAMES_SHORT_2000_2019){
  dummy_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_prec_dataset_",dataset_num,"_trend_bootstrap.rds"))  
  if(dataset_num != PREC_FNAMES_SHORT_2000_2019[1]){
    prec_trend <- rbind( prec_trend, dummy_trend)
  }else{
    prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_prec_dataset_",dataset_num,"_trend_bootstrap.rds"))  
  }
}

### Trend direction
prec_trend[significant_theil_sen*slope > 0, trend_direction := "positive significant" ]
prec_trend[significant_theil_sen*slope < 0, trend_direction := "negative significant"  ]
prec_trend[significant_theil_sen == FALSE & slope > 0, trend_direction := "positive" ]
prec_trend[significant_theil_sen == FALSE & slope <= 0, trend_direction := "negative"  ]

### Calculate and merge grid stats with trend ----
prec_datasets[ year < 2010, time_period := "2000-2009", ]
prec_datasets[ year >= 2010, time_period := "2010-2019", ]

prec_grid_stats_dummy <- prec_datasets[ time_period == "2000-2009", .(mean_prec_dec1 = mean(prec), median_prec_dec1 = median(prec), sd_prec_dec1 =  sd(prec)) , .(dataset, lat, lon)]

prec_trend <- merge(prec_trend, prec_grid_stats_dummy, by = c("dataset", "lon", "lat"))

prec_grid_stats_dummy <- prec_datasets[ time_period == "2010-2019", .(mean_prec_dec2 = mean(prec), median_prec_dec2 = median(prec), sd_prec_dec2 =  sd(prec)) , .(dataset, lat, lon)]

prec_trend <- merge(prec_trend, prec_grid_stats_dummy, by = c("dataset", "lon", "lat"))

prec_grid_stats_dummy <- prec_datasets[, .(mean_prec = mean(prec), median_prec = median(prec), sd_prec =  sd(prec)) , .(dataset, lat, lon)]

prec_trend <- merge(prec_trend, prec_grid_stats_dummy, by = c("dataset", "lon", "lat"))

prec_trend[, slope_percent := slope/mean_prec*100]

prec_trend <- prec_trend[complete.cases(prec_trend)]
prec_trend <- prec_trend[,dataset_count := .N,.(lon, lat)]
prec_trend_complete_only <- prec_trend[dataset_count >= n_datasets_2000_2019,]

## save data----
saveRDS(prec_trend, paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  
saveRDS(prec_trend_complete_only , paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_intersection_lat_lon_bootstrap.rds"))  
