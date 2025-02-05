# Slopes of environment averaged ET
# Data contain grid cell data where all datasets are present
# This might cause biases over barren, where MOD16a has least coverage 

source('source/evap_trend.R')

library("openair")

## Landcover ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                                         .(dataset, land_cover_short_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "land_cover_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_ensemble_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(land_cover_short_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "land_cover_ensemble_trend_bootstrap.rds"))  


## Biomes ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_class_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                                         .(dataset, biome_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "biome_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_class_ensemble_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(biome_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "biome_trend_ensemble_bootstrap.rds"))  



## Elevation ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elev_class_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, elev_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "elevation_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elev_class_ensemble_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(elev_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "elevation_ensemble_trend_bootstrap.rds"))  



## Koeppen-Geiger ----

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_class_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, KG_class_3)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "KG_3_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_class_ensemble_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(KG_class_3)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "KG_3_ensemble_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_2_class_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, KG_class_2)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "KG_2_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_2_class_ensemble_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(KG_class_2)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "KG_2_ensemble_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_1_class_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, KG_class_1)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "KG_1_trend_bootstrap.rds"))  



### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_1_class_ensemble_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(KG_class_1)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "KG_1_ensemble_trend_bootstrap.rds"))  


## IPCC ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_class_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, IPCC_ref_region)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "ipcc_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_class_ensemble_mean.rds"))

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "evap_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(IPCC_ref_region)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_EVAP_TREND, "ipcc_ensemble_trend_bootstrap.rds"))  
