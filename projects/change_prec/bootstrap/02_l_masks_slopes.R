# Slopes of environment averaged precipitation
# Data contain grid cell data where all datasets are present
# This might cause biases over barren, where MOD16a has least coverage 

source('source/changing_prec.R')

library("openair")

## Landcover ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_k_land_cover_class_mean.rds"))
#data <- data[!(dataset == "etmonitor" & year == 2000), ]

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                                         .(dataset, land_cover_short_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_CHANGING_PREC, "02_l_land_cover_trend_bootstrap.rds"))  


## Biomes ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_k_biome_class_mean.rds"))
data <- data[!(dataset == "etmonitor" & year == 2000), ]

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                                         .(dataset, biome_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_CHANGING_PREC, "02_l_biome_trend_bootstrap.rds"))  


## Elevation ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_k_elev_class_mean.rds"))
data <- data[!(dataset == "etmonitor" & year == 2000), ]

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, elev_class)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_CHANGING_PREC, "02_l_elevation_trend_bootstrap.rds"))  


## Koeppen-Geiger ----

### Read data ----
data <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_k_KG_3_class_mean.rds"))
data <- data[!(dataset == "etmonitor" & year == 2000), ]

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, KG_class_3)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_CHANGING_PREC, "02_l_KG_3_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_k_KG_2_class_mean.rds"))
data <- data[!(dataset == "etmonitor" & year == 2000), ]

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, KG_class_2)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_CHANGING_PREC, "02_l_KG_2_trend_bootstrap.rds"))  

### Read data ----
data <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_k_KG_1_class_mean.rds"))
data <- data[!(dataset == "etmonitor" & year == 2000), ]

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, KG_class_1)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_CHANGING_PREC, "02_l_KG_1_trend_bootstrap.rds"))  


## IPCC ----
### Read data ----
data <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_k_ipcc_class_mean.rds"))
data <- data[!(dataset == "etmonitor" & year == 2000), ]

### Analysis ----
data[, date := paste0(year, "-01-01 00:00:00")]
data[, date := as.POSIXct(date)]
data_trend <- data[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], 
                   .(dataset, IPCC_ref_region)]

### Save data ----
saveRDS(data_trend, paste0(PATH_SAVE_CHANGING_PREC, "02_l_ipcc_trend_bootstrap.rds"))  
