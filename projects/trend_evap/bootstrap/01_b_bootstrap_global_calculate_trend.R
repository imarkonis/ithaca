# Trend of global product averages with bootstrapp ----
source('source/evap_trend.R')

library("openair")

## Data  ----
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_annual <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))
evap_annual <- evap_annual[!(dataset == "etmonitor" & year == 2000), ]

evap_annual[, evap_anomaly := evap_annual_mean - mean(evap_annual_mean), .(dataset)]
evap_annual[ year < 2010, time_period := "2000-2009", ]
evap_annual[ year >= 2010, time_period := "2010-2019", ]
evap_annual[, date := paste0(year, "-01-01 00:00:00")]
evap_annual[, date := as.POSIXct(date)]
evap_annual_trend <- evap_annual[, TheilSen(.SD, pollutant = "evap_annual_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], .(dataset)]

evap_annual_trend[p > 0.05, significant_theil_sen:= FALSE] 
evap_annual_trend[p <= 0.05, significant_theil_sen:= TRUE] 

evap_annual_trend[significant_theil_sen*slope > 0, trend_score := 1  ]
evap_annual_trend[significant_theil_sen*slope < 0, trend_score := -1  ]
evap_annual_trend[significant_theil_sen == FALSE, trend_score := 0  ]

evap_annual_trend[trend_score == 1, trend_direction := "positive"]
evap_annual_trend[trend_score == -1, trend_direction := "negative"]
evap_annual_trend[trend_score == 0, trend_direction := "no trend"]

evap_global_stats_dummy <- evap_annual[ time_period == "2000-2009", .(mean_evap_dec1 = mean(evap_annual_mean), median_evap_dec1 = median(evap_annual_mean), sd_evap_dec1 =  sd(evap_annual_mean)) , .(dataset)]

evap_annual_trend <- merge(evap_annual_trend, evap_global_stats_dummy, by = "dataset")

evap_global_stats_dummy <- evap_annual[ time_period == "2010-2019", .(mean_evap_dec2 = mean(evap_annual_mean), median_evap_dec2 = median(evap_annual_mean), sd_evap_dec2 =  sd(evap_annual_mean)) , .(dataset)]

evap_annual_trend <- merge(evap_annual_trend, evap_global_stats_dummy, by = "dataset")

## Save data ----
saveRDS(evap_annual_trend, paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend_bootstrap.rds"))  





