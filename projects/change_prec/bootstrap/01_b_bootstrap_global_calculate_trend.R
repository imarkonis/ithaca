# Trend of global product averages with bootstrapp ----
source('source/changing_prec.R')

library("openair")

## Data  ----
PATH_SAVE_CHANGING_PREC <- paste0(PATH_SAVE, "changing_prec/")
prec_annual <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "prec_global_annual_mean.rds"))
#prec_annual <- prec_annual[!(dataset == "etmonitor" & year == 2000), ]

prec_annual[, prec_anomaly := prec_mean - mean(prec_mean), .(dataset)]
prec_annual[ year < 2010, time_period := "2000-2009", ]
prec_annual[ year >= 2010, time_period := "2010-2019", ]
prec_annual[, date := paste0(year, "-01-01 00:00:00")]
prec_annual[, date := as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")]
prec_annual_trend <- prec_annual[, TheilSen(.SD, pollutant = "prec_mean", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)], .(dataset)]

prec_annual_trend[p > 0.05, significant_theil_sen:= FALSE] 
prec_annual_trend[p <= 0.05, significant_theil_sen:= TRUE] 

prec_annual_trend[significant_theil_sen*slope > 0, trend_score := 1  ]
prec_annual_trend[significant_theil_sen*slope < 0, trend_score := -1  ]
prec_annual_trend[significant_theil_sen == FALSE, trend_score := 0  ]

prec_annual_trend[trend_score == 1, trend_direction := "positive"]
prec_annual_trend[trend_score == -1, trend_direction := "negative"]
prec_annual_trend[trend_score == 0, trend_direction := "no trend"]

prec_global_stats_dummy <- prec_annual[ time_period == "2000-2009", .(mean_prec_dec1 = mean(prec_mean), median_prec_dec1 = median(prec_mean), sd_prec_dec1 =  sd(prec_mean)) , .(dataset)]

prec_annual_trend <- merge(prec_annual_trend, prec_global_stats_dummy, by = "dataset")

prec_global_stats_dummy <- prec_annual[ time_period == "2010-2019", .(mean_prec_dec2 = mean(prec_mean), median_prec_dec2 = median(prec_mean), sd_prec_dec2 =  sd(prec_mean)) , .(dataset)]

prec_annual_trend <- merge(prec_annual_trend, prec_global_stats_dummy, by = "dataset")

## Save data ----
saveRDS(prec_annual_trend, paste0(PATH_SAVE_CHANGING_PREC, "01_b_prec_annual_trend_bootstrap.rds"))  


###############################################################

