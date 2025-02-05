# Trend of global product averages  ----
source('source/evap_trend.R')

library("Kendall")
library(RobustLinearReg)

## Data  ----
evap_annual <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))
evap_annual <- evap_annual[!(dataset == "etmonitor" & year == 2000), ]
evap_annual_test <- evap_annual[!(year < 2003), ]

evap_annual[, evap_anomaly := evap_mean - mean(evap_mean), .(dataset)]
evap_annual[ year < 2010, time_period := "2000-2009", ]
evap_annual[ year >= 2010, time_period := "2010-2019", ]

## Plot Anomalies  ----

ggplot(evap_annual)+
  geom_line(aes(x = year, y = evap_mean, col = dataset))+
  scale_color_manual(values = cols_data) + 
  theme_bw()


ggplot(evap_annual)+
  geom_line(aes(x = year, y = evap_anomaly, col = dataset))+
  scale_color_manual(values = cols_data) + 
  #  ylim(c(-50,50))+
  theme_bw()

## Analysis  ----
evap_annual_trend <- evap_annual[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                     lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                     kendall_tau = Kendall(evap_mean, year)$tau,
                                     kendall_p_value = Kendall(evap_mean, year)$sl,
                                     theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                     theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                     siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                     siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
                                     ), 
                                 .(dataset)]

evap_annual_trend_test <- evap_annual_test[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                     lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                     kendall_tau = Kendall(evap_mean, year)$tau,
                                     kendall_p_value = Kendall(evap_mean, year)$sl,
                                     theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                     theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                     siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                     siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
), 
.(dataset)]

evap_annual_trend[lm_p_value > 0.05,significant_lm:= FALSE] 
evap_annual_trend[lm_p_value <= 0.05,significant_lm:= TRUE] 

evap_annual_trend[kendall_p_value > 0.05,significant_kendall:= FALSE] 
evap_annual_trend[kendall_p_value <= 0.05,significant_kendall:= TRUE] 

evap_annual_trend[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
evap_annual_trend[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 

evap_annual_trend[siegel_p_value > 0.05,significant_siegel:= FALSE] 
evap_annual_trend[siegel_p_value <= 0.05,significant_siegel:= TRUE] 

evap_annual_trend[significant_theil_sen*theil_sen_slope > 0, trend_score := 1  ]
evap_annual_trend[significant_theil_sen*theil_sen_slope < 0, trend_score := -1  ]
evap_annual_trend[significant_theil_sen == FALSE, trend_score := 0  ]

evap_annual_trend[trend_score == 1, trend_direction := "positive"]
evap_annual_trend[trend_score == -1, trend_direction := "negative"]
evap_annual_trend[trend_score == 0, trend_direction := "no trend"]

evap_global_stats_dummy <- evap_annual[ time_period == "2000-2009", .(mean_evap_dec1 = mean(evap_mean), median_evap_dec1 = median(evap_mean), sd_evap_dec1 =  sd(evap_mean)) , .(dataset)]

evap_annual_trend <- merge(evap_annual_trend, evap_global_stats_dummy, by = "dataset")

evap_global_stats_dummy <- evap_annual[ time_period == "2010-2019", .(mean_evap_dec2 = mean(evap_mean), median_evap_dec2 = median(evap_mean), sd_evap_dec2 =  sd(evap_mean)) , .(dataset)]

evap_annual_trend <- merge(evap_annual_trend, evap_global_stats_dummy, by = "dataset")

## Save data ----
saveRDS(evap_annual_trend, paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend.rds"))  

## Save Table ----
evap_trends <- evap_annual_trend[,.(dataset, theil_sen_slope)]
write.table(evap_trends[order(-theil_sen_slope)], file = paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_evap_trends_by_product.csv"), row.names = F)






