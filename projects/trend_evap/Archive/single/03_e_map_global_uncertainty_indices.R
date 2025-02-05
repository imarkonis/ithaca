# Map of DCI and probability groups
source('source/evap_trend.R')

## Data ----
### Input data generated in 01_d
evap_trend_lon_lat <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))

## Plot results ----
### DCI ----
evap_trend_lon_lat[, DCI_theil_sen_brks := cut(DCI_theil_sen, breaks = c(1, 0.35, 0.01, -0.01, -0.35, -1))]

cols_div <- c("darkred","darkorange","gray80", "steelblue1","darkblue")

ggplot(evap_trend_lon_lat[!is.na(DCI_theil_sen_brks)])+
  geom_tile(aes(x = lon, y = lat, fill = DCI_theil_sen_brks))+
  scale_fill_manual(values = rev(cols_div)) +
  labs(fill = "DCI")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_grid_DCI_theil_sen.png"), 
       width = 12, height = 8)


### Probability groups ----
ggplot(evap_trend_lon_lat)+
  geom_tile(aes(x = lon, y = lat, fill = trend))+
  labs(fill = "Trend")+
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_probability_theil_sen.png"), 
       width = 12, height = 8)


ggplot(evap_trend_lon_lat)+
  geom_tile(aes(x = lon, y = lat, fill = "trend"), col = "gray10")+
  geom_tile(data = evap_trend_lon_lat[trend == "uncertain"], aes(x = lon, y = lat, fill = "uncertain"),  col = "deeppink1")+
  geom_tile(data = evap_trend_lon_lat[trend == "no trend"], aes(x = lon, y = lat, fill = "no trend"),  col = "gray80")+
  scale_fill_manual(values = c("uncertain" = "deeppink1","no trend" = "gray80", "trend" = "gray10"))+
  labs(fill = "")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_uncertain_theil_sen.png"), 
       width = 12, height = 8)

