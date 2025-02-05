# Map of DCI and probability groups
source('source/evap_trend.R')

## Data ----
### Input data generated in 01_e
evap_trend<- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_uncertainty_dataset_leftout.rds"))

evap_trend_no_etsynthesis <- evap_trend[dataset_leftout == "etsynthesis"]

## Plot results ----
### Probability groups ----
ggplot(evap_trend_no_etsynthesis)+
  geom_tile(aes(x = lon, y = lat, fill = trend))+
  labs(fill = "Trend")+
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_probability_theil_sen_no_etsynthesis.png"), 
       width = 12, height = 8)

world <- evap_trend_no_etsynthesis[, sum(area)]
trend_fraction <- evap_trend_no_etsynthesis[, (sum(area)/world), trend]
trend_fraction[, sum(V1)]

ggplot(trend_fraction, aes(x = "", y = V1*100, fill = trend))+
  geom_bar(stat = "identity")+
  labs(y = "global terrestrial area fraction [%]", fill = "Trend")+
  theme_void() +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  coord_polar("y", start = 0)+
  geom_text(aes(x = 1.6, label = round(V1*100, digits = 1)),
            position = position_stack(vjust = 0.5), show.legend = FALSE)

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "pie_global_probability_groups_noetsynthesis.png"), 
       width = 6, height = 6)

ggplot(evap_trend_no_etsynthesis)+
  geom_tile(aes(x = lon, y = lat, fill = "trend"), col = "gray10")+
  geom_tile(data = evap_trend_no_etsynthesis[trend == "uncertain"], aes(x = lon, y = lat, fill = "uncertain"),  col = "deeppink1")+
  geom_tile(data = evap_trend_no_etsynthesis[trend == "no trend"], aes(x = lon, y = lat, fill = "no trend"),  col = "gray80")+
  scale_fill_manual(values = c("uncertain" = "deeppink1","no trend" = "gray80", "trend" = "gray10"))+
  labs(fill = "")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_uncertain_theil_sen_no_etsynthesis.png"), 
       width = 12, height = 8)

