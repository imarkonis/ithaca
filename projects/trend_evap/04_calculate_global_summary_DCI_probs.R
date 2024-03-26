# summary metrics indices
source('source/evap_trend.R')

evap_indices <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))
evap_indices[, DCI_theil_sen_brks := cut(DCI_theil_sen, breaks = c(1, 0.35, 0.01, -0.01, -0.35, -1))]

world <- evap_indices[, sum(area)]
trend_fraction <- evap_indices[, (sum(area)/world), trend]
trend_fraction[, sum(V1)]

DCI_fraction <- evap_indices[, sum(area)/world, DCI_theil_sen_brks]
DCI_fraction[, sum(V1)]
DCI_fraction <- DCI_fraction[complete.cases(DCI_fraction)]
DCI_fraction[, DCI := factor(DCI_theil_sen_brks, levels = rev(levels(DCI_fraction$DCI_theil_sen_brks)),
                                 labels = c("positive strongly agree","positive agree", "uncertain/no trend",
                                            "negative agree",
                                            "negative strongly agree"
                                 ), ordered = TRUE)]

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
  
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "pie_global_probability_groups.png"), 
       width = 6, height = 6)

ggplot(DCI_fraction, aes(x = "", y = V1*100, fill = DCI))+
  geom_bar(stat = "identity")+
  labs(y = "global terrestrial area fraction [%]", fill = "DCI")+
  theme_void() +
  coord_polar("y", start = 0)+
  geom_text(aes(x = 1.6, label = round(V1*100, digits = 1)),
            position = position_stack(vjust = 0.5), show.legend = FALSE)+
  scale_fill_manual(values = c("negative strongly agree" = "darkblue", 
                               "negative agree" = "steelblue1", 
                               "positive strongly agree" = "darkred", 
                               "positive agree" = "darkorange", 
                               "uncertain/no trend" = "gray80"
  ))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "pie_global_DCI_theil_sen.png"), 
       width = 6, height = 6)
