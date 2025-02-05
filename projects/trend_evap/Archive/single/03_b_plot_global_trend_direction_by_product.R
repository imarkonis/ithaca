# plot global trend direction by product ----
source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/trend_evap/01_b
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend.rds"))  

## Plot trend direction ----
ggplot(evap_annual_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "global_trend_direction_by_product.png"), 
       width = 12, height = 8)


