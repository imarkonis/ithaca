# Correlation of trends ----

source('source/evap_trend.R')
source('source/geo_functions.R')
source('source/graphics.R')
## libraries ----

library(corrplot)
library(ggcorrplot)

## Data ----
### Input Data generated in projects/evap_trend/01_a
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon.rds"))  

### correlation between datasets ----

evap_trend_dcast <- dcast(evap_trend, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")

cor_data <- cor(evap_trend_dcast[,-c(1,2)])
melt(as.data.table(cor_data, keep.rownames = T))

cor.test(evap_trend_dcast$fldas, evap_trend_dcast$`gldas-noah`)

p.mat <- cor_pmat(evap_trend_dcast[,-c(1,2)])
p.mat <- as.data.table(p.mat, keep.rownames = T)
# plot
corrplot(cor_data, method = 'number', order = 'AOE', diag = FALSE, type = "upper")
ggcorrplot(cor_data, hc.order = TRUE, type = "lower",
           lab = TRUE, p.mat = p.mat)

## Save Data ----
saveRDS(cor_data, paste0(PATH_SAVE_EVAP_TREND, "global_correlation_trends_dataset.rds"))
saveRDS(p.mat, paste0(PATH_SAVE_EVAP_TREND, "global_correlation_p_value_trends_dataset.rds"))
