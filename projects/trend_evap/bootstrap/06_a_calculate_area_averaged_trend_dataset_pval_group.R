# Dataset trend averages per p-value group ----
source('source/evap_trend.R')
source('source/geo_functions.R')

evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  

evap_trend[p < 1e-6, p := 1e-6]

evap_trend[, pval_brk := cut(p, breaks = c(0, 0.01, 0.05, 0.1, 0.2, 1))]

grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend <- grid_cell_area[evap_trend, on = .(lon, lat)]

evap_trend[, evap_vol_trend := slope*M2_TO_KM2*MM_TO_KM*area]
evap_trend[, area_trend_p_valsum := sum(area), .(pval_brk, dataset)]

evap_trend_averages <- evap_trend[, .(mean_slope = mean(slope), mean_vol_slope = mean(evap_vol_trend),
                                      mean_area_slope = sum(slope*area)/sum(area)), .(pval_brk, dataset)]


saveRDS(evap_trend_averages, paste0(PATH_SAVE_EVAP_TREND, "dataset_average_trend_per_pval_groupe.rds")) 

cols_pval <- c("darkblue","darkorchid4","darkorchid1","lightcoral","gold")

ggplot(evap_trend_averages)+
  geom_point(aes(x = dataset, col = pval_brk, y = mean_area_slope))+
  theme_bw()+
  labs(color = "P-value threshold", x = "Dataset", y = "Average trend [mm.yr-2]")+
  scale_color_manual(values = cols_pval)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

## ordered according to p-val 0.01 slope

dataset_rank <- evap_trend_averages[pval_brk == "(0,0.01]"]
dataset_rank[, rank := rank(mean_area_slope)]
dataset_rank <- dataset_rank[order(rank)]

evap_trend_averages[, dataset := factor(dataset, levels = dataset_rank$dataset)]

ggplot(evap_trend_averages)+
  geom_point(aes(x = dataset, col = pval_brk, y = mean_area_slope))+
  theme_bw()+
  labs(color = "P-value threshold", x = "Dataset", y = "Average trend [mm.yr-2]")+
  scale_color_manual(values = cols_pval)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "Dataset_mean_slope_pval.png"), 
       width = 8, height = 8)

##

evap_trend[, dataset := factor(dataset, levels = dataset_rank$dataset)]

ggplot(evap_trend)+
  geom_boxplot(aes(x = dataset, y = slope, col = pval_brk),  outliers = FALSE)+
  theme_bw()+
  labs(color = "P-value threshold", x = "Dataset", y = "Trend [mm.yr-2]")+
  scale_color_manual(values = cols_pval)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(
          margin = margin(r = 10, unit = "pt")),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "Dataset_boxplot_slope_pval.png"), 
       width = 8, height = 8)
