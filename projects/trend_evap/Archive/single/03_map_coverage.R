# Map of dataset count
source('source/evap_trend.R')
# Data
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope.rds"))  
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_mean <- evap_datasets[, (mean_evap = mean(evap)), .(lon, lat, dataset)]
evap_mean[, dataset_count := .N, .(lon, lat)]

evap_merra2 <- evap_datasets[dataset == "merra2"]

# plot

ggplot(evap_trend)+
  geom_tile(aes(x = lon, y = lat, fill = as.factor(dataset_count)))+
  labs(fill = "Dataset count")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_dataset_count.png"), 
       width = 12, height = 8)

ggplot(evap_trend[dataset_count > 9])+
  geom_tile(aes(x = lon, y = lat, fill = as.factor(dataset_count)))+
  labs(fill = "Dataset count")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_dataset_count_10_or_more_trend.png"), 
       width = 12, height = 8)

ggplot(evap_trend)+
  geom_tile(aes(x = lon, y = lat))+
  facet_wrap(.~dataset)+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_global_coverage_by_product_trend.png"), 
       width = 12, height = 8)

ggplot(evap_mean[dataset_count > 9])+
  geom_tile(aes(x = lon, y = lat, fill = as.factor(dataset_count)))+
  labs(fill = "Dataset count")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_dataset_count_10_or_more.png"), 
       width = 12, height = 8)

ggplot(evap_mean)+
  geom_tile(aes(x = lon, y = lat))+
  facet_wrap(.~dataset)+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_global_coverage_by_product.png"), 
       width = 12, height = 8)
