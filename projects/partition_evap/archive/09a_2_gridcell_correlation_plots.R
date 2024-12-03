# plot correlations and calcualte area averages ----
source('source/partition_evap.R')


global_correlation <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "gridwise_correlation.rds"))

ggplot(global_correlation[dataset == "fldas" & rn != "fldas"])+
  geom_tile(aes(x = lon, y = lat, fill = correlation))+
  facet_wrap(~rn)+
  theme_bw()+
  scale_fill_gradient2(midpoint = 0)
