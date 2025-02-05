ipcc_cor <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "correlation_ipcc.rds"))

ggplot(ipcc_cor[correlation < 1])+
  geom_tile(aes(x = rn , y = dataset, fill = correlation))+
  facet_wrap(~IPCC_ref_region)+
  scale_fill_gradient2(midpoint = 0)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "heatplot_land_cover_correlation.png"), 
       width = 8, height = 8)
