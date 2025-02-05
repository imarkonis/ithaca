# Figure supporting methods ----

source('source/partition_evap.R')

## data ----

landcover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_land_cover.rds"))
landcover[KS_test_p < 0.05, agreement := "Not Matching"]
landcover[KS_test_p >= 0.05, agreement := "Matching"]

ggplot(landcover[land_cover_short_class == "Barren"], aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = c("Not Matching" = "black","Matching" = "red"))+
  labs(fill = "", x = "", y = "")+
  theme_bw()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig6_method_distribution_agreement_example.png"), 
       width = 4, height = 2.5)
