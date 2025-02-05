# Plots the datasets that are closests/furthest to the ensemble mean

source('source/partition_evap.R')
source('source/graphics.R')


#Data
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_land_cover_datasets_for_plot.rds"))
plotlist = list()

for (i in 1:(length(unique(data$land_cover_short_class)))){
    land_cover <- unique(data$land_cover_short_class)[i]
    plotlist[[i]] <- 
      ggplot(data[land_cover_short_class == land_cover], aes(x = dataset, y = diff, fill = performance))+
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
      coord_flip()+ylab("Deviation from ensemble mean")+
      ylim(-0.75, 1.3)+
      ggtitle(land_cover)+
      theme_bw()+
      theme(axis.title.y = element_blank(), axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 11, hjust = 0.5),
            axis.text.y = element_text(colour = data[land_cover_short_class == land_cover]$label_color),
            legend.position = "none",
            plot.margin = margin(0,0.5,0,0.5, "cm"))
}

library(grid)
ggarrange(plotlist[[1]], plotlist[[2]],plotlist[[3]],
          plotlist[[5]],
          plotlist[[7]],plotlist[[8]],plotlist[[9]],
          plotlist[[10]],plotlist[[4]],
          nrow = 5, ncol = 2,labels = c("a", "b", "c", "d", "e", "f", "g", "h","i"))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "main/fig_6_performance_landcover.png"), width = 8, height = 12)

