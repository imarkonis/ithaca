# Plots the datasets that are closests/furthest to the ensemble mean

source('source/partition_evap.R')
source('source/graphics.R')

#Data
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_biome_datasets_for_plot.rds"))
plotlist = list()

y_labs <- c(rep("", 11), rep("Deviation from \nensemble mean", 3))

for (i in 1:(length(unique(data$biome_short_class)))){
  biome <- unique(data$biome_short_class)[i]
  plotlist[[i]] <- 
    ggplot(data[biome_short_class == biome], aes(x = dataset, y = diff, fill = performance))+
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
    coord_flip()+ylab(y_labs[i])+
    ylim(-0.5, 0.6)+
    ggtitle(biome)+
    theme_bw()+
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          axis.text.y = element_text(colour = data[biome_short_class == biome]$label_color),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], plotlist[[6]], plotlist[[7]],
          plotlist[[8]], plotlist[[9]], plotlist[[10]], plotlist[[11]], plotlist[[12]], plotlist[[13]], plotlist[[14]],
          nrow = 5, ncol = 3, labels = letters[seq( from = 1, to = i )], align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_performance_biome.png"), width = 8, height = 12)


data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_IPCC_datasets_for_plot.rds"))

# IPCC ####
IPCC_Africa <- c("ARP", "CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

plotlist = list()
y_labs <- c(rep("", 6), rep("Deviation from \nensemble mean", 3))

for (i in 1:(length(IPCC_Africa))){
  IPCC <- IPCC_Africa[i]
  plotlist[[i]] <- 
    ggplot(data[IPCC_ref_region == IPCC], aes(x = dataset, y = diff, fill = performance))+
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
    coord_flip()+ylab(y_labs[i])+
    ylim(-0.7, 0.7)+
    ggtitle(IPCC)+
    theme_bw()+
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          axis.text.y = element_text(colour = data[IPCC_ref_region == IPCC]$label_color),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], plotlist[[6]], plotlist[[7]],
          plotlist[[8]], plotlist[[9]], 
          nrow = 3, ncol = 3, labels = letters[seq( from = 1, to = i )], align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_performance_IPCC_Africa.png"), width = 8, height = 8)


plotlist = list()
y_labs <- c(rep("", 7), rep("Deviation from \nensemble mean", 3))

for (i in 1:(length(IPCC_Asia))){
  IPCC <- IPCC_Asia[i]
  plotlist[[i]] <- 
    ggplot(data[IPCC_ref_region == IPCC], aes(x = dataset, y = diff, fill = performance))+
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
    coord_flip()+ylab(y_labs[i])+
    ylim(-0.7, 0.7)+
    ggtitle(IPCC)+
    theme_bw()+
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          axis.text.y = element_text(colour = data[IPCC_ref_region == IPCC]$label_color),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], plotlist[[6]], plotlist[[7]],
          plotlist[[8]], plotlist[[9]], plotlist[[10]],
          nrow = 4, ncol = 3, labels = letters[seq( from = 1, to = i )], align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_performance_IPCC_Asia.png"), width = 8, height = 9)

plotlist = list()
y_labs <- c(rep("", length(IPCC_Australasia)-3), rep("Deviation from \nensemble mean", 3))

for (i in 1:(length(IPCC_Australasia))){
  IPCC <- IPCC_Australasia[i]
  plotlist[[i]] <- 
    ggplot(data[IPCC_ref_region == IPCC], aes(x = dataset, y = diff, fill = performance))+
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
    coord_flip()+ylab(y_labs[i])+
    ylim(-0.7, 0.7)+
    ggtitle(IPCC)+
    theme_bw()+
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          axis.text.y = element_text(colour = data[IPCC_ref_region == IPCC]$label_color),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], 
          nrow = 2, ncol = 3, labels = letters[seq( from = 1, to = i )], align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_performance_IPCC_Australasia.png"), width = 8, height = 5)


plotlist = list()
y_labs <- c(rep("", length(IPCC_Europe)-3), rep("Deviation from \nensemble mean", 3))

for (i in 1:(length(IPCC_Europe))){
  IPCC <- IPCC_Europe[i]
  plotlist[[i]] <- 
    ggplot(data[IPCC_ref_region == IPCC], aes(x = dataset, y = diff, fill = performance))+
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
    coord_flip()+ylab(y_labs[i])+
    ylim(-0.7, 0.7)+
    ggtitle(IPCC)+
    theme_bw()+
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          axis.text.y = element_text(colour = data[IPCC_ref_region == IPCC]$label_color),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], 
          nrow = 2, ncol = 3, labels = letters[seq( from = 1, to = i )], align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_performance_IPCC_Europe.png"), width = 8, height = 5)


plotlist = list()
y_labs <- c(rep("", length(IPCC_Namerica)-3), rep("Deviation from \nensemble mean", 3))

for (i in 1:(length(IPCC_Namerica))){
  IPCC <- IPCC_Namerica[i]
  plotlist[[i]] <- 
    ggplot(data[IPCC_ref_region == IPCC], aes(x = dataset, y = diff, fill = performance))+
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
    coord_flip()+ylab(y_labs[i])+
    ylim(-0.7, 0.7)+
    ggtitle(IPCC)+
    theme_bw()+
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          axis.text.y = element_text(colour = data[IPCC_ref_region == IPCC]$label_color),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], plotlist[[6]], plotlist[[7]],
          plotlist[[8]], 
          nrow = 3, ncol = 3, labels = letters[seq( from = 1, to = i )], align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_performance_IPCC_Namerica.png"), width = 8, height = 8)


plotlist = list()
y_labs <- c(rep("", length(IPCC_Samerica)-3), rep("Deviation from \nensemble mean", 3))

for (i in 1:(length(IPCC_Samerica))){
  IPCC <- IPCC_Samerica[i]
  plotlist[[i]] <- 
    ggplot(data[IPCC_ref_region == IPCC], aes(x = dataset, y = diff, fill = performance))+
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("best"= "firebrick","overestimated" = "dodgerblue4", "underestimated" = "forestgreen"))+
    coord_flip()+ylab(y_labs[i])+
    ylim(-0.7, 0.7)+
    ggtitle(IPCC)+
    theme_bw()+
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          axis.text.y = element_text(colour = data[IPCC_ref_region == IPCC]$label_color),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], plotlist[[6]], plotlist[[7]],
          nrow = 3, ncol = 3, labels = letters[seq( from = 1, to = i )], align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_performance_IPCC_Samerica.png"), width = 8, height = 8)
