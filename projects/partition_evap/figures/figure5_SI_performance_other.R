# tile plots for over and underestimator and best
# heatplots
source('source/partition_evap.R')
source('source/graphics.R')

cols_data_dt <- data.table(dataset = names(cols_data), colors = cols_data) 

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_land_cover_datasets_for_plot.rds"))
data_count <- data[performance == "Over", .N, .(performance, dataset)]
data_count <- rbind(data_count, data.table(performance = "Over", dataset = "gldas-vic", N = 0))
data_count[, rank := rank(N)]
data_count <- merge(data_count, cols_data_dt, by = "dataset")  
data_count <- data_count[order(rank)]
data[, dataset := factor(dataset, levels = data_count$dataset)]

performance_landcover <- ggplot(data[land_cover_short_class != "Other" & land_cover_short_class != "Global" ],
                                aes(x = land_cover_short_class, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "Land cover types", y = "Dataset", fill = "Deviation\nto ensemble   \nmean")

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_elevation_datasets_for_plot.rds"))
data_count <- data[performance == "Over", .N, .(performance, dataset)]
data_count <- rbind(data_count,
                    data.table(performance = "Over", dataset = "terraclimate", N = 0))
data_count <- rbind(data_count,
                    data.table(performance = "Over", dataset = "gleam", N = 0))
data_count <- rbind(data_count, data.table(performance = "Over", dataset = "gldas-vic", N = 0))
data_count <- rbind(data_count, data.table(performance = "Over", dataset = "etmonitor", N = 0))


data_count[, rank := rank(N)]
data_count <- merge(data_count, cols_data_dt, by = "dataset")  
data_count <- data_count[order(rank)]
data[, dataset := factor(dataset, levels = data_count$dataset)]

performance_elev <- ggplot(data, aes(x = elev_class, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "Elevation", y = "Dataset", fill = "")


data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap_datasets_for_plot.rds"))
data_count <- data[performance == "Over", .N, .(performance, dataset)]
data_count[, rank := rank(N)]
data_count <- merge(data_count, cols_data_dt, by = "dataset")  
data_count <- data_count[order(rank)]
data[, dataset := factor(dataset, levels = data_count$dataset)]

performance_evap <- ggplot(data, aes(x = evap_quant, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "Evaporation quantile", y = "Dataset", fill = "")



data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_IPCC_datasets_for_plot.rds"))
data_count <- data[performance == "Over", .N, .(performance, dataset)]
data_count[, rank := rank(N)]
data_count <- merge(data_count, cols_data_dt, by = "dataset")  
data_count <- data_count[order(rank)]
data[, dataset := factor(dataset, levels = c(data_count$dataset))]

IPCC_Africa <- c("CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("ARP","EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

data[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
data[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
data[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
data[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
data[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
data[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]

performance_africa <- ggplot(data[region %in% "Africa"], aes(x = IPCC_ref_region, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        legend.position = "none",
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "IPCC Africa", y = "Dataset", fill = "")



performance_asia <- ggplot(data[region %in% "Asia"], aes(x = IPCC_ref_region, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        legend.position = "none",
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "IPCC Asia", y = "Dataset", fill = "")


performance_aus <- ggplot(data[region %in% "Australasia"], aes(x = IPCC_ref_region, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        legend.position = "none",
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "IPCC Australasia", y = "Dataset", fill = "")


performance_eur <- ggplot(data[region %in% "Europe"], aes(x = IPCC_ref_region, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        legend.position = "none",
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "IPCC Europe", y = "Dataset", fill = "")

performance_nam <- ggplot(data[region %in% "North America"], aes(x = IPCC_ref_region, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        legend.position = "none",
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "IPCC North America", y = "Dataset", fill = "")


performance_sam <- ggplot(data[region %in% "South America"], aes(x = IPCC_ref_region, y = dataset, fill = performance))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        legend.position = "none",
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "IPCC South America", y = "Dataset", fill = "")


## composite SI figure ----
###  add labels
top <- ggarrange(performance_landcover, performance_elev, performance_evap, 
                 common.legend = T, nrow = 1, align = "hv", widths = c(8,6,9))
ipcc_top <- ggarrange(performance_africa, performance_asia, performance_aus, widths = c(1,1,0.7), nrow = 1, ncol = 3)

ipcc_bot <- ggarrange(performance_eur, performance_nam, performance_sam, widths = c(0.7,1,0.9), nrow = 1, ncol = 3)
all <- ggarrange(top, ipcc_top, ipcc_bot, nrow = 3, heights = c(2.5,2,2))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig5_SI_dataset_performance_other.png"), 
       width = 8, height = 12)
