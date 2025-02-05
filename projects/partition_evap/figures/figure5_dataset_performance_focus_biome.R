# tile plots for over and underestimator and best
# heatplots
source('source/partition_evap.R')
source('source/graphics.R')

evap_summary <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "grid_performance_datasets.rds"))
cols_data_dt <- data.table(dataset = names(cols_data), colors = cols_data) 

evap_summary_over <- evap_summary[performance == "Over"]
evap_summary_over[, rank := rank(volume_fraction)]
evap_summary_over <- merge(evap_summary_over, cols_data_dt, by = "dataset")  
evap_summary_over <- evap_summary_over[order(rank)]
evap_summary[, dataset := factor(dataset, levels = evap_summary_over$dataset)]


performance_global <- ggplot(evap_summary)+
  geom_bar(aes(x = dataset, y = volume_fraction, fill = performance), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.y = element_text(colour = evap_summary_over$colors))+
  labs(x = "Datasets", y = "Global volume fraction [-]", fill = "Deviation\nto ensemble   \nmean")+
  coord_flip()

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_biome_datasets_for_plot.rds"))
data_count <- data[performance == "Over", .N, .(performance, dataset)]
data_count <- rbind(data_count, data.table(performance = "Over", dataset = "gldas-vic", N = 0))
data_count[, rank := rank(N)]

data_count <- merge(data_count, cols_data_dt, by = "dataset")  
data_count <- data_count[order(rank)]
data[, dataset := factor(dataset, levels = data_count$dataset)]

performance_biome <- ggplot(data,
                                aes(x = biome_short_class, y = dataset, fill = performance))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Over" = colset_RdBu_5[5], "Under" = colset_RdBu_5[1]))+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
        axis.text.y = element_text(colour = data_count$colors))+
  labs(x = "Biomes", y = "Dataset", fill = "Deviation\nto ensemble   \nmean")


top_row <- ggarrange(performance_global, performance_biome, 
                     labels = c("a", "b"), nrow = 1, 
                     common.legend = T, align = "v")



global <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products.rds"))
q10 <- quantile(global$volume_fraction, c(0.1))
q30 <- quantile(global$volume_fraction, c(0.3))
q70 <- quantile(global$volume_fraction, c(0.7))
q90 <- quantile(global$volume_fraction, c(0.9))

global[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                             labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

dataset_global <- ggplot(global, aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = colset_RdBu_5)+
  labs(fill = "Distribution\nagreement    ", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "Global")

biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))
q10 <- quantile(biome$volume_fraction, c(0.1))
q30 <- quantile(biome$volume_fraction, c(0.3))
q70 <- quantile(biome$volume_fraction, c(0.7))
q90 <- quantile(biome$volume_fraction, c(0.9))

biome[, fraction_fac := cut(volume_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                                labels = c("Low", "Below average", "Average", "Above average", "High"))]

dataset_cols <- c("Low" =  colset_RdBu_5[1], "Below average" = colset_RdBu_5[2],
                  "Average" = colset_RdBu_5[3], "Above average" = colset_RdBu_5[4], 
                  "High" = colset_RdBu_5[5])

dataset_tundra <- ggplot(biome[biome_short_class %in% "Tundra"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = dataset_cols)+
  labs(fill = "Distribution\nagreement    ", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "Tundra")

dataset_desert <- ggplot(biome[biome_short_class %in% "Deserts"], 
                         aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_cols)+
  labs(fill = "Distribution\nagreement    ", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "Deserts")

dataset_trop_forest <- ggplot(biome[biome_short_class %in% "T/S Moist BL Forests"], 
                            aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_cols)+
  labs(fill = "Distribution\nagreement    ", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "T/S Moist BL Forests")


dataset_gg <- ggarrange(dataset_global,dataset_desert, dataset_tundra, dataset_trop_forest, 
                        nrow = 2, ncol = 2,
                        common.legend = T, align = "hv",
                        labels = c("c", "d", "e", "f"))

## composite figure ----

ggarrange(top_row, dataset_gg, nrow = 2, heights = c(0.55, 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig5_dataset_performance_biome.png"), 
       width = 8, height = 12)
