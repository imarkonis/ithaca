# Read fluxnet sites and see if sites make a difference ----
source('source/partition_evap.R')
source('source/graphics.R')

library(rnaturalearth)

## Data ---

file <- list.files(path = paste0(PATH_SAVE_PARTITION_EVAP, "/Fluxnet"), full.names = TRUE)
fluxnet_station <- as.data.table(read.csv(file))

colnames(fluxnet_station)
fluxnet_station[FLUXNET2015 != "", .N]

fluxnet_station[FLUXNET2015 != "", summary(LOCATION_ELEV)]

## plot

ggplot(fluxnet_station[FLUXNET2015 != ""])+
  geom_point(aes(y = LOCATION_LAT, x = LOCATION_LONG))

## Distribution ----
distribution <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise.rds")))
distribution <-  distribution[!is.na(index),]
distribution[, summary(index)]

### Relative dataset agreement at quantile 0.1, 0.3, 0.7. 0.9
quant_thr_0_1 <- quantile(distribution$index, c(0.1))
quant_thr_0_3 <- quantile(distribution$index, c(0.3))
quant_thr_0_7 <- quantile(distribution$index, c(0.7))
quant_thr_0_9 <- quantile(distribution$index, c(0.9))

distribution[index > quant_thr_0_9, agreement_fac := ordered(1, labels = "High")]
distribution[index > quant_thr_0_7 & index <= quant_thr_0_9, agreement_fac := ordered(2, labels = "Above average")]
distribution[index > quant_thr_0_3 & index <= quant_thr_0_7, agreement_fac := ordered(3, labels = "Average")]
distribution[index > quant_thr_0_1 & index <= quant_thr_0_3, agreement_fac := ordered(4, labels = "Below average")]
distribution[index <= quant_thr_0_1, agreement_fac := ordered(5, labels = "Low")] 

## Quartile 

evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")

lats <- unique(distribution$lat)
lons <- unique(distribution$lon)

fluxnet_station[, id_match := which.min(sqrt((distribution$lon - LOCATION_LONG)^2 + (distribution$lat - LOCATION_LAT)^2)), .I]
fluxnet_station[, dist_agreement := distribution$agreement_fac[id_match]]

fluxnet_station[, id_match_quart := which.min(sqrt((evap_mask$lon - LOCATION_LONG)^2 + (evap_mask$lat - LOCATION_LAT)^2)), .I]
fluxnet_station[, quart_agreement := evap_mask$rel_dataset_agreement[id_match]]

expectation <- data.table(agreement = c("High", "Above average", "Average",
                                         "Below average", "Low"),
                          count = c(212*0.1, 212*0.2, 212*0.4, 212*0.2, 212*0.1))

distribution <- ggplot(fluxnet_station[FLUXNET2015 != ""])+
  geom_bar(aes(x = dist_agreement, fill = dist_agreement), stat = "count", width = 0.8)+
  geom_bar(data = expectation, aes(x = agreement, y = count, col = "Expectation"), stat = "identity", fill = NA)+
  scale_fill_manual(values = rev(colset_RdBu_5),
                     guide = "none") +
  scale_color_manual(values = ("Expectation" = "black"))+
  labs(x = "Distribution agreement", y = "Grid count", col = "")+
  theme_bw()+
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  ylim(c(0, 100))

quartile <- ggplot(fluxnet_station[FLUXNET2015 != ""])+
  geom_bar(aes(x = quart_agreement, fill = quart_agreement), stat = "count", width = 0.8)+
  geom_bar(data = expectation, aes(x = agreement, y = count, col = "Expectation"), stat = "identity", fill = NA)+
  scale_fill_manual(values = rev(colset_RdBu_5),
                    guide = "none") +
  scale_color_manual(values = ("Expectation" = "black"))+
  labs(x = "Quartile agreement", y = "Grid count", col = "")+
  theme_bw()+
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  
  ylim(c(0, 100))

ggarrange(distribution, quartile, labels = c("a", "b"), common.legend = T)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig_SI_fluxnet_dataset_agreement.png"), 
       width = 8, height = 3.5)
