# Plots global maps of dataset agreement classes 
source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)

## Data
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(evap_mask$evap_quant_dataset_agreement) <- c("High", "Above average", "Average",
                                                    "Below average", "Low")


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


## World and Land borders
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels
labs_y <- data.frame(lon = -170, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°", ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

## Figures

### distribution index
to_plot_sf <- distribution[, .(lon, lat, agreement_fac)
][, value := as.numeric(agreement_fac)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_distribution_index <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), labels = levels(distribution$agreement_fac)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Distribution\nagreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


### Q75 - Q25

evap_mask[, Qdiff := ens_mean_q75-ens_mean_q25]

evap_mask[, Qdiff_brk := cut(Qdiff, breaks = c(0,50,100, 150, 200, 1145))]

to_plot_sf <- evap_mask[, .(lon, lat, Qdiff_brk)
][, value := as.numeric(Qdiff_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_quantile_range <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = colset_prec_quant[c(1,3,5,7,9)], labels = levels(evap_mask$Qdiff_brk)) +
  scale_color_manual(values = colset_prec_quant[c(1,3,5,7,9)],
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile\nrange [mm/year]") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

### Relative dataset agreement
to_plot_sf <- evap_mask[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_rel_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5),
                    labels = levels(evap_mask$rel_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile\nagreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

### Relative dataset agreement conditioned by evaporation rate
to_plot_sf <- evap_mask[, .(lon, lat, evap_quant_dataset_agreement)
][, value := as.numeric(evap_quant_dataset_agreement )]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_evap_quant_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), 
                    labels = levels(evap_mask$evap_quant_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$evap_quant_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile\nagreement\nper evap. quantile") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))



gg_agreement_v2 <- ggarrange(fig_quantile_range, fig_rel_dataset_agreement, 
                             fig_evap_quant_dataset_agreement, fig_distribution_index, 
                             labels = c('a', 'b', 'c', 'd'), common.legend = FALSE,
                             legend = 'right', align = 'hv',
                             nrow = 2, ncol = 2)

jpeg(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig2_main_dataset_range_agreement_maps.png"), 
     width = 24, height = 12, res = 300, units = 'in')
gg_agreement_v2
dev.off()
