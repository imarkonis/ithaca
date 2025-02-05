# Plots global maps of dataset agreement classes 
source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)

## Figure a ----

### Data ----
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))
evap_annual_vol[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
evap_annual_vol[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
evap_annual_vol[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
evap_annual_vol[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

evap_annual_vol <- evap_annual_vol[!(dataset == "etmonitor" & year == 2000), ]

gg_volume <- ggplot(evap_annual_vol, aes(x = 0, y = evap_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Global annual volume [km'^3,']')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


## Figue b-d data ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,"dataset_agreement_grid_wise.rds"))


### World and Land borders ----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

### Labels ----
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

### distribution index ----
to_plot_sf <- dataset_agreement_grid_wise[, .(lon, lat, dist_dataset_agreement)
][, value := as.numeric(dist_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_distribution_index <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), labels = levels(dataset_agreement_grid_wise$dist_dataset_agreement)) +
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


### IQR agreement ----

to_plot_sf <- dataset_agreement_grid_wise[, .(lon, lat, IQR_agreement)
][, value := as.numeric(IQR_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_quantile_range <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), labels = levels(dataset_agreement_grid_wise$IQR_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile\nrange") +
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

### Relative dataset agreement ----
to_plot_sf <- dataset_agreement_grid_wise[, .(lon, lat, rel_dataset_agreement)
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
                    labels = levels(dataset_agreement_grid_wise$rel_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(dataset_agreement_grid_wise$rel_dataset_agreement),
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


gg_agreement_v2 <- ggarrange(gg_volume, fig_quantile_range, 
                             fig_distribution_index, fig_rel_dataset_agreement,
                             labels = c('a', 'b', 'c', 'd'), common.legend = FALSE,
                             legend = 'right',
                             nrow = 2, ncol = 2)

jpeg(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig1_main_dataset_range_agreement_maps.png"), 
     width = 20, height = 11, res = 300, units = 'in')
gg_agreement_v2
dev.off()
