# Plot global maps of main statistics
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)

## Data
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
prec_grid <- read_stars(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                               "prec_station_grid.nc")) %>% st_as_sf()
colnames(prec_grid)[1] <- "value"
st_crs(prec_grid) <- "+proj=longlat +datum=WGS84 +no_defs"

## Needed for plot
### World and Land borders
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

### Labels
labs_y <- data.frame(lon = -170, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(120, -120, -60)
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
### Median

to_plot_sf <- prec_stats[, .(lon, lat, value = 12 * ens_mean_median)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_ens_mean_median <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = value)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_color_gradient2(low = colset_RdBu_5[1], 
                       mid = colset_RdBu_5[3], 
                       high = colset_RdBu_5[5],
                       limits = c(0, 1500),
                       labels = c("0", "500", "1000", ">1500"),
                       oob = scales::squish) +
    labs(x = NULL, y = NULL, col = "Ensemble\nMedian") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color="gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color="gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour="gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "SI_ensemble_median_map.png"), width = 12, height = 8)

### 0.25-0.75 Quantile range 
to_plot_sf <- prec_stats[, .(lon, lat, value = 12 * (ens_mean_q75 - ens_mean_q25))]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_std_quant_range <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = value)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_color_gradient2(low = colset_RdBu_5[1], 
                       mid = colset_RdBu_5[3], 
                       high = colset_RdBu_5[5],
                       limits = c(0,  500),
                       labels = c("0", "100", "200", "300", "400", ">500"),
                       oob = scales::squish) +
    labs(x = NULL, y = NULL, col = "0.25 - 0.75\nQuantile range") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color="gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color="gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour="gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "SI_quant_range_map.png"), width = 12, height = 8)
