#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_trends.rds"))

## Plot
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_CHANGE_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")

labs_y <- data.frame(lon = -162, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°",
                       ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -80)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

to_plot_ensemble <- prec_data[dataset == "ensemble", .(lon, lat, slope)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()


ggplot(to_plot_ensemble) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = slope, fill = slope)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_color_gradientn(guide = "none",
                        colors = c("blue", "white", "red"),
                        values = rescale(c(-2, -0.01, 0, 0.01, 3))) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = rescale(c(-2, -0.01, 0, 0.01, 3))) +
  labs(x = NULL, y = NULL, fill = "[mm/month/month]",
       title = NULL) +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "prec_change.png"),
       width = 8.7, height = 8.7/GOLDEN_RATIO)

to_plot_em_earth <- prec_data[dataset == "em-earth", .(lon, lat, slope)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()


p00 ggplot(to_plot_em_earth) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = slope, fill = slope)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_color_gradientn(guide = "none",
                        colors = c("blue", "white", "red"),
                        values = rescale(c(-2, -0.01, 0, 0.01, 3))) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = rescale(c(-2, -0.01, 0, 0.01, 3))) +
  labs(x = NULL, y = NULL, fill = "[mm/month/month]",
       title = NULL) +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "prec_change.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)