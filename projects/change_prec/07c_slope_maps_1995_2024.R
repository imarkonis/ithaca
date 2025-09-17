#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_trends_1995_2024.rds"))

prec_avgs <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_annual_avg.rds"))

prec_data <- merge(prec_data, prec_avgs, by = c("lon", "lat", "dataset"))

prec_data[, trend := 100*12*slope/prec, .(lon, lat, dataset)]

prec_data <- prec_data[trend != Inf & trend != -Inf & !is.na(trend)]

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

to_plot_data <- prec_data[dataset == "ensemble", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "ensemble_1995_2024.png"),
       width = 8.7, height = 8.7/GOLDEN_RATIO)

to_plot_data <- prec_data[dataset == "cpc-global", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "cpc-global_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)

to_plot_data <- prec_data[dataset == "era5-land", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_colour_fermenter(palette = "BrBG", guide = "none", direction = 1,
                         breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "era5-land_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)

to_plot_data <- prec_data[dataset == "gpcp-v1-3", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "gpcp-v1-3_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)

to_plot_data <- prec_data[dataset == "jra-3q", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "jra-3q_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)

to_plot_data <- prec_data[dataset == "merra-2", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "merra-2_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)

to_plot_data <- prec_data[dataset == "mswep-v2-8", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "mswep-v2-8_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)

to_plot_data <- prec_data[dataset == "ncep-doe", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "ncep-doe_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)

to_plot_data <- prec_data[dataset == "gpm-imerg-v7", .(lon, lat, trend)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = trend, fill = trend)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
          color = "#f5f5f5") +
  scale_fill_fermenter(palette = "BrBG", direction = 1,
                       breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                        breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 8)) +
  labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "gpm-imerg-v7_1995_2024.png"),
       width = 8.7*GOLDEN_RATIO, height = 8.7)
