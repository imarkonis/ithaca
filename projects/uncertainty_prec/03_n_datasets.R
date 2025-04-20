#Determine number of data sets on each grid cell (large memory requirements)
source("source/uncertainty_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data.rds"))

## Analysis
lonlat <- prec_data[, .(prec = mean(value, na.rm = TRUE)), .(lon, lat, dataset)]

lonlat <- lonlat[, .(n_datasets = .N), .(lon, lat)]

prec_data <- prec_data[lonlat[n_datasets == MIN_N_DATASETS, .(lon, lat)],
                       on = .(lon, lat)]
setnames(prec_data, "value", "prec")

## Save data
saveRDS(prec_data, paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_roi.rds"))

## Figure
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
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
### Map data
to_plot_sf <- lonlat %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

col_vals <- rev(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                           "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6",
                           "#6a3d9a"))
                           
### Map
p00 <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(n_datasets), fill = as.factor(n_datasets))) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  labs(x = NULL, y = NULL, fill = "Number of\nData Sets") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  scale_fill_manual(values = col_vals) +
  scale_color_manual(values = col_vals, guide = "none") +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

### Save
ggsave(plot = p00, paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                          "n_datasets.png"), width = 16.5, units = "cm",
       height = 16.5/GOLDEN_RATIO)
