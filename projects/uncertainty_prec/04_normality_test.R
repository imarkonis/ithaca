# Test for normality on each grid cell
source("source/uncertainty_prec.R")

install.packages(setdiff(c("lmom"), rownames(installed.packages())))

library(lmom)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_roi.rds"))

## Analysis
prec_data <- prec_data[, date := year(date)
                       ][, prec := sum(prec, na.rm = TRUE),
                         .(lon, lat, dataset, date)
                         ][, prec := mean(prec, na.rm = TRUE),
                           .(lon, lat, dataset)]

l_moments <- prec_data[, as.list(samlmu(prec)), .(lon, lat)]

saveRDS(l_moments, paste0(PATH_SAVE_UNCERTAINTY_PREC,
                          "prec_data_l_moments.rds"))

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
to_plot_st3 <- l_moments[, .(lon, lat, t_3)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_st4 <- l_moments[, .(lon, lat, t_4)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

### Map
p01 <- ggplot(to_plot_st3) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(fill = t_3), color = NA) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_gradientn(colours = c("#2d004b", "#542788" , "#8073ac", "#b2abd2",
                                            "#d8daeb", "#f7f7f7", "#fee0b6",
                                            "#fdb863", "#e08214", "#b35806",
                                            "#7f3b08"),
                       values = rescale(c(-0.6, -0.3, -0.1, -0.05, 0, 0.05,
                                          0.1, 0.3, 0.6, 0.9))) +
  labs(x = NULL, y = NULL, fill = "L-Skewness") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
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

p02 <- ggplot(to_plot_st4) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(fill = t_4), color = NA) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_gradientn(colours = c("#2d004b", "#542788" , "#8073ac", "#b2abd2",
                                            "#d8daeb", "#f7f7f7", "#fee0b6",
                                            "#fdb863", "#e08214", "#b35806",
                                            "#7f3b08"),
                       values = rescale(c(-0.14, 0, 0.11, 0.1226, 0.13,
                                          0.2, 0.4, 0.6, 0.8))) +
  labs(x = NULL, y = NULL, fill = "L-Kurtosis") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
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

p00 <- ggarrange(p01, p02, labels = c("(a)", "(b)"), nrow = 1)

ggsave(plot = p00, paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                          "l-moments_map.png"), width = 16.5,
       height = 16.5/GOLDEN_RATIO)
