# SI support for figure 1 - the global overview ----
source('source/changing_prec.R')
library(rnaturalearth)

# Map preparation -----
## World and Land borders ----
PATH_SAVE_PARTITION_PREC <- paste0(PATH_SAVE, "partition_prec/")
load(paste0(PATH_SAVE_PARTITION_PREC, "paths.Rdata"))

earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels ----
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

## Slope ratio and IQR ----
prec_trend_min_max[, slope_ratio := max/min]
prec_trend_min_max[, IQR := Q75-Q25]

prec_trend_min_max[, slope_ratio_brk := cut(slope_ratio, breaks = c(-Inf,-20, -10, -2, 2, 10, 20, Inf))]
prec_trend_min_max[, IQR_brk := cut(IQR, breaks = c(0, 1, 2, 3, 4, 5, 21))]

## IQR ----
cols_IQR <- c("lightblue", "steelblue1", "steelblue3","darkblue", "darkorange" ,"gold")

to_plot_sf <- prec_trend_min_max[, .(lon, lat, IQR_brk)
][, value := as.numeric(IQR_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_IQR <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_IQR, labels = levels(prec_trend_min_max$IQR_brk)) +
  scale_color_manual(values = cols_IQR,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "IQR\n[mm/year/year]") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


#### Slope ratio ----

cols_slope_ratio <- c("darkblue", 
                                "steelblue3",
                                "steelblue1", 
                                "gray90",
                                "gold",
                                "darkorange",
                                "darkred"
)

to_plot_sf <- prec_trend_min_max[, .(lon, lat, slope_ratio_brk)
][, value := as.numeric(slope_ratio_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_slope_ratio <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_slope_ratio, labels = levels(prec_trend_min_max$slope_ratio_brk)) +
  scale_color_manual(values = cols_slope_ratio,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Slope max/min [-]") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))



