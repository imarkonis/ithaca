#Scatter plot matrix
source("source/change_prec.R")

library(DescTools)
## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_r1x.rds"))
prec_data <- prec_data[year(date) >= 1995]
prec_data[, month_count := .N, .(lon, lat, dataset, month(date))]
prec_data <- prec_data[, .SD[which.max(month_count)], .(lon, lat, dataset)]
prec_data[, month_mode := month(date)]

DATASETS <- c("cpc-global", "ensemble", "era5-land", "gpcp-v1-3",
              "gpm-imerg-v7", "jra-3q", "merra-2", "mswep-v2-8", "ncep-doe")


## Quick plot
MAP_COLORS <- c("Dec" = "#1d91c0", "Jan" = "#225ea8", "Feb" = "#253494",
                "Mar" = "#78c679", "Apr" = "#238443", "May" = "#004529",
                "Jun" = "#fc4e2a", "Jul" = "#e31a1c", "Aug" = "#bd0026",
                "Sep" = "#cc4c02", "Oct" = "#993404", "Nov" = "#662506")


pretty_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = month_mode, fill = month_mode)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    scale_fill_manual(values = MAP_COLORS, drop = FALSE) +
    scale_color_manual(values = MAP_COLORS, guide = "none") +
    labs(x = NULL, y = NULL, fill = "[Month]", title = NULL) +
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
  return(p00)
}

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



foreach(idx = 1:length(DATASETS)) %do% {
  to_plot_data <- prec_data[dataset == DATASETS[idx], .(lon, lat, month_mode)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_data$month_mode <- lubridate::month(to_plot_data$month_mode,
                                              label = TRUE)
  pretty_map(to_plot_data) 
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                "_r1x_modes.png"),
         width = 8.7, height = 8.7/GOLDEN_RATIO)
}
