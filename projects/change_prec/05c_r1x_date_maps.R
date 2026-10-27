#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_r1x.rds"))
prec_data <- prec_data[year(date) <= 2024]
prec_data[, month_count := .N, .(lon, lat, dataset, month(date))]
prec_data <- prec_data[, .SD[month_count == max(month_count)], .(lon, lat, dataset)]
prec_data <- prec_data[, .(month_mode = median(lubridate::yday(date))),
                       .(lon, lat, dataset)]
prec_data[, month_mode := month(as.Date("2010-01-01") + month_mode - 1)]

prec_r1x <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                           "prec_data_r1x_timing_slopes.rds"))

ipcc_sf <- read_sf(paste0(PATH_SAVE_CHANGE_PREC_SPATIAL,
                       "IPCC-WGI-reference-regions-v4.shp"))


DATASETS <- c("cpc-global", "ensemble", "era5-land", "gpcp-v1-3",
              "gpm-imerg-v7", "jra-3q", "merra-2")


## Quick plot
MAP_COLORS <- c("Dec" = "#D9E8F5", "Jan" = "#A8C7FF", "Feb" = "#4C78A8",
                "Mar" = "#7BC96F", "Apr" = "#BFEA8A", "May" = "#FFE08A",
                "Jun" = "#FFB36B", "Jul" = "#FF7F50", "Aug" = "#E85D3F",
                "Sep" = "#B86B2B", "Oct" = "#8C4A2F", "Nov" = "#6B3E2E")


pretty_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = month_mode, fill = month_mode)) +
    geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
    geom_sf_text(data = ipcc_sf, aes(label = Acronym), size = 3,
                 fontface = "bold") +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    scale_fill_manual(values = MAP_COLORS, drop = FALSE) +
    scale_color_manual(values = MAP_COLORS, guide = "none") +
    labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid = element_blank(),
          axis.text = element_blank(), 
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  return(p00)
}

dates_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "#f5f5f5", color = "#f5f5f5") +
    geom_sf(aes(color = prec, fill = prec)) +
    geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
    geom_sf_text(data = ipcc_sf, aes(label = Acronym), size = 3,
                 fontface = "bold") +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    scale_fill_manual(values = c("1" = "#ca0020", "2" = "#f4a582",
                                 "3" = "#f5f5f5", "4" = "#92c5de",
                                 "5" = "#0571b0", na.value = "#f5f5f5"),
                      labels = c("1" = "Earlier Significant", "2" = "Earlier",
                                 "3" = "No Trend", "4" = "Later",
                                 "5" = "Later Significant")) +
    scale_color_manual(values = c("1" = "#ca0020", "2" = "#f4a582",
                                  "3" = "#f5f5f5", "4" = "#92c5de",
                                  "5" = "#0571b0", na.value = "#f5f5f5"),
                       guide = "none") +
    labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid = element_blank(),
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

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -70)
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
  to_plot_data$month_mode <- factor(to_plot_data$month_mode,
                                    levels = c("Dec", "Jan", "Feb",
                                               "Mar", "Apr", "May",
                                               "Jun", "Jul", "Aug",
                                               "Sep", "Oct", "Nov"))
  p01 <- pretty_map(to_plot_data)
  
  to_plot_r1x <- prec_r1x[dataset == DATASETS[idx]]
  to_plot_r1x[slope < 0 & p_value <= 0.1, prec := "1"
              ][slope < 0 & p_value > 0.1, prec := "2"
                ][slope == 0, prec := "3"
                  ][slope > 0 & p_value > 0.1, prec := "4"
                    ][slope > 0 & p_value <= 0.1, prec := "5"]
  to_plot_r1x <- to_plot_r1x[, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_r1x$prec <- as.character(to_plot_r1x$prec)
  
  p02 <- dates_map(to_plot_r1x)
  
  ggarrange(p01, p02, ncol = 1, labels = c("a", "b"), align = "hv",
            font.label = list(size = 16))
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx], "_r1x_dates.jpg"),
         width = 9, height = 7.2)
}
