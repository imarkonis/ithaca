#Scatter plot matrix
source("source/change_prec.R")

library(dplyr)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))
prec_data <- prec_data[year(date) <= 2024]
prec_data[, doy := lubridate::yday(date)]

dummie <- prec_data[dataset != "ensemble"]
dummie <- dummie[, .(prec = median(prec, na.rm = TRUE),
                     doy = median(doy, na.rm = TRUE)),
                 .(lon, lat, year(date))]
dummie$dataset <- "ensemble_new"

prec_data <- rbind(prec_data[, .(lon, lat, year = year(date), doy, prec, dataset)],
                   dummie[, .(lon, lat, year, doy, prec, dataset)])
prec_data[, month := month(as.Date(paste0(year,"-01-01")) + doy - 1)]
prec_data[, month_count := .N, .(lon, lat, dataset, month)]
prec_data <- prec_data[, .SD[month_count == max(month_count)], .(lon, lat, dataset)]
prec_data <- prec_data[, .(month_mode = median(doy)), .(lon, lat, dataset)]
prec_data[, month_mode := month(as.Date("2010-01-01") + month_mode - 1)]

prec_r1x <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                           "prec_data_r1x_timing_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                         "new_ensemble_r1x_timing_slopes.rds"))

prec_r1x <- rbind(prec_r1x, dummie)

ipcc_sf <- read_sf(paste0(PATH_SAVE_CHANGE_PREC_SPATIAL,
                       "IPCC-WGI-reference-regions-v4.shp"))

DATASETS <- c("cpc-global", "ensemble", "ensemble_new", "era5-land",
              "gpcp-v1-3", "gpm-imerg-v7", "jra-3q", "merra-2")

## Quick plot
MAP_COLORS <- c("Dec" = "#D9E8F5", "Jan" = "#A8C7FF", "Feb" = "#4C78A8",
                "Mar" = "#7BC96F", "Apr" = "#BFEA8A", "May" = "#FFE08A",
                "Jun" = "#FFB36B", "Jul" = "#FF7F50", "Aug" = "#E85D3F",
                "Sep" = "#B86B2B", "Oct" = "#8C4A2F", "Nov" = "#6B3E2E")

pretty_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = NA, color = "gray23", lwd = 0.5) +
    geom_sf(aes(color = month_mode, fill = month_mode)) +
    geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    scale_fill_manual(values = MAP_COLORS, drop = FALSE) +
    scale_color_manual(values = MAP_COLORS, guide = "none") +
    labs(x = NULL, y = NULL, fill = NULL, title = "Annual Maxima Month Mode") +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid = element_blank(),
          axis.text = element_blank(), 
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  return(p00)
}

sign_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = NA, color = "gray23", lwd = 0.5) +
    geom_sf(aes(color = prec, fill = prec)) +
    geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    scale_fill_manual(values = c("1" = "#ca0020", "2" = "#f4a582",
                                 "3" = "#f5f5f5", "4" = "#92c5de",
                                 "5" = "#0571b0", na.value = "#f5f5f5"),
                      labels = c("1" = "Earlier Significant", "2" = "Earlier",
                                 "3" = "No Trend", "4" = "Later",
                                 "5" = "Later Significant"),
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_color_manual(values = c("1" = "#ca0020", "2" = "#f4a582",
                                  "3" = "#f5f5f5", "4" = "#92c5de",
                                  "5" = "#0571b0", na.value = "#f5f5f5"),
                       guide = "none") +
    labs(x = NULL, y = NULL, fill = NULL, title = "Annual Maxima Seasonality Trend Significance") +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid = element_blank(),
          axis.text = element_blank(), 
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16),
          legend.position = "bottom",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  return(p00)
}

date_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = NA, color = "gray23", lwd = 0.5) +
    geom_sf(aes(color = slope, fill = slope)) +
    geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    scale_fill_fermenter(palette = "RdBu", direction = 1,
                         breaks = 10*c(-113, -2.1, -1.4, -0.7, 0, 0.7, 1.4, 2.1, 90),
                         na.value = "#f5f5f5",
                         guide = guide_coloursteps(
                           direction = "horizontal",
                           barwidth = unit(10, "cm"),
                           barheight = unit(0.6, "cm"),
                           title.position = "bottom",
                           title.hjust = 0.5
                         )) +
    scale_color_fermenter(palette = "RdBu", guide = "none", direction = 1,
                          breaks = 10*c(-113, -2.1, -1.4, -0.7, 0, 0.7, 1.4, 2.1, 90),
                          na.value = "#f5f5f5") +
    labs(x = NULL, y = NULL, fill = "[days/decade]", title = "Annual Maxima Seasonality Trend Magnitude") +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid = element_blank(),
          axis.text = element_blank(), 
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16),
          legend.position = "bottom",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  return(p00)
}

## Plot
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_CHANGE_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- st_wrap_dateline(ne_coastline(returnclass = "sf"),
                             options = c("WRAPDATELINE=YES",
                                         "DATELINEOFFSET=10"))

p01 <- ggplot(ipcc_sf) +
  geom_sf(data = world_sf, fill = NA, color = "gray23", lwd = 0.2) +
  geom_sf(fill = NA, color = "gray23") +
  geom_sf_text(data = ipcc_sf, aes(label = Acronym), size = 2.5,
               fontface = "bold") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
  labs(x = NULL, y = NULL, fill = NULL, title = "IPCC AR6 Reference Regions") +
  coord_sf(expand = FALSE, crs = "+proj=eqearth") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_blank(), 
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(0, "cm"),
        legend.spacing.x = unit(0, "cm"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

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
  p02 <- pretty_map(to_plot_data)
  
  to_plot_slope <- prec_r1x[dataset == DATASETS[idx],
                            .(lon, lat, slope = slope*10)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  p03 <- date_map(to_plot_slope)
  
  to_plot_sign <- prec_r1x[dataset == DATASETS[idx]]
  to_plot_sign[slope < 0 & p_value <= 0.1, prec := "1"
              ][slope < 0 & p_value > 0.1, prec := "2"
                ][slope == 0, prec := "3"
                  ][slope > 0 & p_value > 0.1, prec := "4"
                    ][slope > 0 & p_value <= 0.1, prec := "5"]
  to_plot_sign <- to_plot_sign[, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_sign$prec <- as.character(to_plot_sign$prec)
  
  p04 <- sign_map(to_plot_sign)
  
  p00 <- ggarrange(p01, p02, p03, p04, ncol = 2, nrow = 2,
                   labels = c("a", "b", "c", "d"), align = "hv",
                   font.label = list(size = 16), legend = "bottom")
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx], "_r1x_dates.jpg"),
         width = 12, height = 8)
}
