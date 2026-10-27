#Slope Map for Various Metrics
source("source/change_prec.R")

## Quick plot
pretty_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "#f5f5f5", color = "#f5f5f5") +
    geom_sf(aes(color = prec, fill = prec)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    scale_fill_manual(values = c("1" = "#01665E", "2" = "#5AB4AC",
                                 "3" = "#f5f5f5", "4" = "#D8B365",
                                 "5" = "#8C510A", na.value = "#f5f5f5"),
                      labels = c("1" = "Positive Significant", "2" = "Positive",
                                 "3" = "No Trend", "4" = "Negative",
                                 "5" = "Negative Significant")) +
    scale_color_manual(values = c("1" = "#01665e", "2" = "#5AB4AC",
                                  "3" = "#f5f5f5", "4" = "#D8B365",
                                  "5" = "#8C510A", na.value = "#f5f5f5"),
                       guide = "none") +
    labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 3) +
    geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 3) +
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

## Data
prec_exev <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x_slopes.rds"))

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs_slopes.rds"))

prec_wday <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_wet_days_slopes.rds"))

prec_epf <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_epf_slopes.rds"))

DATASETS <- c("cpc-global", "ensemble", "era5-land", "gpcp-v1-3",
              "gpm-imerg-v7", "jra-3q", "merra-2")

## Plot
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_CHANGE_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")

labs_y <- data.frame(lon = c(-158, -162, -170, -162, -155),
                     lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°",
                       ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -72)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

### Image per dataset
foreach(idx = 1:length(DATASETS)) %do% {
  to_plot_avg <- prec_data[dataset == DATASETS[idx]]
  to_plot_avg[slope > 0 & p_value <= 0.1, prec := "1"
              ][slope > 0 & p_value > 0.1, prec := "2"
                ][slope == 0, prec := "3"
                  ][slope < 0 & p_value > 0.1, prec := "4"
                    ][slope < 0 & p_value <= 0.1, prec := "5"]
  to_plot_avg <- to_plot_avg[, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_avg$prec <- as.character(to_plot_avg$prec)
  
  to_plot_r1x <- prec_exev[dataset == DATASETS[idx]]
  to_plot_r1x[slope > 0 & p_value <= 0.1, prec := "1"
              ][slope > 0 & p_value > 0.1, prec := "2"
                ][slope == 0, prec := "3"
                  ][slope < 0 & p_value > 0.1, prec := "4"
                    ][slope < 0 & p_value <= 0.1, prec := "5"]
  to_plot_r1x <- to_plot_r1x[, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_r1x$prec <- as.character(to_plot_r1x$prec)
  
  to_plot_epf <- prec_epf[dataset == DATASETS[idx]]
  to_plot_epf[slope > 0 & p_value <= 0.1, prec := "1"
              ][slope > 0 & p_value > 0.1, prec := "2"
                ][slope == 0, prec := "3"
                  ][slope < 0 & p_value > 0.1, prec := "4"
                    ][slope < 0 & p_value <= 0.1, prec := "5"]
  to_plot_epf <- to_plot_epf[, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_epf$prec <- as.character(to_plot_epf$prec)
  
  to_plot_wday <- prec_wday[dataset == DATASETS[idx]]
  to_plot_wday[slope > 0 & p_value <= 0.1, prec := "1"
               ][slope > 0 & p_value > 0.1, prec := "2"
                 ][slope == 0, prec := "3"
                   ][slope < 0 & p_value > 0.1, prec := "4"
                     ][slope < 0 & p_value <= 0.1, prec := "5"]
  to_plot_wday <- to_plot_wday[, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_wday$prec <- as.character(to_plot_wday$prec)
  
  p01 <- pretty_map(to_plot_avg)
  
  p02 <- pretty_map(to_plot_r1x)
  
  p03 <- pretty_map(to_plot_epf)
  
  p04 <- pretty_map(to_plot_wday)
  
  p00 <- ggarrange(p01, p02, p03, p04, ncol = 2, nrow = 2,
                   labels = c("a", "b", "c", "d"), common.legend = TRUE,
                   font.label = list(size = 16), legend = "bottom")
  
  ggsave(plot = p00, paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                            "_slopes.jpg"), width = 9, height = 5)
}
