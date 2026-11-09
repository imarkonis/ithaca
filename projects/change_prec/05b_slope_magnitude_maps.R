#Slope Map for Various Metrics
source("source/change_prec.R")

## Quick plot
pretty_map <- function(to_plot_data, breaks, colorbar, title) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = NA, color = "gray23", lwd = 0.5) +
    geom_sf(aes(color = slope, fill = slope)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    geom_sf(data = to_plot_data[to_plot_data$slope == 0,], fill = "#f5f5f5",
            color = "#f5f5f5") +
    scale_fill_fermenter(palette = "BrBG", direction = 1,
                         breaks = breaks, na.value = "#f5f5f5",
                         guide = guide_coloursteps(
                           direction = "horizontal",
                           barwidth = unit(8, "cm"),
                           barheight = unit(0.25, "cm"),
                           title.position = "bottom",
                           title.hjust = 0.5
                         )) +
    scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                          breaks = breaks, na.value = "#f5f5f5") +
    labs(x = NULL, y = NULL, fill = colorbar, title = title) +
    coord_sf(expand = FALSE, crs = "+proj=robin") +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 3) +
    geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 3) +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.title.position = "bottom")
  return(p00)
  }


breaks_tot <- c(-408, -3.5, -1, -0.4, 0, 0.4, 1, 3.5, 276)
breaks_r1x <- c(-16, -0.3, -0.13, -0.04, 0, 0.04, 0.13, 0.3, 15)
breaks_epf <- c(-78, -0.071, -0.033, -0.014, 0, 0.014, 0.033, 0.071, 55)
breaks_wds <- c(-15, -0.7, -0.3, -0.1, 0, 0.1, 0.3, 0.7, 9)
## Data
prec_exev <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_r1x_slopes.rds"))
prec_exev <- rbind(prec_exev, dummie)

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_yrs_slopes.rds"))
prec_data <- rbind(prec_data, dummie)

prec_wday <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_wet_days_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                         "new_ensemble_wet_days_slopes.rds"))
prec_wday <- rbind(prec_wday, dummie)

prec_epf <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_epf_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_epf_slopes.rds"))
prec_epf <- rbind(prec_epf, dummie)

DATASETS <- c("cpc-global", "ensemble", "ensemble_new", "era5-land",
              "gpcp-v1-3", "gpm-imerg-v7", "jra-3q", "merra-2")

## Plot
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_CHANGE_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- st_wrap_dateline(ne_coastline(returnclass = "sf"),
                             options = c("WRAPDATELINE=YES",
                                         "DATELINEOFFSET=10"))

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
  to_plot_avg <- prec_data[dataset == DATASETS[idx], .(lon, lat, slope = 10*slope)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  to_plot_r1x <- prec_exev[dataset == DATASETS[idx], .(lon, lat, slope = 10*slope)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  to_plot_epf <- prec_epf[dataset == DATASETS[idx], .(lon, lat, slope = 10*slope)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  to_plot_wday <- prec_wday[dataset == DATASETS[idx], .(lon, lat, slope = 10*slope)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  p01 <- pretty_map(to_plot_avg, 10*breaks_tot, "[mm/decade]", "Total Annual Precipitation")
  
  p02 <- pretty_map(to_plot_r1x, 10*breaks_r1x, "[mm/decade]", "Annual Maxima Daily Precipitation")
  
  p03 <- pretty_map(to_plot_epf, 10*breaks_epf, "[%/decade]", "Extreme Precipitation Fraction")
  
  p04 <- pretty_map(to_plot_wday, 10*breaks_wds, "[days/decade]", "Wet Days")
  
  p00 <- ggarrange(p01, p02, p03, p04, ncol = 2, nrow = 2,
                   labels = c("a", "b", "c", "d"), align = "hv",
                   font.label = list(size = 16), legend = "bottom")
  
  ggsave(plot = p00, paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                            "_slope_magnitude.jpg"), width = 10, height = 7.2)
}
