#Scatter plot matrix
source("source/change_prec.R")

## Quick plot

pretty_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = slope, fill = slope)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    geom_sf(data = to_plot_data[to_plot_data$slope == 0,], fill = "#f5f5f5",
            color = "#f5f5f5") +
    scale_fill_fermenter(palette = "RdBu", direction = 1,
                         breaks = c(-180, -21, -14, -7, 0, 7, 14, 21, 180)) +
    scale_color_fermenter(palette = "RdBu", guide = "none", direction = 1,
                          breaks = c(-180, -21, -14, -7, 0, 7, 14, 21, 180)) +
    labs(x = NULL, y = NULL, fill = "[day/decade]", title = NULL) +
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

## Data
prec_r1x <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_r1x_date_slopes.rds"))

prec_r5x <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_r5x_date_slopes.rds"))

DATASETS <- c("cpc-global", "ensemble", "era5-land", "gpcp-v1-3",
              "gpm-imerg-v7", "jra-3q", "merra-2", "mswep-v2-8", "ncep-doe")

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
  to_plot_r1x <- prec_r1x[dataset == DATASETS[idx], .(lon, lat, slope)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  pretty_map(to_plot_r1x) 
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                "_prec_r1x_date_shift.png"),
         width = 8.7, height = 4.2)
  
  to_plot_r5x <- prec_r5x[dataset == DATASETS[idx], .(lon, lat, slope)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  pretty_map(to_plot_r5x) 
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                "_prec_r5x_date_shift.png"),
         width = 8.7, height = 4.2)
}
