#Scatter plot matrix
source("source/change_prec.R")

## Quick plot

pretty_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = trend, fill = trend)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    geom_sf(data = to_plot_data[to_plot_data$trend == 0,], fill = "#f5f5f5",
            color = "#f5f5f5") +
    scale_fill_fermenter(palette = "BrBG", direction = 1,
                         breaks = c(-27, -2, -1, -0.5, 0, 0.5, 1, 2, 30)) +
    scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                          breaks = c(-27, -2, -1, -0.5, 0, 0.5, 1, 2, 30)) +
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
  return(p00)
}

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_monthly_slopes.rds"))

prec_data[, trend := 100*slope_1995_2024/mean_1995_2024,
          .(lon, lat, dataset, month)]

prec_data <- prec_data[trend != Inf & trend != -Inf & !is.na(trend)]

prec_data[, map_id := .GRP, by = c("month", "dataset")]

MAX_ID <- max(prec_data$map_id)

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



foreach(idx = 1:MAX_ID) %do% {
  to_plot_data <- prec_data[map_id == idx, .(lon, lat, trend)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  dummie_dataset <- unique(prec_data[map_id == idx, dataset])
  dummie_month <- unique(prec_data[map_id == idx, month])
  dummie_month <- tolower(month.abb[dummie_month])
  
  pretty_map(to_plot_data) 
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, dummie_dataset, "_", 
                dummie_month, "_1995_2024.png"),
         width = 8.7, height = 8.7/GOLDEN_RATIO)
}
