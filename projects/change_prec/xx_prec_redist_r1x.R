#Scatter plot matrix
source("source/change_prec.R")

## Quick plot

pretty_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = direction, fill = direction)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    scale_fill_manual(values = c("2" = "#08306B", "1" = "#D94801",
                                 "0" = "#ffffbf", "-1" = "#4292C6",
                                 "-2" = "#F4A261"),
                      labels = c("2" = "+Tot & +Exs", "1" = "-Tot & +Exs",
                                 "0" = "No changes", "-1" = "+Tot & -Exs",
                                 "-2" = "-Tot & -Exs"), drop = FALSE) +
    scale_color_manual(values = c("2" = "#08306B", "1" = "#D94801",
                                  "0" = "#ffffbf", "-1" = "#4292C6",
                                  "-2" = "#F4A261"), guide = "none") +
    labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
    geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
    guides(fill = guide_legend(nrow = 2)) +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
          axis.text = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  return(p00)
}

avg_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = slope, fill = slope)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    geom_sf(data = to_plot_data[to_plot_data$slope == 0,], fill = "#f5f5f5",
            color = "#f5f5f5") +
    scale_fill_fermenter(palette = "BrBG", direction = 1,
                         breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 11)) +
    scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                          breaks = c(-14, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 11)) +
    labs(x = NULL, y = NULL, fill = "[%/year]", title = NULL) +
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

ext_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = estimates, fill = estimates)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    geom_sf(data = to_plot_data[to_plot_data$estimates == 0,], fill = "#f5f5f5",
            color = "#f5f5f5") +
    scale_fill_fermenter(palette = "BrBG", direction = 1,
                         breaks = c(-42, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 41)) +
    scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                          breaks = c(-42, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 41)) +
    labs(x = NULL, y = NULL, fill = "[%/decade]", title = NULL) +
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
prec_exts <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_r1x_slopes.rds"))

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_yrs_slopes.rds"))

prec_data <- merge(prec_data[, .(lon, lat, dataset, slope)],
                   prec_exts[, .(lon, lat, dataset, estimates = estimates*10)],
                   by = c("lon", "lat", "dataset"))

#prec_data <- add_bivariate_colour(prec_data, "slope", "estimates")
prec_data[slope > 0 & estimates > 0, direction := 2
          ][slope < 0 & estimates > 0, direction := 1
            ][slope == 0 & estimates == 0, direction := 0
              ][slope > 0 & estimates < 0, direction := -1
                ][slope < 0 & estimates < 0, direction := -2]

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

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -75)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")



foreach(idx = 1:length(DATASETS)) %do% {
  to_plot_dist <- prec_data[dataset == DATASETS[idx], .(lon, lat, direction)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_dist$direction <- factor(to_plot_dist$direction,
                                   levels = c(2, -1, 0, -2, 1))
  
  p01 <- pretty_map(to_plot_dist)
  
  to_plot_avg <- prec_data[dataset == DATASETS[idx], .(lon, lat, slope)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  p02 <- avg_map(to_plot_avg)
  
  to_plot_ext <- prec_data[dataset == DATASETS[idx], .(lon, lat, estimates)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  p03 <- ext_map(to_plot_ext)
  
  p04 <- ggarrange(p02, p03, ncol = 1, align = "v", labels = c("b", "c"))
  
  p00 <- ggarrange(p01, p04, ncol = 2, widths = c(1.3, 1), labels = "a")
  
  ggsave(plot = p00, paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                            "_yearly_vs_r1x.jpg"), width = 12, height = 4.2)
}
