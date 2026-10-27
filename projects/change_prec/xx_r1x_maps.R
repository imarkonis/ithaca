#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x_slopes.rds"))
prec_data[p.value < .1, prec := 1
          ][p.value >= .1 & p.value < .25, prec := 2
            ][p.value >= .25 & p.value < .50, prec := 3
              ][p.value >= .50 & p.value < .75, prec := 4
                ][p.value >= .75, prec := 5
                  ][, estimates := 10*estimates]

DATASETS <- c("cpc-global", "ensemble", "era5-land", "gpcp-v1-3",
              "gpm-imerg-v7", "jra-3q", "merra-2", "mswep-v2-8", "ncep-doe")

## Plot
### Plot templates
slope_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = estimates, fill = estimates)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    geom_sf(data = to_plot_data[to_plot_data$estimates == 0,], fill = "#f5f5f5",
            color = "#f5f5f5") +
    scale_fill_fermenter(palette = "BrBG", direction = 1,
                         breaks = c(-42, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 41)) +
    scale_color_fermenter(palette = "BrBG", guide = "none", direction = 1,
                          breaks = c(-42, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 41)) +
    labs(x = NULL, y = NULL, fill = "[%/decade]", title = NULL) +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
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

pvalue_map <- function(to_plot_data) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
    geom_sf(aes(color = prec, fill = prec)) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
    scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                 "3" = "#F4CC70", "4" = "#E38B75",
                                 "5" = "#CE5A57"),
                      labels = c("1" = "< 0.1", "2" = "< 0.25",
                                 "3" = "< 0.5",
                                 "4" = "< 0.75", "5" = ">= 0.75")) +
    scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                  "3" = "#F4CC70", "4" = "#E38B75",
                                  "5" = "#CE5A57"), guide = "none") +
    #geom_sf(data = world_sf, fill = NA, color = "gray23") +
    labs(x = NULL, y = NULL, fill = "\U03B1", title = NULL) +
    coord_sf(expand = FALSE, crs = "+proj=eqearth") +
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
  to_plot_data <- prec_data[dataset == DATASETS[idx], .(lon, lat, estimates)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  slope_map(to_plot_data) 
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                "_r1x_slope.png"),
         width = 8.7, height = 4.2)
  
  to_plot_data <- prec_data[dataset == DATASETS[idx], .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_data$prec <- as.character(to_plot_data$prec)
  
  pvalue_map(to_plot_data) 
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                "_r1x_p_value.png"),
         width = 8.7, height = 4.2)
}
