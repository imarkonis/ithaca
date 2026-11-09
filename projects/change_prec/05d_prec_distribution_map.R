#Scatter plot matrix
source("source/change_prec.R")

## Quick plot
pretty_map <- function(to_plot_data, title) {
  p00 <- ggplot(to_plot_data) +
    geom_sf(data = world_sf, fill = NA, color = "gray23", lwd = 0.2) +
    geom_sf(aes(fill = prec, color = prec), linewidth = 0.01) +
    geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 1.5) +
    scale_fill_manual(values = c("1" = "#984ea3", "2" = "#e41a1c",
                                 "3" = "#4daf4a", "4" = "#377eb8"),
                      labels = c("1" = expression("\u2191EPF &" * "\u2193D"[wet]),
                                 "2" = expression("\u2191EPF &" * "\u2191D"[wet]),
                                 "3" = expression("\u2193EPF &" * "\u2193D"[wet]),
                                 "4" = expression("\u2193EPF &" * "\u2191D"[wet])),
                      drop = FALSE) +
    scale_color_manual(values = c("1" = "#984ea3", "2" = "#e41a1c",
                                  "3" = "#4daf4a", "4" = "#377eb8"),
                       guide = "none") +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    labs(x = NULL, y = NULL, fill = NULL, title = title) +
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
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 14))
  return(p00)
}


## Data
prec_exev <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_r1x_slopes.rds"))
prec_exev <- rbind(prec_exev, dummie)

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_yrs_slopes.rds"))
prec_data <- rbind(prec_data, dummie)

prec_epf <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_epf_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_epf_slopes.rds"))
prec_epf <- rbind(prec_epf, dummie)

prec_wday <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_wet_days_slopes.rds"))
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                         "new_ensemble_wet_days_slopes.rds"))
prec_wday <- rbind(prec_wday, dummie)

prec_data <- merge(prec_data[, .(lon, lat, dataset, tot = slope)],
                   prec_exev[, .(lon, lat, dataset, r1x = slope)],
                   by = c("lon", "lat", "dataset"), all = TRUE)
prec_data <- merge(prec_data,
                   prec_wday[, .(lon, lat, dataset, wds = slope)],
                   by = c("lon", "lat", "dataset"), all = TRUE)
prec_data <-merge(prec_data,
                  prec_epf[, .(lon, lat, dataset, epf = slope)],
                  by = c("lon", "lat", "dataset"), all = TRUE)

rm(prec_exev, prec_wday,prec_epf)

prec_data[tot < 0 & r1x < 0, panel := 1
          ][tot < 0 & r1x > 0, panel := 2
            ][tot > 0 & r1x < 0, panel := 3
              ][tot > 0 & r1x > 0, panel := 4]


prec_data[epf < 0 & wds > 0, prec := 4
          ][epf < 0 & wds <= 0, prec := 3
            ][epf > 0 & wds > 0, prec := 2
              ][epf > 0 & wds <= 0, prec := 1]

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

foreach(idx = 1:length(DATASETS)) %do% {
  to_plot_data <- prec_data[dataset == DATASETS[idx]]
  to_plot_panel1 <- to_plot_data[panel == 1, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_panel1$prec <- as.character(to_plot_panel1$prec)
  
  to_plot_panel2 <- to_plot_data[panel == 2, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_panel2$prec <- as.character(to_plot_panel2$prec)
  
  to_plot_panel3 <- to_plot_data[panel == 3, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_panel3$prec <- as.character(to_plot_panel3$prec)
  
  to_plot_panel4 <- to_plot_data[panel == 4, .(lon, lat, prec)] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  to_plot_panel4$prec <- as.character(to_plot_panel4$prec)
  
  p01 <- pretty_map(to_plot_panel1, "\u2193 Annual Total \u2193 Annual Maxima")
  
  p02 <- pretty_map(to_plot_panel2, "\u2193 Annual Total \u2191 Annual Maxima")
  
  p03 <- pretty_map(to_plot_panel3, "\u2191 Annual Total \u2193 Annual Maxima")
  
  p04 <- pretty_map(to_plot_panel4, "\u2191 Annual Total \u2191 Annual Maxima")
  
  ggarrange(p01, p02, p03, p04, ncol = 2, nrow = 2, align = "hv",
            common.legend = TRUE, font.label = list(size = 16),
            legend = "bottom", labels = c("a", "b", "c", "d"))
  
  ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, DATASETS[idx],
                "_prec_redist.jpg"), width = 9, height = 5)
}

