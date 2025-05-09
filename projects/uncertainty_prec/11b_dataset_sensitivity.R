# Sensitivity to referential data
source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

registerDoParallel(cores = N_CORES)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric.rds"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                "t_metric_bootstrap.rds"))

## Analysis
prec_data <- prec_data[order(prec_data$t_prec, decreasing = TRUE)
                       ][, head(.SD, 3), .(lon, lat)
                         ][, coord_id := .GRP, by = c("lon", "lat")]

bootstrap_data <- bootstrap_data[order(bootstrap_data$t_prec, decreasing = TRUE)
                                 ][, head(.SD, 3), .(lon, lat, combination_idx)]

bootstrap_data <- bootstrap_data[unique(prec_data[, .(lon, lat, coord_id)]),
                                 on = .(lon,lat)]

COORD_IDX <- max(prec_data$coord_id)

prec_bootstrap <- foreach (idx = 1:COORD_IDX, .combine = rbind) %dopar% {
  dummie_all <- prec_data[coord_id == idx, dataset]
  dummie_test <- bootstrap_data[coord_id == idx]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                   .(lon, lat, combination_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/330)), .(lon, lat)]
}

## Plot
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
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

###
prec_bootstrap[pct < .1, prec := 1
               ][pct >= .1 & pct < .25, prec := 2
                 ][pct >= .25 & pct < .50, prec := 3
                   ][pct >= .50 & pct < .75, prec := 4
                     ][pct >= .75, prec := 5]

to_plot_sensitivity <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sensitivity$prec <- as.character(to_plot_sensitivity$prec)

p01 <- ggplot(to_plot_sensitivity) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence",
                               "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence",
                               "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset\nRepresentative\nConfidence",
       title = NULL) +
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

ggsave(plot = p01,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "dataset_sensitivity.png"),
       width = 4.5*GOLDEN_RATIO, height = 4.5)
