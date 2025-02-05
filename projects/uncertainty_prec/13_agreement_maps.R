# Time Series
source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_map <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_roi.rds"))

PREC_REPS <- c("cmap", "cpc", "cru-ts-v4-07", "em-earth", "era5-land", "fldas",
               "gpcp-v3-2", "jra55", "ncep-doe", "precl")

prec_map <- prec_map[dataset %in% PREC_REPS]

prec_ensemble <- prec_map[, .(median_prec = median(prec, na.rm = TRUE)),
                          .(lon, lat, date)]

prec_map[, YEAR := year(date)]
prec_ensemble[, YEAR := year(date)]

prec_map <- prec_map[, .(value = sum(prec, na.rm = TRUE)),
                     .(lon, lat, YEAR, dataset)]
prec_ensemble <- prec_ensemble[, .(value = sum(median_prec, na.rm = TRUE)),
                               .(lon, lat, YEAR)]

prec_map <- prec_map[, .(prec = mean(value, na.rm = TRUE)),
                     .(lon, lat, dataset)]
prec_ensemble <- prec_ensemble[, .(prec = mean(value, na.rm = TRUE)),
                               .(lon, lat)]

prec_map <- prec_map[, .(q75 = quantile(prec, .75), q25 = quantile(prec, .25),
                         mean_prec = mean(prec, na.rm = TRUE)), .(lon, lat)]

prec_map <- prec_map[, .(prec = (q75 - q25)/mean_prec), .(lon, lat)]

AGREEMENT_QUANTILES <- quantile(prec_map$prec, c(0, 0.1, 0.3, 0.7, 0.9, 1))

prec_map[prec <= AGREEMENT_QUANTILES[2], agreement := 1
         ][prec > AGREEMENT_QUANTILES[2] & prec <= AGREEMENT_QUANTILES[3],
           agreement := 2
           ][prec > AGREEMENT_QUANTILES[3] & prec <= AGREEMENT_QUANTILES[4],
             agreement := 3
             ][prec > AGREEMENT_QUANTILES[4] & prec <= AGREEMENT_QUANTILES[5],
               agreement := 4
               ][prec > AGREEMENT_QUANTILES[5] & prec <= AGREEMENT_QUANTILES[6],
                 agreement := 5]

to_plot_data <- prec_map[, .(lon, lat, agreement)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_data$agreement <- as.character(to_plot_data$agreement)

prec_ensemble <- prec_ensemble[, .(value = 0.0393701*prec), .(lon, lat)]

prec_ensemble[value < 10, prec := 1
              ][value >= 10 & value < 25, prec := 2
                ][value >= 25 & value < 50, prec := 3
                  ][value >= 50 & value < 75, prec := 4
                    ][value >= 75, prec := 5]

to_plot_ensemble <- prec_ensemble[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_ensemble$prec <- as.character(to_plot_ensemble$prec)

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

p01 <- ggplot(to_plot_ensemble) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#bdcd9e", "2" = "#acc8b9",
                               "3" = "#86bcae", "4" = "#6c9c9c",
                               "5" = "#467972"),
                    labels = c("1" = "< 254 mm", "2" = "254-635 mm",
                               "3" = "635-1270 mm",
                               "4" = "1270-1905 mm", "5" = "> 1905 mm")) +
  scale_color_manual(values = c("1" = "#bdcd9e", "2" = "#acc8b9",
                                "3" = "#86bcae", "4" = "#6c9c9c",
                                "5" = "#467972"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Annual\nPrecipitation",
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
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

p02 <- ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = agreement, fill = agreement)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "High", "2" = "Above Average",
                               "3" = "Average",
                               "4" = "Below Average", "5" = "Low")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Ensemble\nConfidence",
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
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

p00 <- ggarrange(p01, p02, ncol = 1, nrow = 2,
                 labels = c("a)", "b)"))

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "figure_03.png"),
       width = 5*GOLDEN_RATIO, height = 5*2)
