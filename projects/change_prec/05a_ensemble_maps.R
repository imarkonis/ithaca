source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs.rds"))
prec_data <- prec_data[year <= 2024]

prec_ensemble <- prec_data[dataset != "ensemble"]
prec_ensemble <- prec_ensemble[, .(prec = median(prec, na.rm = TRUE)),
                               .(lon, lat, year)]
prec_ensemble[, prec := round(prec, 1)]
prec_ensemble <- prec_ensemble[, .(prec = median(prec, na.rm = TRUE)),
                               .(lon, lat)]
prec_ensemble[prec <= 75, mm := 0
              ][prec > 75 & prec <= 150, mm := 1
                ][prec > 150 & prec <= 250, mm := 2
                  ][prec > 250 & prec <= 375, mm := 3
                    ][prec > 375 & prec <= 525, mm := 4
                      ][prec > 525 & prec <= 700, mm := 5
                        ][prec > 700 & prec <= 900, mm := 6
                          ][prec > 900 & prec <= 1400, mm := 7
                            ][prec > 1400 & prec <= 2000, mm := 8
                              ][prec > 2000, mm := 9]

prec_data <- prec_data[dataset != "ensemble",
                       .(prec = mean(prec, na.rm = TRUE)), .(lon, lat, dataset)]
prec_data <- prec_data[, .(prec = mad(prec, na.rm = TRUE)/median(prec, na.rm = TRUE)), .(lon, lat)]

AGREEMENT_QUANTILES <- quantile(prec_data$prec, c(0, 0.1, 0.3, 0.7, 0.9, 1))

prec_data[prec <= 0.075, agreement := 1
          ][prec > 0.075 & prec <= 0.15, agreement := 2
            ][prec > 0.15 & prec <= 0.35, agreement := 3
              ][prec > 0.35 & prec <= 0.65, agreement := 4
                ][prec > 0.65, agreement := 5]

to_plot_data <- prec_data[, .(lon, lat, agreement)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_data$agreement <- as.character(to_plot_data$agreement)

to_plot_ensemble <- prec_ensemble[, .(lon, lat, mm)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_ensemble$mm <- as.character(to_plot_ensemble$mm)

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

## Figures
p01 <- ggplot(to_plot_ensemble) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = mm, fill = mm)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("0" = "#d9e5c1", "1" = "#c8ddbf",
                               "2" = "#b5d5bd", "3" = "#a0ccb9",
                               "4" = "#8ac2b2", "5" = "#74b5ab",
                               "6" = "#5fa5a1", "7" = "#4f918f",
                               "8" = "#417d7d", "9" = "#35686d"),
                    labels = c("0" = "< 75 mm", "1" = "75-150 mm",
                               "2" = "150-250 mm", "3" = "250-375 mm",
                               "4" = "375-525 mm", "5" = "525-700 mm",
                               "6" = "700-900 mm", "7" = "900-1400 mm",
                               "8" = "1400-2000", "9" = "> 2000 mm")) +
  scale_color_manual(values = c("0" = "#d9e5c1", "1" = "#c8ddbf",
                                "2" = "#b5d5bd", "3" = "#a0ccb9",
                                "4" = "#8ac2b2", "5" = "#74b5ab",
                                "6" = "#5fa5a1", "7" = "#4f918f",
                                "8" = "#417d7d", "9" = "#35686d"),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Annual\nPrecipitation",
       title = NULL) +
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
  labs(x = NULL, y = NULL, fill = "Ensemble\nAgreement",
       title = NULL) +
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

ggarrange(p01, p02, ncol = 1, labels = c("a", "b"),
          font.label = list(size = 16))

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "ensemble_yearly_mean.jpg"),
       width = 9, height = 7.2)
