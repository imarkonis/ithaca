# Plot global map of dataset agreement classses 
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)
library(dplyr)

# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
levels(prec_mask$land_cover_short_class) <- c("Barren", "Croplands", "Forests", "Grasslands", "Other", "Savannas", 
                                            "Shrublands", "Snow/Ice", "Water" )

#World and Land borders
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

#Labels
labs_y <- data.frame(lon = -170, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°", ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

# Figures
#land use
to_plot_sf <- prec_mask[, .(lon, lat, land_cover_short_class)
][, value := as.numeric(land_cover_short_class)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(land_cover_short_class = 
                                      case_when(to_plot_sf$value == 1 ~ "Barren", 
                                                to_plot_sf$value == 2 ~ "Croplands", 
                                                to_plot_sf$value == 3 ~ "Forests", 
                                                to_plot_sf$value == 4 ~ "Grasslands", 
                                                to_plot_sf$value == 5 ~ "Other", 
                                                to_plot_sf$value == 6 ~ "Savannas", 
                                                to_plot_sf$value == 7 ~ "Shrublands", 
                                                to_plot_sf$value == 8 ~ "Snow/Ice", 
                                                to_plot_sf$value == 9 ~ "Water"
                                                ))

to_plot_sf$land_cover_short_class <- factor(to_plot_sf$land_cover_short_class, 
                                           levels = c("Barren", "Croplands", "Forests", 
                                                      "Grasslands", "Other", "Savannas", 
                                                      "Shrublands", "Snow/Ice", "Water"), 
                                           ordered = TRUE)

fig_land_cover_short_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = land_cover_short_class, fill = land_cover_short_class)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_fill_manual(values = colset_land_cover_short) + 
  #labels = levels(to_plot_sf$rel_dataset_agreement)) +
  scale_color_manual(values = colset_land_cover_short,
                     #labels = levels(to_plot_sf$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Land use") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "supplement/land_cover_map.png"), width = 12, height = 8)


#biome
levels(prec_mask$biome_short_class) <- c("B. Forests", "Deserts", "Flooded", 
                                         "M. Grasslands", "Mediterranean", 
                                         "T. Forests", "T. Grasslands", 
                                         "T/S Forests", "T/S Grasslands", "Tundra")


to_plot_sf <- prec_mask[, .(lon, lat, biome_short_class)
][, value := as.numeric(biome_short_class)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(biome_short_class = 
                                      case_when(to_plot_sf$value == 1 ~ "B. Forests", 
                                                to_plot_sf$value == 2 ~ "Deserts", 
                                                to_plot_sf$value == 3 ~ "Flooded", 
                                                to_plot_sf$value == 4 ~ "M. Grasslands", 
                                                to_plot_sf$value == 5 ~ "Mediterranean", 
                                                to_plot_sf$value == 6 ~ "T. Forests", 
                                                to_plot_sf$value == 7 ~ "T. Grasslands", 
                                                to_plot_sf$value == 8 ~ "T/S Forests", 
                                                to_plot_sf$value == 9 ~ "T/S Grasslands", 
                                                to_plot_sf$value == 10 ~ "Tundra"
                                                ))

to_plot_sf$biome_short_class <- factor(to_plot_sf$biome_short_class, 
                                           levels = c("B. Forests", "Deserts", "Flooded", 
                                                      "M. Grasslands", "Mediterranean", 
                                                      "T. Forests", "T. Grasslands", 
                                                      "T/S Forests", "T/S Grasslands", "Tundra"), ordered = TRUE)
fig_biome_short_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = biome_short_class, fill = biome_short_class)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_fill_manual(values = colset_biome_short[c(2, 8, 10, 9, 6, 1, 7, 5, 4, 3)]) + 
  #labels = levels(to_plot_sf$rel_dataset_agreement)) +
  scale_color_manual(values = colset_biome_short[c(2, 8, 10, 9, 6, 1, 7, 5, 4, 3)],
                     #labels = levels(to_plot_sf$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Biome") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "supplement/biome_map.png"), width = 12, height = 8)

#elevation
levels(prec_mask$elev_class) <- c("0-100", "100-400", "400-800", "800-1500", "1500-3000", "3000+")


to_plot_sf <- prec_mask[, .(lon, lat, elev_class)
][, value := as.numeric(elev_class)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(elev_class = 
                                      case_when(to_plot_sf$value == 1 ~ "0-100", 
                                                to_plot_sf$value == 2 ~ "100-400", 
                                                to_plot_sf$value == 3 ~ "400-800", 
                                                to_plot_sf$value == 4 ~ "800-1500", 
                                                to_plot_sf$value == 5 ~ "1500-3000", 
                                                to_plot_sf$value == 6 ~ "3000+"))

to_plot_sf$elev_class <- factor(to_plot_sf$elev_class, 
                                       levels = c("0-100", "100-400", "400-800", 
                                                  "800-1500", "1500-3000", "3000+"), ordered = TRUE)
fig_elev_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = elev_class, fill = elev_class)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_fill_manual(values = rev(c(colset_elev_mono))) + 
  #labels = levels(to_plot_sf$rel_dataset_agreement)) +
  scale_color_manual(values = rev(c(colset_elev_mono)), 
                     #labels = levels(to_plot_sf$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Elevation") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "supplement/elev_map.png"), width = 12, height = 8)


#prec_quantile
levels(prec_mask$prec_quant) <- c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", 
                                  "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", 
                                  "0.8-0.9", "0.9-1")


to_plot_sf <- prec_mask[, .(lon, lat, prec_quant)
][, value := as.numeric(prec_quant)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(prec_quant = 
                                      case_when(to_plot_sf$value == 1 ~ "0-0.1", 
                                                to_plot_sf$value == 2 ~ "0.1-0.2", 
                                                to_plot_sf$value == 3 ~ "0.2-0.3", 
                                                to_plot_sf$value == 4 ~ "0.3-0.4", 
                                                to_plot_sf$value == 5 ~ "0.4-0.5", 
                                                to_plot_sf$value == 6 ~ "0.5-0.6", 
                                                to_plot_sf$value == 7 ~ "0.6-0.7", 
                                                to_plot_sf$value == 8 ~ "0.7-0.8", 
                                                to_plot_sf$value == 9 ~ "0.8-0.9", 
                                                to_plot_sf$value == 10 ~ "0.9-1"))

to_plot_sf$prec_quant <- factor(to_plot_sf$prec_quant, 
                                levels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", 
                                           "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", 
                                           "0.8-0.9", "0.9-1"), ordered = TRUE)
fig_prec_quant_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = prec_quant, fill = prec_quant)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_fill_manual(values = c(colset_prec_quant)) + 
  #labels = levels(to_plot_sf$rel_dataset_agreement)) +
  scale_color_manual(values = c(colset_prec_quant), 
                     #labels = levels(to_plot_sf$rel_dataset_agreement),
                     guide = "none") + 
  labs(x = NULL, y = NULL, fill = "Precipitataion\nquantile") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "supplement/prec_quant_map.png"), width = 12, height = 8)
