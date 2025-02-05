# Time Series
source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

registerDoParallel(cores = 32)

## Data
prec_masks <- pRecipe_masks()

## Sensitivity
### Country
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "country_ranking.csv"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "country_bootstrap.rds"))

prec_data <- prec_data[order(prec_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(country)][country != ""]

bootstrap_data<- bootstrap_data[order(bootstrap_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(country, loop_idx)][country != ""]

LIST_COUNTRIES <- unique(prec_data$country)

prec_bootstrap <- foreach (idx = 1:length(LIST_COUNTRIES), .combine = rbind) %dopar% {
  dummie_country <- LIST_COUNTRIES[idx]
  dummie_all <- prec_data[country == dummie_country, dataset]
  dummie_test <- bootstrap_data[country == dummie_country]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                        .(country, loop_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/10000)), .(country)]
}

prec_bootstrap[pct < .1, prec := 1
][pct >= .1 & pct < .25, prec := 2
][pct >= .25 & pct < .50, prec := 3
][pct >= .50 & pct < .75, prec := 4
][pct >= .75, prec := 5]

prec_bootstrap <- merge(prec_masks[, .(lon, lat, country)], prec_bootstrap,
      by = "country")

to_plot_country <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_country$prec <- as.character(to_plot_country$prec)

### Basin
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "basin_ranking.csv"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "basin_bootstrap.rds"))

prec_data <- prec_data[order(prec_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(basin_name)]

bootstrap_data<- bootstrap_data[order(bootstrap_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(basin_name, loop_idx)]

LIST_BASINS <- unique(prec_data$basin_name)

prec_bootstrap <- foreach (idx = 1:length(LIST_BASINS), .combine = rbind) %dopar% {
  dummie_basin <- LIST_BASINS[idx]
  dummie_all <- prec_data[basin_name == dummie_basin, dataset]
  dummie_test <- bootstrap_data[basin_name == dummie_basin]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                        .(basin_name, loop_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/10000)), .(basin_name)]
}

prec_bootstrap[pct < .1, prec := 1
][pct >= .1 & pct < .25, prec := 2
][pct >= .25 & pct < .50, prec := 3
][pct >= .50 & pct < .75, prec := 4
][pct >= .75, prec := 5]

prec_bootstrap <- merge(prec_masks[basin_name != "Tana", .(lon, lat, basin_name)],
                        prec_bootstrap,
                        by = "basin_name")

to_plot_basin <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_basin$prec <- as.character(to_plot_basin$prec)

### IPCC
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "ipcc_ranking.csv"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "ipcc_bootstrap.rds"))

prec_data <- prec_data[order(prec_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(ipcc_region)]

bootstrap_data<- bootstrap_data[order(bootstrap_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(ipcc_region, loop_idx)]

LIST_IPCC <- unique(prec_data$ipcc_region)

prec_bootstrap <- foreach (idx = 1:length(LIST_IPCC), .combine = rbind) %dopar% {
  dummie_ipcc <- LIST_IPCC[idx]
  dummie_all <- prec_data[ipcc_region == dummie_ipcc, dataset]
  dummie_test <- bootstrap_data[ipcc_region == dummie_ipcc]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                        .(ipcc_region, loop_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/10000)), .(ipcc_region)]
}

prec_bootstrap[pct < .1, prec := 1
][pct >= .1 & pct < .25, prec := 2
][pct >= .25 & pct < .50, prec := 3
][pct >= .50 & pct < .75, prec := 4
][pct >= .75, prec := 5]

prec_bootstrap <- merge(prec_masks[ipcc_short_region != "NAO" &
                                     ipcc_short_region != "EAO" &
                                     ipcc_short_region != "SOO" &
                                     ipcc_short_region != "SIO" &
                                     ipcc_short_region != "EIO" &
                                     ipcc_short_region != "BOB" &
                                     ipcc_short_region != "ARS" &
                                     ipcc_short_region != "NPO" &
                                     ipcc_short_region != "EPO" &
                                     ipcc_short_region != "SPO",
                                   .(lon, lat, ipcc_region)],
                        prec_bootstrap,
                        by = "ipcc_region")

to_plot_ipcc <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_ipcc$prec <- as.character(to_plot_ipcc$prec)

### KG
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "kg_ranking.csv"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "kg_bootstrap.rds"))

prec_data <- prec_data[order(prec_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(KG_class)]

bootstrap_data<- bootstrap_data[order(bootstrap_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(KG_class, loop_idx)]

LIST_KG <- unique(prec_data$KG_class)

prec_bootstrap <- foreach (idx = 1:length(LIST_KG), .combine = rbind) %dopar% {
  dummie_kg <- LIST_KG[idx]
  dummie_all <- prec_data[KG_class == dummie_kg, dataset]
  dummie_test <- bootstrap_data[KG_class == dummie_kg]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                        .(KG_class, loop_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/10000)), .(KG_class)]
}

prec_bootstrap[pct < .1, prec := 1
][pct >= .1 & pct < .25, prec := 2
][pct >= .25 & pct < .50, prec := 3
][pct >= .50 & pct < .75, prec := 4
][pct >= .75, prec := 5]

prec_bootstrap <- merge(prec_masks[KG_class != "Ocean" &
                                     KG_class != "EF",
                                   .(lon, lat, KG_class)],
                        prec_bootstrap,
                        by = "KG_class")

to_plot_kg <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_kg$prec <- as.character(to_plot_kg$prec)

### Biome
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "biome_ranking.csv"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "biome_bootstrap.rds"))

prec_data <- prec_data[order(prec_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(biome_short_class)]

bootstrap_data<- bootstrap_data[order(bootstrap_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(biome_short_class, loop_idx)]

LIST_BIOME <- unique(prec_data$biome_short_class)

prec_bootstrap <- foreach (idx = 1:length(LIST_BIOME), .combine = rbind) %dopar% {
  dummie_biome <- LIST_BIOME[idx]
  dummie_all <- prec_data[biome_short_class == dummie_biome, dataset]
  dummie_test <- bootstrap_data[biome_short_class == dummie_biome]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                        .(biome_short_class, loop_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/10000)), .(biome_short_class)]
}

prec_bootstrap[pct < .1, prec := 1
][pct >= .1 & pct < .25, prec := 2
][pct >= .25 & pct < .50, prec := 3
][pct >= .50 & pct < .75, prec := 4
][pct >= .75, prec := 5]

prec_bootstrap <- merge(prec_masks[biome_short_class != "Water" & biome_short_class != "Polar",
                                   .(lon, lat, biome_short_class)],
                        prec_bootstrap,
                        by = "biome_short_class")

to_plot_biome <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_biome$prec <- as.character(to_plot_biome$prec)

### Elevation
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "elev_ranking.csv"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "elev_bootstrap.rds"))

prec_data <- prec_data[order(prec_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(elev_class)]

bootstrap_data<- bootstrap_data[order(bootstrap_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(elev_class, loop_idx)]

LIST_ELEV <- unique(prec_data$elev_class)

prec_bootstrap <- foreach (idx = 1:length(LIST_ELEV), .combine = rbind) %dopar% {
  dummie_elev <- LIST_ELEV[idx]
  dummie_all <- prec_data[elev_class == dummie_elev, dataset]
  dummie_test <- bootstrap_data[elev_class == dummie_elev]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                        .(elev_class, loop_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/10000)), .(elev_class)]
}

prec_bootstrap[pct < .1, prec := 1
][pct >= .1 & pct < .25, prec := 2
][pct >= .25 & pct < .50, prec := 3
][pct >= .50 & pct < .75, prec := 4
][pct >= .75, prec := 5]

prec_bootstrap <- merge(prec_masks[lat >= -60 & elev_class != "",
                                   .(lon, lat, elev_class)],
                        prec_bootstrap,
                        by = "elev_class")

to_plot_elev <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_elev$prec <- as.character(to_plot_elev$prec)

### Land Cover
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "land_cover_ranking.csv"))

bootstrap_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "land_cover_bootstrap.rds"))

prec_data <- prec_data[order(prec_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(land_cover_short_class)]

bootstrap_data<- bootstrap_data[order(bootstrap_data$prec_t, decreasing = TRUE)
][, head(.SD, 3), .(land_cover_short_class, loop_idx)]

LIST_LAND <- unique(prec_data$land_cover_short_class)

prec_bootstrap <- foreach (idx = 1:length(LIST_LAND), .combine = rbind) %dopar% {
  dummie_land <- LIST_LAND[idx]
  dummie_all <- prec_data[land_cover_short_class == dummie_land, dataset]
  dummie_test <- bootstrap_data[land_cover_short_class == dummie_land]
  dummie <- dummie_test[, .(first_check = ifelse(sum(dummie_all %in% dataset) >= 2, 1, 0)),
                        .(land_cover_short_class, loop_idx)]
  dummie <- dummie[, .(pct = 1 - sum(first_check/10000)), .(land_cover_short_class)]
}

prec_bootstrap[pct < .1, prec := 1
][pct >= .1 & pct < .25, prec := 2
][pct >= .25 & pct < .50, prec := 3
][pct >= .50 & pct < .75, prec := 4
][pct >= .75, prec := 5]

prec_bootstrap <- merge(prec_masks[land_cover_short_class != "Water" & land_cover_short_class != "Snow/Ice",
                                   .(lon, lat, land_cover_short_class)],
                        prec_bootstrap,
                        by = "land_cover_short_class")

to_plot_land <- prec_bootstrap[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_land$prec <- as.character(to_plot_land$prec)

## Plots
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")
borders_sf <- read_sf("~/shared/data/geodata/world_borders/border_shapefile.shp")
ipcc_sf <- read_sf("~/shared/data/geodata/ipcc_v4/IPCC-WGI-reference-regions-v4.shp")
basin_sf <- read_sf("~/shared/data/geodata/major_basins/Major_Basins_of_the_World.shp")
kg_sf <- read_sf("~/shared/data/geodata/kg_classes/shapefile_kg.shp")
kg_sf <- kg_sf[kg_sf$layer != "Ocean",]
biome_sf <- read_sf("~/shared/data/geodata/biome_shapefile/biome_shapefile.shp")
elev_sf <- read_sf("~/shared/data/geodata/elevation_shapefile/elev_shapefile.shp")
land_sf <- read_sf("~/shared/data/geodata/land_cover_shapefile/land_shapefile.shp")

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

### Country
p01 <- ggplot(to_plot_country) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  geom_sf(data = borders_sf, fill = NA, color = "gray23") +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
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

p_legend <- get_legend(p01)

### Basin
p02 <- ggplot(to_plot_basin) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = basin_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
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

### IPCC
p03 <- ggplot(to_plot_ipcc) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
  geom_sf(data = borders_sf, fill = NA, color = "gray23") +
  geom_sf_text(data = ipcc_sf, aes(label = Acronym), size = 2,
               fontface = "bold") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
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

### KG
p04 <- ggplot(to_plot_kg) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = kg_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
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

### Biome
p05 <- ggplot(to_plot_biome) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = biome_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
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

### Elevation
p06 <- ggplot(to_plot_elev) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = elev_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
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

### Land Cover
p07 <- ggplot(to_plot_elev) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = land_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                               "3" = "#F4CC70", "4" = "#E38B75",
                               "5" = "#CE5A57"),
                    labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                               "3" = "Confidence",
                               "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
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

###
p00 <- ggarrange(p01 + guides(fill="none"), 
                 p02 + guides(fill="none"),
                 p03 + guides(fill="none"),
                 p04 + guides(fill="none"),
                 p05 + guides(fill="none"),
                 p06 + guides(fill="none"),
                 p07 + guides(fill="none"),
                 p_legend,
                 ncol = 2, nrow = 4, labels = c("a)", "b)", "c)", "d)", "e)",
                                                "f)", "g)"))

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "domain_sensitivity.png"),
       width = 5*GOLDEN_RATIO*2, height = 5*4)
