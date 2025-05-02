# Maps
source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_masks <- pRecipe_masks()

### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")
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

##
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "country_ranking.csv"))

DATASETS <- c(unique(prec_data$dataset), NA) %>% as.factor()
levels(DATASETS) <- c(levels(DATASETS), "Others")
DATASETS[is.na(DATASETS)] <- "Others"

TOP_DATASETS <- c("cmap", "cru-ts-v4-07", "em-earth", "era5-land", "fldas",
                  "gpm-imerg-v7", "jra55", "merra2-land", "mswep-v2-8",
                  "persiann-cdr", "precl", "terraclimate") %>%
  factor(levels(DATASETS))

prec_data <- prec_data[, .SD[which.max(prec_t)], by = .(country)
][country != ""]
prec_data <- merge(prec_masks[, .(lon, lat, country)], prec_data,
                   by = "country")
prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, prec_t, dataset = factor(dataset,
                                         levels(DATASETS)))]

to_plot_country <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_country$name <- levels(DATASETS)[to_plot_country$name] %>%
  factor(levels(DATASETS))

p_legend <- ggplot(to_plot_country) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                               "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                               "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                               "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                               "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                               "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                               "terraclimate" = "#E7298A"),
                    drop = FALSE,
                    labels = c("cmap" = "CMAP", "cru-ts-v4-07" = "CRU TS v4.07",
                               "em-earth" = "EM-Earth",
                               "era5-land" = "ERA5-Land",
                               "fldas" = "FLDAS",
                               "gpm-imerg-v7" = "GPM-IMERG v7",
                               "jra55" = "JRA-55",
                               "merra2-land" = "MERRA-2 Land",
                               "mswep-v2-8" = "MSWEP v2.8",
                               "persiann-cdr" = "PERSIANN-CDR",
                               "precl" = "PREC/L",
                               "terraclimate" = "TerraClimate")) +
  scale_color_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                                "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                                "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                                "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                                "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                                "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                                "terraclimate" = "#E7298A"),
                     guide = "none") +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  labs(x = NULL, y = NULL, fill = "Dataset") +
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

p_legend <- get_legend(p_legend, position = "bottom")

### Biome
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "biome_ranking.csv"))

DATASETS <- c(unique(prec_data$dataset), NA) %>% as.factor()
levels(DATASETS) <- c(levels(DATASETS), "Others")
DATASETS[is.na(DATASETS)] <- "Others"

TOP_DATASETS <- c("cmap", "cru-ts-v4-07", "em-earth", "era5-land", "fldas",
                  "gpm-imerg-v7", "jra55", "merra2-land", "mswep-v2-8",
                  "persiann-cdr", "precl", "terraclimate") %>%
  factor(levels(DATASETS))

prec_data <- prec_data[biome_short_class != "Water" & biome_short_class != "Polar", .SD[which.max(prec_t)], by = .(biome_short_class)
]
prec_data <- merge(prec_masks[, .(lon, lat, biome_short_class)], prec_data,
                   by = "biome_short_class")
prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, prec_t, dataset = factor(dataset,
                                         levels(DATASETS)))]

to_plot_biome <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_biome$name <- levels(DATASETS)[to_plot_biome$name] %>%
  factor(levels(DATASETS))

to_plot_t_biome <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p01 <- ggplot(to_plot_biome) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = biome_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                               "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                               "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                               "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                               "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                               "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                               "terraclimate" = "#E7298A"),
                    drop = FALSE,
                    labels = c("cmap" = "CMAP", "cru-ts-v4-07" = "CRU TS v4.07",
                               "em-earth" = "EM-Earth",
                               "era5-land" = "ERA5-Land",
                               "fldas" = "FLDAS",
                               "gpm-imerg-v7" = "GPM-IMERG v7",
                               "jra55" = "JRA-55",
                               "merra2-land" = "MERRA-2 Land",
                               "mswep-v2-8" = "MSWEP v2.8",
                               "persiann-cdr" = "PERSIANN-CDR",
                               "precl" = "PREC/L",
                               "terraclimate" = "TerraClimate")) +
  scale_color_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                                "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                                "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                                "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                                "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                                "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                                "terraclimate" = "#E7298A"),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset") +
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

p04 <- ggplot(to_plot_t_biome) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = biome_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_viridis_c(limits = c(0.45, 1), option = "H", direction = -1) +
  scale_color_viridis_c(limits = c(0.45, 1), option = "H", guide = "none",
                        direction = -1) +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  labs(x = NULL, y = NULL, fill = "T-metric") +
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
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        legend.key.width = unit(dev.size()[2]/3, "inches"),
        legend.key.height = unit(dev.size()[2]/10, "inches"))

###
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "elev_ranking.csv"))

prec_data <- prec_data[elev_class != "", .SD[which.max(prec_t)], by = .(elev_class)]

prec_data <- merge(prec_masks[lat >= -60, .(lon, lat, elev_class)],
                   prec_data, by = "elev_class")

prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, prec_t,
      dataset = factor(dataset, levels(DATASETS)))]

to_plot_elev <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_elev$name <- levels(DATASETS)[to_plot_elev$name] %>%
  factor(levels(DATASETS))

to_plot_t_elev <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p02 <- ggplot(to_plot_elev) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = elev_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                               "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                               "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                               "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                               "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                               "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                               "terraclimate" = "#E7298A"),
                    drop = FALSE,
                    labels = c("cmap" = "CMAP", "cru-ts-v4-07" = "CRU TS v4.07",
                               "em-earth" = "EM-Earth",
                               "era5-land" = "ERA5-Land",
                               "fldas" = "FLDAS",
                               "gpm-imerg-v7" = "GPM-IMERG v7",
                               "jra55" = "JRA-55",
                               "merra2-land" = "MERRA-2 Land",
                               "mswep-v2-8" = "MSWEP v2.8",
                               "persiann-cdr" = "PERSIANN-CDR",
                               "precl" = "PREC/L",
                               "terraclimate" = "TerraClimate")) +
  scale_color_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                                "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                                "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                                "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                                "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                                "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                                "terraclimate" = "#E7298A"),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset") +
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

p05 <- ggplot(to_plot_t_elev) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = elev_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_viridis_c(limits = c(0.45, 1), option = "H", direction = -1) +
  scale_color_viridis_c(limits = c(0.45, 1), option = "H", guide = "none",
                        direction = -1) +
  labs(x = NULL, y = NULL, fill = "T-metric") +
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
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        legend.key.width = unit(dev.size()[2]/3, "inches"),
        legend.key.height = unit(dev.size()[2]/10, "inches"))

###
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "land_cover_ranking.csv"))

prec_data <- prec_data[land_cover_short_class != "Water" & land_cover_short_class != "Snow/Ice",
                       .SD[which.max(prec_t)], by = .(land_cover_short_class)]

prec_data <- merge(prec_masks[, .(lon, lat, land_cover_short_class)], prec_data,
                   by = "land_cover_short_class")

prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, prec_t,
      dataset = factor(dataset, levels(DATASETS)))]

to_plot_land <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_land$name <- levels(DATASETS)[to_plot_land$name] %>%
  factor(levels(DATASETS))

to_plot_t_land <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p03 <- ggplot(to_plot_land) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = land_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                               "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                               "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                               "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                               "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                               "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                               "terraclimate" = "#E7298A"),
                    drop = FALSE,
                    labels = c("cmap" = "CMAP", "cru-ts-v4-07" = "CRU TS v4.07",
                               "em-earth" = "EM-Earth",
                               "era5-land" = "ERA5-Land",
                               "fldas" = "FLDAS",
                               "gpm-imerg-v7" = "GPM-IMERG v7",
                               "jra55" = "JRA-55",
                               "merra2-land" = "MERRA-2 Land",
                               "mswep-v2-8" = "MSWEP v2.8",
                               "persiann-cdr" = "PERSIANN-CDR",
                               "precl" = "PREC/L",
                               "terraclimate" = "TerraClimate")) +
  scale_color_manual(values = c("cmap" = "#32CD32", "cru-ts-v4-07" = "#f5cb00",
                                "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                                "fldas" = "#ff07ff", "gpm-imerg-v7" = "#1b9e77",
                                "jra55" = "#E6E6FA", "merra2-land" = "#7570B3",
                                "mswep-v2-8" = "#00ffb6", "Others" = "gray23",
                                "persiann-cdr" = "#228B22", "precl" = "#ffed00",
                                "terraclimate" = "#E7298A"),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset") +
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

p06 <- ggplot(to_plot_t_land) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = land_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_viridis_c(limits = c(0.45, 1), option = "H", direction = -1) +
  scale_color_viridis_c(limits = c(0.45, 1), option = "H", guide = "none",
                        direction = -1) +
  labs(x = NULL, y = NULL, fill = "T-metric") +
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
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        legend.key.width = unit(dev.size()[2]/3, "inches"),
        legend.key.height = unit(dev.size()[2]/10, "inches"))

###
p07 <- ggarrange(p01, p02, p03, ncol = 1, nrow = 3, common.legend = TRUE,
                 legend = "bottom", labels = c("a)", "b)", "c)"),
                 legend.grob = p_legend)

#p08 <- ggarrange(p04, p05, p06, ncol = 1, nrow = 3, common.legend = TRUE,
#                 legend = "bottom", labels = c("b)", "d)", "f)"))

#p00 <- ggarrange(p07, p08, ncol = 2, nrow = 1, align = "hv")

ggsave(plot = p07,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
              "maps_first_rank_2.png"), width = 5*GOLDEN_RATIO,
       height = 5*3)
