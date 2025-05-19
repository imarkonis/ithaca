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
borders_sf <- read_sf("~/shared/data/geodata/world_borders/border_shapefile.shp")
ipcc_sf <- read_sf("~/shared/data/geodata/ipcc_v4/IPCC-WGI-reference-regions-v4.shp")
basin_sf <- read_sf("~/shared/data/geodata/major_basins/Major_Basins_of_the_World.shp")
kg_sf <- read_sf("~/shared/data/geodata/kg_classes/shapefile_kg.shp")
kg_sf <- kg_sf[kg_sf$layer != "Ocean",]
  
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

MAP_COLORS <- c("cmap" = "#194f46", "cru-ts-v4-08" = "#ffffbf",
                "em-earth" = "#e6ab02", "era5-land" = "#8B008B",
                "fldas" = "#ff07ff", "gpcp-cdr-v3-2" = "#399283",
                "gpm-imerg-v7" = "#21f0b6", "merra2-land" = "#7570B3",
                "mswep-v2-8" = "#238910", "Others" = "gray23",
                "persiann-cdr" = "#78ee5a", "precl" = "#ffed00",
                "terraclimate" = "#E7298A")
MAP_LABELS <- c("cmap" = "CMAP", "cru-ts-v4-08" = "CRU TS v4.08",
                "precl" = "PREC/L", "em-earth" = "EM-Earth",
                "era5-land" = "ERA5-Land", "fldas" = "FLDAS",
                "gpcp-cdr-v3-2" = "GPCP CDR v3.2",
                "gpm-imerg-v7" = "GPM IMERG v7", "merra2-land" = "MERRA-2 Land",
                "mswep-v2-8" = "MSWEP v2.8", "persiann-cdr" = "PERSIANN CDR",
                "terraclimate" = "TerraClimate")

### Countries
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "country_ranking.csv"))

DATA_LEVELS = c("cpc-global", "cru-ts-v4-08", "em-earth", "gpcc-v2022", "precl",
                "cmap", "cmorph-cdr", "gpcp-cdr-v3-2", "gpm-imerg-v7",
                "gsmap-v8", "mswep-v2-8", "persiann-cdr", "era5", "era5-land",
                "jra55", "merra2-land", "ncep-ncar", "ncep-doe", "fldas",
                "terraclimate")
DATASETS <- c(unique(prec_data$dataset), NA) %>% factor(levels = DATA_LEVELS)
levels(DATASETS) <- c(levels(DATASETS), "Others")
DATASETS[is.na(DATASETS)] <- "Others"

TOP_DATASETS <- c("cru-ts-v4-08", "em-earth", "precl", "cmap", "gpcp-cdr-v3-2",
                  "gpm-imerg-v7", "mswep-v2-8", "persiann-cdr", "era5-land",
                  "merra2-land", "fldas", "terraclimate") %>%
  factor(levels(DATASETS))

prec_data <- prec_data[, .SD[which.max(prec_t)], by = .(country)
                       ][country != ""]
prec_data <- merge(prec_masks[, .(lon, lat, country)], prec_data,
                   by = "country")
prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
                       ][, .(lon, lat, prec_t,
                             dataset = factor(dataset, levels(DATASETS)))]

to_plot_country <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_country$name <- levels(DATASETS)[to_plot_country$name] %>%
  factor(levels(DATASETS))

to_plot_t_country <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p01 <- ggplot(to_plot_country) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = borders_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = MAP_COLORS, drop = FALSE, labels = MAP_LABELS)+
  scale_color_manual(values = MAP_COLORS, guide = "none") +
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

p05 <- ggplot(to_plot_t_country) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
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
                           "basin_ranking.csv"))

prec_data <- prec_data[, .SD[which.max(prec_t)], by = .(basin_name)]

prec_data <- merge(prec_masks[basin_name != "Tana", .(lon, lat, basin_name)],
                   prec_data, by = "basin_name")

prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, prec_t,
      dataset = factor(dataset, levels(DATASETS)))]

to_plot_basin <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_basin$name <- levels(DATASETS)[to_plot_basin$name] %>%
  factor(levels(DATASETS))

to_plot_t_basin <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p02 <- ggplot(to_plot_basin) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = basin_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = MAP_COLORS, drop = FALSE, labels = MAP_LABELS)+
  scale_color_manual(values = MAP_COLORS, guide = "none") +
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

p06 <- ggplot(to_plot_t_basin) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = basin_sf, fill = NA, color = "gray23") +
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
                          "ipcc_ranking.csv"))

prec_data <- prec_data[, .SD[which.max(prec_t)], by = .(ipcc_region)]

prec_data <- merge(prec_masks[ipcc_short_region != "NAO" &
                                ipcc_short_region != "EAO" &
                                ipcc_short_region != "SOO" &
                                ipcc_short_region != "SIO" &
                                ipcc_short_region != "EIO" &
                                ipcc_short_region != "BOB" &
                                ipcc_short_region != "ARS" &
                                ipcc_short_region != "NPO" &
                                ipcc_short_region != "EPO" &
                                ipcc_short_region != "SPO",
                              .(lon, lat, ipcc_region)], prec_data,
                   by = "ipcc_region")

prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, prec_t,
      dataset = factor(dataset, levels(DATASETS)))]

to_plot_ipcc <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_ipcc$name <- levels(DATASETS)[to_plot_ipcc$name] %>%
  factor(levels(DATASETS))

to_plot_t_ipcc <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p03 <- ggplot(to_plot_ipcc) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
  geom_sf(data = borders_sf, fill = NA, color = "gray23") +
  geom_sf_text(data = ipcc_sf, aes(label = Acronym), size = 2,
               fontface = "bold") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = MAP_COLORS, drop = FALSE, labels = MAP_LABELS)+
  scale_color_manual(values = MAP_COLORS, guide = "none") +
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

p07 <- ggplot(to_plot_t_ipcc) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
  geom_sf(data = borders_sf, fill = NA, color = "gray23") +
  geom_sf_text(data = ipcc_sf, aes(label = Acronym), size = 2,
               fontface = "bold") +
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
                           "kg_ranking.csv"))

prec_data <- prec_data[KG_class != "Ocean" & KG_class != "EF",
                       .SD[which.max(prec_t)], by = .(KG_class)]

prec_data <- merge(prec_masks[country_short != "ATA" & lat >= -60,
                              .(lon, lat, KG_class)], prec_data,
                   by = "KG_class")

prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, prec_t,
      dataset = factor(dataset, levels(DATASETS)))]

to_plot_kg <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_kg$name <- levels(DATASETS)[to_plot_kg$name] %>%
  factor(levels(DATASETS))

to_plot_t_kg <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p04 <- ggplot(to_plot_kg) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = kg_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = MAP_COLORS, drop = FALSE, labels = MAP_LABELS)+
  scale_color_manual(values = MAP_COLORS, guide = "none") +
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

p08 <- ggplot(to_plot_t_kg) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = kg_sf, fill = NA, color = "gray23") +
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
p09 <- ggarrange(p01, p02, p03, p04, ncol = 2, nrow = 2, common.legend = TRUE,
                 legend = "bottom", labels = c("a)", "b)", "c)", "d)"))

#p10 <- ggarrange(p05, p06, p07, p08, ncol = 1, nrow = 4, common.legend = TRUE,
#                 legend = "bottom", labels = c("b)", "d)", "f)", "h)"))

#p00 <- ggarrange(p09, p10, ncol = 2, nrow = 1, align = "hv")

ggsave(plot = p09,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
              "maps_first_rank.png"), width = 5*GOLDEN_RATIO*2,
       height = 5*2)
