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

### Countries
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "country_ranking.csv"))

DATASETS <- c(unique(prec_data$dataset), NA) %>% as.factor()
levels(DATASETS) <- c(levels(DATASETS), "Others")
DATASETS[is.na(DATASETS)] <- "Others"

TOP_DATASETS <- c("cmap", "cpc", "cru-ts-v4-07", "em-earth", "era5-land",
                  "fldas", "gpm-imerg-v7", "jra55", "mswep-v2-8",
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

to_plot_t_metric <- prec_data[, .(lon, lat, prec_t)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p01 <- ggplot(to_plot_country) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("cmap" = "#b2df8a", "cpc" = "#ffff99",
                               "cru-ts-v4-07" = "#fdbf6f",
                               "em-earth" = "#ff7f00", "era5-land" = "#e31a1c",
                               "fldas" = "#fb9a99", "gpm-imerg-v7" = "#1f78b4",
                               "jra55" = "#cab2d6", "mswep-v2-8" = "#33a02c",
                               "Others" = "gray23", "persiann-cdr" = "#a6cee3",
                               "precl" = "#b15928", "terraclimate" = "#6a3d9a"),
                    labels = c("cmap" = "CMAP", "cpc" = "CPC Global",
                               "cru-ts-v4-07" = "CRU TS v4.07",
                               "em-earth" = "EM-Earth",
                               "era5-land" = "ERA5-Land",
                               "fldas" = "FLDAS",
                               "gpm-imerg-v7" = "GPM-IMERG v7",
                               "jra55" = "JRA-55",
                               "mswep-v2-8" = "MSWEP v2.8",
                               "persiann-cdr" = "PERSIANN-CDR",
                               "precl" = "PREC/L",
                               "terraclimate" = "TerraClimate")) +
  scale_color_manual(values = c("cmap" = "#b2df8a", "cpc" = "#ffff99",
                                "cru-ts-v4-07" = "#fdbf6f",
                                "em-earth" = "#ff7f00", "era5-land" = "#e31a1c",
                                "fldas" = "#fb9a99", "gpm-imerg-v7" = "#1f78b4",
                                "jra55" = "#cab2d6", "mswep-v2-8" = "#33a02c",
                                "Others" = "gray23", "persiann-cdr" = "#a6cee3",
                                "precl" = "#b15928", "terraclimate" = "#6a3d9a"),
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
        legend.title = element_text(size = 16),
        legend.position="bottom")

p02 <- ggplot(to_plot_t_metric) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec_t, fill = prec_t)) +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_viridis_c(limits = c(0.47, 1), option = "H", direction = -1) +
  scale_color_viridis_c(limits = c(0.47, 1), option = "H", guide = "none",
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
p00 <- ggarrange(p01, p02, ncol = 2, nrow = 1, labels = c("(a)", "(b)"))

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
              "country_first_rank.png"), width = 5*GOLDEN_RATIO*2,
       height = 5)
