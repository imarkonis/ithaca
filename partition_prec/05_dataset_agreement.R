# Plot global map of dataset agreement classses 
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)

# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")

prec_grid <- read_stars(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                               "prec_station_grid.nc")) %>% st_as_sf()
colnames(prec_grid)[1] <- "value"
st_crs(prec_grid) <- "+proj=longlat +datum=WGS84 +no_defs"

prec_mask_sf <- prec_mask[, .(lon, lat, rel_dataset_agreement)
                          ][, value := as.numeric(rel_dataset_agreement)]
prec_mask_sf <- prec_mask_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

#World and Land borders
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

#Labels
labs_y <- data.frame(lon = -172, lat = seq(60, -60, -30))
labs_y$label <- ifelse(labs_y$lat == 0, "°", ifelse(labs_y$lat > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y$lat), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

# Figures
fig_stations <- ggplot(prec_grid) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(color = "dark red") +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_color_viridis_c(option = "H") +
  labs(x = NULL, y = NULL, color = "No.\nStations") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color="black", size = 7) +
  geom_sf_text(data = labs_x, aes(label = label), color="black", size = 7) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour="gray30"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

fig_dataset_agreement <- ggplot(prec_mask_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_fill_manual(values = colset_RdBu_5,
                    labels = levels(prec_mask$rel_dataset_agreement)) +
  scale_color_manual(values = colset_RdBu_5,
                     labels = levels(prec_mask$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset\nAgreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color="black", size = 7) +
  geom_sf_text(data = labs_x, aes(label = label), color="black", size = 7) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour="gray30"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

gg_fig <- ggarrange(fig_dataset_agreement, fig_stations,
                      labels = c('a', 'b'), align = 'hv',
                      common.legend = TRUE, legend = 'right', 
                      nrow = 2, ncol = 1) +
  bgcolor("white")     

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "SI_dataset_agreement_maps.png"), width = 12, height = 10)
