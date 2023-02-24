# Plot global map of data set agreement classes
rm(list=ls(all=TRUE))
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)
library(raster)
library(rgdal)
library(sp)
library(sf)


# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")

prec_grid <- read_stars(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                               "prec_station_grid.nc")) %>% st_as_sf()
colnames(prec_grid)[1] <- "value"

st_crs(prec_grid) <- "+proj=longlat +datum=WGS84 +no_defs"
prec_grid_rproj <-st_transform(prec_grid, "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0")


prec_mask_sf <- prec_mask[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
prec_mask_sf <- prec_mask_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

prec_mask_sf_rproj <-st_transform(prec_mask_sf, "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0")


# #World and Land borders
# earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
#                             "earth_box.rds")) %>%
#   st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
# 
# world_sf <- ne_countries(returnclass = "sf")


#Labels
#Labels
lbl.Y     <- data.frame(lon = -168, lat = seq(60, -60, -30))
lbl.Y$dir <- ifelse(lbl.Y$lat == 0, "", ifelse(lbl.Y$lat > 0, "°N", "°S"))
lbl.Y$lbl <- paste0(abs(lbl.Y$lat), lbl.Y$dir)

lbl.X <- data.frame(lon = seq(120, -120, -60), lat = -82)
lbl.X$dir <- ifelse(lbl.X$lon == 0, "", ifelse(lbl.X$lon > 0, "°E", "°W"))
lbl.X$lbl <- paste0(abs(lbl.X$lon), lbl.X$dir)

PROJ <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0" 

prj.coord <- project(cbind(lbl.Y$lon, lbl.Y$lat), proj=PROJ)
lbl.Y.prj <- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2] <- c("X.prj","Y.prj")

prj.coord <- project(cbind(lbl.X$lon, lbl.X$lat), proj=PROJ)
lbl.X.prj <- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2] <- c("X.prj","Y.prj")

# Countries 
countries <- readOGR("~/ithaca/partition_prec/map_file/countries/", layer="ne_110m_admin_0_countries") 
count_proj <- spTransform(countries, CRS("+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0"))

# boundary box
bb_shapefile <- readOGR(dsn= path.expand("~/ithaca/partition_prec/map_file/bbox/"),
                        layer="ne_50m_wgs84_bounding_box")
bb_lines_proj <- spTransform(bb_shapefile, CRS("+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0"))


# Graticules if needed
grat <- readOGR("~/ithaca/partition_prec/map_file/gratules", layer="ne_110m_graticules_15") 
grat_prj <- spTransform(grat, CRSobj = PROJ)


# Figures
fig_stations <- ggplot(prec_grid_rproj) +
  geom_polygon(data = count_proj, aes(x = long, y = lat, group = group), fill="light gray", color = "transparent", linewidth = 0.05) + #0.2
  #geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  #geom_path(data=grat_prj, aes(x = long, y = lat, group = group, fill=NULL), linetype="solid", color="black", size = 0.05) + #0.2
  geom_sf(color = "dark red") +
  geom_path(data=bb_lines_proj, aes(x = long, y = lat, group = group, fill = NULL), linetype="solid", color="black", linewidth = 0.7) + #0.2
  #geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.5) +
  #scale_color_viridis_c(option = "H") +
  labs(x = NULL, y = NULL) +
  #coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="black", size = 5) +
  geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="black", size = 5) +
  theme_void() +
  theme(panel.background = element_blank(), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="black", size = 0.25),
        axis.text = element_blank(), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))



fig_dataset_agreement <- ggplot(prec_mask_sf_rproj) +
  geom_polygon(data = count_proj, aes(x = long, y = lat, group = group), fill="light gray", color = "transparent", linewidth = 0.05) + #0.2
  #geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_path(data=bb_lines_proj, aes(x = long, y = lat, group = group, fill = NULL), linetype="solid", color="black", linewidth = 0.7) + #0.2  # geom_path(data=grat_prj, aes(x = long, y = lat, group = group, fill = NULL), linetype="dashed", color="#BFBBB4", linewidth = 0.2) + #0.2
  #geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.8) +
  scale_fill_manual(values = colset_RdBu_5,
                    labels = levels(prec_mask$rel_dataset_agreement)) +
  scale_color_manual(values = colset_RdBu_5,
                     labels = levels(prec_mask$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset \nAgreement            ") + # Don't remove blank space, as it protect cutting of legends (only in case of ggarrange)
  #coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="black", size = 5) +
  geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="black", size = 5) +
  theme_void() +
  #theme_bw() +
  theme(panel.background = element_blank(), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour="black", linewidth = 0.3),
        axis.text = element_blank(), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 20))

gg_fig <- ggarrange(fig_dataset_agreement, fig_stations,
                    labels = c('a', 'b'), align = 'hv',
                    common.legend = TRUE, legend = 'right',
                    nrow = 2, ncol = 1) +
  bgcolor("white")

# ggsave("SI_dataset_agreement_maps2.png", width = 12, height = 10) #check plot

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "SI_dataset_agreement_maps.png"), width = 12, height = 10)