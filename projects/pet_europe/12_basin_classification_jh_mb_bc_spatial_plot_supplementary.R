################################################
# Basin classification exculding JH, MB and BC
######################################################
library(data.table)
library(fst)
library(sf)
library(ggplot2)
library(gridExtra)
library(grid)


FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/figures/"

#loading data 
shp_area_dt <- readRDS(paste0(FILE_PATH, "shapefile_area_datatable.rds"))
basin_classification <- read_fst(paste0(FILE_PATH, "basin_classification.fst"), as.data.table = TRUE)
monthly_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc.fst"), as.data.table = TRUE)
monthly_dt[, YEAR := year(date)]

#computing monthly to annual scale
yearly_dt <- monthly_dt[YEAR >= 1980 & YEAR <= 2019, 
                        .(value = sum(value, na.rm = TRUE)), by = .(basin,pet_method,variable,YEAR)]

#Getting required variable PET, AET, P
yearly_dt_dcast <- dcast(yearly_dt, basin + pet_method + YEAR ~ variable)
subset_yearly_dt  <- yearly_dt_dcast[, c("basin", "pet_method", "YEAR", "pet", "pre", "aet")] 


#Computing aridity and evaporative index
aridity_dt <- subset_yearly_dt[, lapply(.SD, mean, na.rm=TRUE), by=.(basin,pet_method), .SDcols=c("pre","pet", "aet")
                               ][,aridity_idx := pet/pre
                                 ][, evaporative_idx := aet/pre] 

#method to omit from the classification
aridity <- aridity_dt[!pet_method %in% c('pet_jh', "pet_mb", "pet_bc")]

aridity[, basin_type := fifelse(aridity_idx > 1 , "water_limited","energy_limited" )
        ][basin_type == "energy_limited", basin_type_1 := fifelse(length(basin_type) == length(unique(aridity$pet_method) ), "energy_limited","mixed"), by = .(basin)
           ][basin_type == "water_limited", basin_type_1 := fifelse(length(basin_type) == length(unique(aridity$pet_method) ), "water_limited","mixed"), by = .(basin)]

#Merging aridity datatable with basin types, area and shapefiles 
aridity <- merge(aridity, shp_area_dt, by = "basin", allow.cartesian = TRUE)
setorder(aridity, -area)
europe_shp <- st_read(paste0(FILE_PATH, "/europe/Europe_coastline_poly.shp"))
europe_shp <- st_transform(europe_shp, 4326)

#Spatial plot of catchments based on its classification 
p1 <- ggplot() +
  geom_sf(data = europe_shp, aes())+
  scale_x_continuous(limits = c(-9, 32.5), breaks = c(-8, 0, 10, 20, 30)) +
  scale_y_continuous(limits = c(38, 69))+
  geom_sf(data = aridity, aes(geometry = geometry, fill = basin_type_1), color = "#222222")+
  coord_sf(crs = st_crs(4326))+
  theme_bw()+
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.text.x = element_text(size = 18, color = "black"), 
    strip.text = element_text( size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    legend.title = element_blank(),
    legend.direction = "vertical", 
    legend.position = c(0.2,0.9),
    legend.key.width = unit(0.4,"cm"),
    legend.key.height = unit(0.1,"cm"),
    legend.text = element_text(size = 18, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1)
  )+
  labs(y = NULL)+ 
  guides(fill = guide_legend(byrow = TRUE))+
  scale_fill_manual(
    values = c("energy_limited" = "#A2C523", "mixed" = "#B8860B", "water_limited" = "#5CC5EF"),
    labels = c("energy_limited" = "Energy-Limited", "mixed" = "Mixed", "water_limited" = "Water-Limited")
  )

ggsave(paste0(SAVE_PATH,"basin_classification_bc_jh_mb.png"), p1,  width = 11, height = 12, 
       units = "in", dpi = 300)






