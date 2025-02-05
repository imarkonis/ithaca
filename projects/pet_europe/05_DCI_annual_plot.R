##########################################################
# DCI computation at annual scale and spatial plot   
#########################################################
# required library 
library(ggh4x)
library(fst)
library(data.table)
library(ggplot2)
library(tidyverse)
library(Kendall)
library(trend)
library(ggpmisc)
library(ggpubr)
library(sf)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/figures/"
SHP_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/europe_basin_shapefile"

monthly_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc.fst"), as.data.table = TRUE)
monthly_dt[, YEAR := year(date)]

#computing monthly to annual scale
yearly_dt <- monthly_dt[, .(value = sum(value, na.rm = TRUE)), by = .(basin, pet_method, variable, YEAR)]

#averaging the TWS by diving 12 
yearly_dt[variable == "tws", value := value/12]

#slope agreement for period 1980 to 2019 
slope_dt <- yearly_dt[YEAR >= 1980 & YEAR <= 2019, .(sen_slope = sens.slope(value, conf.level = 0.95)[[1]]) , by = .(basin, pet_method, variable)]

# Computing DCI 
slope_dt[, frac := fifelse(is.na(sen_slope), 0, sen_slope/abs(sen_slope))]
#removing the NAN due to 0 slope
slope_dt <- na.omit(slope_dt)
dci <- slope_dt[, .(dci = sum(frac)/length(frac)), by = .(basin,variable)]

shp_area_dt <- readRDS(paste0(FILE_PATH, "shapefile_area_datatable.rds"))

dci <- merge(dci, shp_area_dt, by = "basin", allow.cartesian = TRUE)
setorder(dci, -area)

################################################################################
# DCI spatial plot 
################################################################################
europe <- st_read(paste0(FILE_PATH, "/europe/Europe_coastline_poly.shp"))
europe <- st_transform(europe, 4326)

# Creating the legend range column 
dci$legend_range <- cut(dci$dci, breaks = c(-1.0001, -0.5, 0.499999999, 1),
                        labels = c("<= -0.5", "-0.5 — 0.5" , ">= 0.5"))
levels(dci$variable) <- c(twsc = "TWSC",pet = "PET", aet = "AET", q = "Q", 
                          tws = "TWS", pre = "PRE" )

p1 <- ggplot() +
  geom_sf(data = europe, aes()) +
  scale_x_continuous(limits = c(-9, 32.5)) +
  scale_y_continuous(limits = c(38, 69)) +
  geom_sf(data = dci[variable != "TWSC" & variable != "PRE"], aes(geometry=geometry, fill= legend_range)) +
  coord_sf(crs = st_crs(4326)) +
  scale_color_manual(guide = guide_legend(title = "DCI: ", title.hjust = 0.5, 
                                          title.position = "top", label.position = "right", 
                                          direction = "horizontal", label.hjust = 0.5, nrow = 1)) +
  scale_fill_manual(
    values = c("#4B0082" , "#009E73", "#d01c8b")  
  ) +
  facet_wrap(.~factor(variable, levels = c("PET", 'AET',"Q","TWS", "TWSC", "PRE")), ncol = 2) +
  theme_bw() + 
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black", family = "Helvetica", face = "bold"),  
    axis.text.x = element_text(size = 14, color = "black", hjust = 0.2,family = "Helvetica", face = "bold" ), 
    strip.text = element_text( size = 14, color = "black" , face = "bold"),
    legend.title = element_text(size = 14, color = "black", face = "bold"),
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.key.width = unit(0.4,"cm"),
    legend.key.height = unit(0.1,"cm"),
    legend.text = element_text(size = 14, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1),
    strip.background = element_rect(color = "black", linewidth = 1)
  ) +
  labs(fill = "DCI: ") +
  guides(fill = guide_legend(title.position = "left", nrow = 1))

# Saving plot
ggsave(file = paste0(SAVE_PATH,"fig04.png"), plot = p1 , width = 18, 
       height = 24, units = "cm", dpi = 300)

