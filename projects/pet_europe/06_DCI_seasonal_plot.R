################################################################
# seasonal_dci computation at seasonal scale and spatial plot   
###############################################################
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

#load data
seasonal_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc_seasonal.fst"), as.data.table = TRUE)

#sen slope computation
seasonal_slope_dt <- seasonal_dt[ season_year >= 1980 & season_year <= 2019,
                                  .(sen_slope = sens.slope(seasonal_value, conf.level = 0.95)[[1]]),
                                  by = .(basin, pet_method, season, variable)]

# Computing seasonal_dci 
seasonal_slope_dt[, fraction := fifelse(is.na(sen_slope), 0, sen_slope/abs(sen_slope))]
#removing the NAN due to 0 slope
seasonal_slope_dt <- na.omit(seasonal_slope_dt)
seasonal_dci <- seasonal_slope_dt[, .(dci = sum(fraction)/length(fraction)), 
                                  by = .(basin, season, variable)]

shp_area_dt <- readRDS(paste0(FILE_PATH, "shapefile_area_datatable.rds"))

seasonal_dci <- merge(seasonal_dci, shp_area_dt, by = "basin", allow.cartesian = TRUE)
setorder(seasonal_dci, -area)

################################################################################
# seasonal_dci spatial plot 
################################################################################
europe <- st_read(paste0(FILE_PATH, "/europe/Europe_coastline_poly.shp"))
europe <- st_transform(europe, 4326)

# Creating the legend range column 
seasonal_dci$legend_range <- cut(seasonal_dci$dci, breaks = c(-1.0001, -0.5, 0.499999999, 1),
                                 labels = c("<= -0.5", "-0.5 — 0.5", ">= 0.5"))
levels(seasonal_dci$variable) <- c(twsc = "TWSC", pet = "PET", aet = "AET", q = "Q", 
                                   tws = "TWS", pre = "PRE")
p1 <- ggplot() +
  geom_sf(data = europe, aes()) +
  scale_x_continuous(limits = c(-9, 32.5),  breaks = c(-8, 0, 10, 20, 30)) +
  scale_y_continuous(limits = c(38, 69)) +
  geom_sf(data = seasonal_dci[variable != "TWSC" & variable != "PRE"], aes(geometry = geometry, fill = legend_range)) +
  coord_sf(crs = st_crs(4326)) +
  scale_color_manual(guide = guide_legend(title = "seasonal_dci: ", title.hjust = 0.5, 
                                          title.position = "top", label.position = "right", 
                                          direction = "horizontal", label.hjust = 0.5, nrow = 1)) +
  scale_fill_manual(
    values =  c("#4B0082" , "#009E73", "#d01c8b") #c("blue",   "darkgreen", "#7D0A0A")
  ) +
  #facet_wrap(.~factor(variable, levels = c("PET", 'AET',"Q","TWS", "TWSC", "PRE")), ncol = 2) +
  theme_bw() + 
  theme(
    text = element_text(family = "Helvetica"),    
    axis.text.y = element_text(size = 14, color = "black", family = "Helvetica", face = "bold"),  
    axis.text.x = element_text(size = 14, color = "black",family = "Helvetica", face = "bold" ), 
    strip.text = element_text( size = 14, color = "black" , face = "bold"),
    legend.title = element_text(size = 14, color = "black", face = "bold"),
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.1, "cm"),
    legend.text = element_text(size = 14, color = "black"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1),
    strip.background = element_rect(color = "black", linewidth = 1)
  ) +
  labs(fill = "DCI: ") +
  guides(fill = guide_legend(title.position = "left", nrow = 1) ) +
  facet_nested(factor(season, levels = c("DJF", "MAM", 'JJA', "SON")) ~
                 factor(variable, levels = c("PET", 'AET', "Q", "TWS",
                                             "TWSC", "PRE")))
#Saving plot 
ggsave(file = paste0(SAVE_PATH, "fig05.png"), plot = p1 , width = 28, 
       height = 32, units = "cm", dpi = 300)



