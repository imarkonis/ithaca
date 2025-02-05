################################################
# Introduction plot 
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

#Merging aridity datatable with basin types, area and shapefiles 
aridity_dt <- merge(aridity_dt, basin_classification, by = "basin", allow.cartesian = TRUE)
aridity_dt <- merge(aridity_dt, shp_area_dt, by = "basin", allow.cartesian = TRUE)
setorder(aridity_dt, -area)



europe_shp <- st_read(paste0(FILE_PATH, "/europe/Europe_coastline_poly.shp"))
europe_shp <- st_transform(europe_shp, 4326)

#Spatial plot of catchments based on its classification 
p1 <- ggplot() +
  geom_sf(data = europe_shp, aes())+
  scale_x_continuous(limits = c(-9, 32.5), breaks = c(-8, 0, 10, 20, 30)) +
  scale_y_continuous(limits = c(38, 69))+
  geom_sf(data = aridity_dt, aes(geometry = geometry, fill = basin_type), color = "#222222")+
  geom_sf(data = aridity_dt[(basin == "6731601"| basin == "6974360" |basin == "7002004")], aes(geometry = geometry, fill = basin_type), color = "black", lwd = 1) +
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

# Plotting representative catchment in budyko framework
p2 <- ggplot(data = aridity_dt[(basin == "6731601" | basin == "6974360" | basin == "7002004")])  +
  geom_point(aes(x = aridity_idx, y = evaporative_idx, group = basin), size = 2) +
  geom_smooth(aes(x = aridity_idx, y = evaporative_idx, group = basin, color = basin_type), 
              se = FALSE, linetype = "solid", linewidth = 2) +
  labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [AET/P]") + 
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1) +
  annotate("segment", x = 1, xend = 1, y = 0, yend = 1, colour = "red") +
  annotate("segment", x = 1, xend = 2.40, y = 1, yend = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(0, 0.1)), breaks = c(0.25, 0.5, 0.75, 1)) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_color_manual(
    values = c("energy_limited" = "#A2C523", "mixed" = "#B8860B", "water_limited" = "#5CC5EF"),
    labels = c("energy_limited" = "Energy-Limited", "mixed" = "Mixed", "water_limited" = "Water-Limited")
  )+
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.text.x = element_text(size = 18, color = "black"), 
    strip.text = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 18, color = "black"), 
    axis.title.y = element_text(size = 18, color = "black"),
    legend.title = element_blank(), 
    legend.direction = "vertical", 
    legend.position = c(0.75, 0.15),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.1, "cm"),
    legend.text = element_text(size = 18),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1),
    axis.ticks.length = unit(-0.15, "cm")
  )
  
p2

# Plotting representative basins annual time series, plot p3

#average of total water storage(tws)
yearly_dt[variable == "tws", value := value/12]
yearly_dt <- merge(yearly_dt, basin_classification, by = "basin")

# Changing names and creating order 
yearly_dt[, pet_method := as.factor(pet_method) ]
pet_method_order <- c("pet_th", "pet_br", "pet_bc", "pet_od", "pet_mb", "pet_hm", "pet_hs", "pet_jh", 
                      "pet_eop", "pet_pt", "pet_pm", "pet_co2")

# Apply the desired order to the pet_method variable
yearly_dt$pet_method <- factor(yearly_dt$pet_method, levels = pet_method_order)

#Changing name of pet methods and hydrological components 
levels(yearly_dt$pet_method) <- c(pet_th = "TH", pet_br = "BR" , pet_bc = "BC",
                                  pet_od = "OD", pet_mb ="MB", pet_hm= "HM",
                                  pet_hs = "HS", pet_jh = "JH", pet_eop ="MD",
                                  pet_pt = "PT", pet_pm = "PM", pet_co2 = "CO2")


p3 <- ggplot(yearly_dt[(basin == "6731601" | basin == "6974360" | basin == "7002004") & variable != "pre" & variable != "twsc"])+
  geom_line( aes(x= YEAR, y= value, colour = pet_method), linewidth = 0.5)+
  scale_x_continuous(breaks = c(1981, 1990, 2000, 2010, 2019))+
  facet_grid(variable ~ basin_type ,scales = "free_y" ,
             labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited"), 
                        variable = c(pet = "PET", aet = "AET", q = "Q", tws = "TWS")
                        )
             ) +
  scale_color_manual(
  values = c(TH = "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
               MB ="#FDBF6F", HM= "gold1", HS = "deeppink1", JH = "darkturquoise", 
               MD ="green1", PT = "brown", PM = "blue1", CO2 = "yellow3"  ))+
  labs(x= NULL, y = NULL, colour = "Method:")+  
  theme_bw()+
  theme(text = element_text(family = "Helvetica"),  
        legend.position = "bottom", 
        legend.title = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"),  
        axis.text.x = element_text(size = 15, color = "black"), 
        strip.text = element_text( size = 18, color = "black"),
        axis.text = element_text(size = 18, color = "black"),
        #panel.grid = element_blank(), 
        panel.border = element_rect(color = "black", linewidth = 2),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.text = element_text(size = 18),
        strip.background = element_rect(color = "black", linewidth = 2)
  ) +
  guides(colour = guide_legend(title.position = "left", nrow = 1, label.position = "top", keywidth = unit(2, "cm")),)

p3

#adjusting width of p1 and p2
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g1_grob_widths <- g1$widths
g2_grob_widths <- g2$widths
maxWidth <- unit.pmax(g1_grob_widths, g2_grob_widths)
g1$widths <- maxWidth 
g2$widths <- maxWidth

layout <- rbind(c(1,2),
                c(3,3))
# plotting p1,p2 and p3 in grid 
p4 <- grid.arrange(arrangeGrob(g1, left = textGrob("a)",gp = gpar(fontsize = 25), x = unit(1, "npc"), 
                                                   y = unit(.95, "npc"))) ,
                   arrangeGrob(g2, left = textGrob("b)", gp = gpar(fontsize = 25), x = unit(1, "npc"), 
                                                   y = unit(.95, "npc"))), 
                   arrangeGrob(g3, left = textGrob("c)", gp = gpar(fontsize = 25), x = unit(1, "npc"), 
                                                   y = unit(.98, "npc"))), layout_matrix=layout)

#saving plot
ggsave(paste0(SAVE_PATH, "fig01.png"), p4,  width = 15, height = 15, 
       units = "in", dpi = 300)
