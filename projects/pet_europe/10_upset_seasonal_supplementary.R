##################################
#UpsetR seasonal plots 
#################################
library(ggplot2)
library(data.table)
library(fst)
library(ComplexUpset)
library(trend)
library(ggpubr)
library(dplyr)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <-  "~/shared/data_projects/ithaca/pet_europe/figures/"

#load data
basin_classification <- read_fst(paste0(FILE_PATH, "basin_classification.fst"), as.data.table = TRUE)
seasonal_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc_seasonal.fst"), as.data.table = TRUE)

#sen slope computation
seasonal_slope_dt <- seasonal_dt[ season_year >= 1980 & season_year <= 2019,
                                  .(sen_slope = sens.slope(seasonal_value, conf.level = 0.95)[[1]]) ,
                                  by = .(basin, pet_method, season, variable)]

seasonal_slope_dt <- dcast(seasonal_slope_dt, basin + pet_method + season ~ variable, value.var = c( "sen_slope"))

#Merging slope datatable with basin types
seasonal_slope_dt <- merge(seasonal_slope_dt, basin_classification, by = "basin")

# data preparation for the upset plot 
seasonal_slope_dt[, "TWS+" := fifelse(tws > 0, 1, 0) 
                  ][, "TWS-" := fifelse(tws < 0, 1, 0)
                    ][, "pet+" := fifelse(pet > 0, 1, 0) 
                      ][, "pet-" := fifelse(pet < 0, 1, 0)
                        ][, "AET+" := fifelse(aet > 0, 1, 0) 
                          ][, "AET-" := fifelse(aet < 0, 1, 0)
                            ][, "Q+" := fifelse(q > 0, 1, 0) 
                              ][, "Q-" := fifelse(q < 0, 1, 0)
                                ][, "PRE+" := fifelse(pre > 0, 1, 0) 
                                  ][, "PRE-" := fifelse(pre < 0, 1, 0)]

# Ordering pet methods based on temperature, radiation and combinational type
pet_method_order <- c("pet_th", "pet_br", "pet_bc", "pet_od", "pet_mb", "pet_hm", "pet_hs", "pet_jh", 
                      "pet_eop", "pet_pt", "pet_pm", "pet_co2")
seasonal_slope_dt$pet_method <- factor(seasonal_slope_dt$pet_method, levels = pet_method_order)

#changing name of pet methods and hydrological components  
levels(seasonal_slope_dt$pet_method) <- c(pet_th = "TH", pet_br = "BR" , pet_bc = "BC", pet_od = "OD", 
                                 pet_mb ="MB", pet_hm= "HM", pet_hs = "HS", pet_jh = "JH", 
                                 pet_eop ="MD", pet_pt = "PT", pet_pm = "PM", pet_co2 = "CO2")

# Plotting upset plot for different hydrological components 

# Winter season plot 
p1 <- upset(seasonal_slope_dt[season == "DJF"],
            name = "Combination of hydrological cycle components",
            n_intersections = 10, 
            c("PRE-","AET-","Q-","TWS-","TWS+","Q+","AET+", "PRE+"),
            sort_intersections=FALSE,
            intersections=list(c("PRE+","AET+","Q+","TWS+"),
                               c("PRE-","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q+","TWS-"),
                               c("PRE-","AET-","Q-","TWS-"),
                               c("PRE-","AET+","Q+","TWS+"),
                               c("PRE-","AET+","Q+","TWS-"),
                               c("PRE+","AET-","Q+","TWS-"),
                               c("PRE+","AET-","Q+","TWS+")

            ),
            stripes = "white",
            base_annotations = list(
              'Annual'= (
                ggplot(mapping=aes(fill=pet_method))+
                  geom_bar(stat='count', position='fill')+
                  scale_fill_manual(name = 'Method',
                                    values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                               MB ="#FDBF6F", HM= "gold1", HS = "pink", JH = "darkturquoise", 
                                               MD ="green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
                  )+
                  geom_text(aes(label=after_stat(count)), stat = "count", position = position_fill(vjust = .5), size = 4, family = "Helvetica")+ #!!aes_percentage(relative_to='intersection')
                  ylab('Catchments')+
                  theme(text = element_text(family = "Helvetica"),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        legend.text = element_text(size = 10, color = "black", family = "Helvetica" ), 
                        legend.title = element_text(size = 12, color = "black", family = "Helvetica"),
                        panel.grid = element_line(color = "white")
                  )
              )
            ),
            themes = upset_modify_themes(list('intersections_matrix' = theme(axis.text.y = element_text(size = 12, color = "black", family = "Helvetica") ))
            ),
            width_ratio=0.1, 
            sort_sets=FALSE, 
            set_sizes = FALSE
             
) 

# Spring season plot 
p2 <- upset(seasonal_slope_dt[season == "MAM"],
            name = "Combination of hydrological cycle components",
            n_intersections = 10, 
            c("PRE-","AET-","Q-","TWS-","TWS+","Q+","AET+", "PRE+"),
            sort_intersections=FALSE,
            intersections=list(c("PRE+","AET+","Q+","TWS+"),
                               c("PRE-","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q+","TWS-"),
                               c("PRE-","AET-","Q-","TWS-"),
                               c("PRE-","AET+","Q-","TWS+"),
                               c("PRE+","AET+","Q-","TWS+"),
                               c("PRE-","AET+","Q+","TWS+"),
                               c("PRE-","AET+","Q+","TWS-"),
                               c("PRE-","AET-","Q-","TWS+")
                               
            ),
            stripes = "white",
            base_annotations = list(
              'Annual'= (
                ggplot(mapping=aes(fill=pet_method))+
                  geom_bar(stat='count', position='fill')+
                  scale_fill_manual(name = 'Method',
                                    values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                               MB ="#FDBF6F", HM= "gold1", HS = "pink", JH = "darkturquoise", 
                                               MD="green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
                  )+
                  geom_text(aes(label=after_stat(count)), stat = "count", position = position_fill(vjust = .5), size = 4, family = "Helvetica")+ #!!aes_percentage(relative_to='intersection')
                  ylab('Catchments')+
                  theme(text = element_text(family = "Helvetica"),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        legend.text = element_text(size = 10, color = "black", family = "Helvetica" ), 
                        legend.title = element_text(size = 12, color = "black", family = "Helvetica"),
                        panel.grid = element_line(color = "white")
                  )
              )
            ),
            themes = upset_modify_themes(list('intersections_matrix' = theme(axis.text.y = element_text(size = 12, color = "black", family = "Helvetica") ))
            ),
            width_ratio=0.1, 
            sort_sets=FALSE, 
            set_sizes = FALSE
          
) 

# Summer season plot 
p3 <- upset(seasonal_slope_dt[season == "JJA"],
            name = "Combination of hydrological cycle components",
            n_intersections = 10, 
            c("PRE-","AET-","Q-","TWS-","TWS+","Q+","AET+", "PRE+"),
            sort_intersections=FALSE,
            intersections=list(c("PRE+","AET+","Q+","TWS+"),
                               c("PRE-","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q+","TWS-"),
                               c("PRE-","AET-","Q-","TWS-"),
                               c("PRE-","AET-","Q-","TWS+"),
                               c("PRE+","AET+","Q-","TWS+"),
                               c("PRE-","AET-","Q+","TWS+"),
                               c("PRE-","AET-","Q+","TWS-"),
                               c("PRE+","AET-","Q-","TWS-")
                               
            ),
            stripes = "white",
            base_annotations = list(
              'Annual'= (
                ggplot(mapping=aes(fill=pet_method))+
                  geom_bar(stat='count', position='fill')+
                  scale_fill_manual(name = 'Method',
                                    values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                               MB ="#FDBF6F", HM= "gold1", HS = "pink", JH = "darkturquoise", 
                                               MD="green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
                  )+
                  geom_text(aes(label=after_stat(count)), stat = "count", position = position_fill(vjust = .5), size = 4, family = "Helvetica")+ #!!aes_percentage(relative_to='intersection')
                  ylab('Catchments')+
                  theme(text = element_text(family = "Helvetica"),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        legend.text = element_text(size = 10, color = "black", family = "Helvetica" ), 
                        legend.title = element_text(size = 12, color = "black", family = "Helvetica"),
                        panel.grid = element_line(color = "white")
                  )
              )
            ),
            themes = upset_modify_themes(list('intersections_matrix' = theme(axis.text.y = element_text(size = 12, color = "black", family = "Helvetica") ))
            ),
            width_ratio=0.1, 
            sort_sets=FALSE, 
            set_sizes = FALSE
) 

# Autumn season plot 
p4 <- upset(seasonal_slope_dt[season == "SON"],
            name = "Combination of hydrological cycle components",
            n_intersections = 10, 
            c("PRE-","AET-","Q-","TWS-","TWS+","Q+","AET+", "PRE+"),
            sort_intersections=FALSE,
            intersections=list(c("PRE+","AET+","Q+","TWS+"),
                               c("PRE-","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q-","TWS-"),
                               c("PRE+","AET+","Q+","TWS-"),
                               c("PRE-","AET-","Q-","TWS-"),
                               c("PRE-","AET+","Q-","TWS+"),
                               c("PRE-","AET+","Q+","TWS+"),
                               c("PRE+","AET-","Q-","TWS-"),
                               c("PRE+","AET-","Q+","TWS-"),
                               c("PRE-","AET+","Q+","TWS-")
                               
            ),
            stripes = "white",
            base_annotations = list(
              'Annual'= (
                ggplot(mapping=aes(fill=pet_method))+
                  geom_bar(stat='count', position='fill')+
                  scale_fill_manual(name = 'Method',
                                    values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                               MB ="#FDBF6F", HM= "gold1", HS = "pink", JH = "darkturquoise", 
                                               MD="green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
                  )+
                  geom_text(aes(label=after_stat(count)), stat = "count", position = position_fill(vjust = .5), size = 4, family = "Helvetica")+
                  ylab('Catchments')+
                  theme(text = element_text(family = "Helvetica"),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        legend.text = element_text(size = 10, color = "black", family = "Helvetica" ), 
                        legend.title = element_text(size = 12, color = "black", family = "Helvetica"),
                        panel.grid = element_line(color = "white")
                  )
              )
            ),
            themes = upset_modify_themes(list('intersections_matrix' = theme(axis.text.y = element_text(size = 12, color = "black", family = "Helvetica") ))
            ),
            width_ratio=0.1, 
            sort_sets=FALSE, 
            set_sizes = FALSE
) 

#Saving plots
ggsave(paste0(SAVE_PATH,"upset_winter_supplement_new.png"), p1, width = 15, height = 10, units = c("mm"), dpi = 300, scale = 15)
ggsave(paste0(SAVE_PATH,"upset_spring_supplement_new.png"), p2, width = 15, height = 10, units = c("mm"), dpi = 300, scale = 15)
ggsave(paste0(SAVE_PATH,"upset_summer_supplement_new.png"), p3, width = 15, height = 10, units = c("mm"), dpi = 300, scale = 15)
ggsave(paste0(SAVE_PATH,"upset_autumn_supplement_new.png"), p4, width = 15, height = 10, units = c("mm"), dpi = 300, scale = 15)

