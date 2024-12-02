##################################
#UpsetR annual plot 
#################################
library(ggplot2)
library(data.table)
library(fst)
library(ComplexUpset)
library(trend)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/figures/"

#loading data
basin_classification <- read_fst(paste0(FILE_PATH, "basin_classification.fst"), as.data.table = TRUE)
monthly_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc.fst"), as.data.table = TRUE)
monthly_dt[, YEAR := year(date)]

#computing annual time series
yearly_dt <- monthly_dt[, .(value = sum(value, na.rm = TRUE)), by = .(basin, pet_method, variable, YEAR)]

#averaging the TWS by diving 12 
yearly_dt[variable == "tws", value := value/12]

#slope computation
slope_dt <- yearly_dt[, .(sen_slope = sens.slope(value, conf.level = 0.95)[[1]]) , by = .(basin, pet_method,variable)]

slope_dt <- dcast(slope_dt, basin + pet_method ~ variable, value.var = c( "sen_slope"))

#Merging slope datatable with basin types
slope_dt <- merge(slope_dt, basin_classification, by = "basin")

# data preparation for the upset plot 
slope_dt[, "TWS+" := fifelse(tws > 0, 1, 0) 
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
slope_dt$pet_method <- factor(slope_dt$pet_method, levels = pet_method_order)

#changing name of pet methods and hydrological components  
levels(slope_dt$pet_method) <- c(pet_th = "TH", pet_br = "BR" , pet_bc = "BC", pet_od = "OD", 
                                 pet_mb = "MB", pet_hm = "HM", pet_hs = "HS", pet_jh = "JH", 
                                 pet_eop ="MD", pet_pt = "PT", pet_pm = "PM", pet_co2 = "CO2"  )

# Plotting upset plot for different hydrological components 
p1 <- upset(slope_dt,
            n_intersections = 10, 
            c("PRE-","AET-","Q-","TWS-","TWS+","Q+","AET+", "PRE+"),
            stripes = "white",
            name = "Combination of hydrological cycle components",
            base_annotations = list(
              'Annual'= (
                ggplot(mapping=aes(fill=pet_method))+
                  geom_bar(stat='count', position='fill')+
                  scale_fill_manual(name = 'Method',
                                    values = c(TH = "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                               MB = "#FDBF6F", HM = "gold1", HS = "pink", JH = "darkturquoise", 
                                               MD = "green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
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
            width_ratio = 0.1, 
            sort_sets = FALSE, 
            set_sizes = FALSE
) 
p1

# Saving plot 
ggsave(paste0(SAVE_PATH,"fig06.png"), p1, width = 15, height = 10, units = c("mm"), dpi = 300, scale = 15)
