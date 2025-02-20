##################################
#UpsetR annual plot 
#################################
# Need to install updated ggplot package for plot
install.packages("ggplot2")

library(ggplot2)
library(data.table)
library(fst)
library(ComplexUpset)
library(trend)
library(ggpubr)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/figures/"
SAVE_PATH_REV <- "~/shared/data_projects/ithaca/pet_europe/figures/revision/"

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

slope_dt[(pre > 0 & aet > 0 & q > 0 & tws > 0), comb:= "a"]
slope_dt[(pre < 0 & aet > 0 & q < 0 & tws < 0), comb:= "b"]
slope_dt[(pre > 0 & aet > 0 & q < 0 & tws < 0), comb:= "c"]
slope_dt[(pre > 0 & aet > 0 & q > 0 & tws < 0), comb:= "d"]
slope_dt[(pre < 0 & aet < 0 & q < 0 & tws < 0), comb:= "e"]
slope_dt[(pre < 0 & aet < 0 & q < 0 & tws > 0), comb:= "f"]
slope_dt[(pre < 0 & aet > 0 & q < 0 & tws > 0), comb:= "g"]
slope_dt[(pre > 0 & aet > 0 & q < 0 & tws > 0), comb:= "h"]
slope_dt[(pre < 0 & aet > 0 & q > 0 & tws > 0), comb:= "i"]
slope_dt[(pre < 0 & aet < 0 & q > 0 & tws > 0), comb:= "j"]

slope_combinations <- slope_dt[,.SD[sample(.N, min(1,.N))], by = comb]
slope_combinations <- copy(slope_combinations[!is.na(comb)])
slope_combinations <- slope_combinations[slope_combinations[, order(comb)]]
slope_combinations[`TWS+` == 1, TWS := "+"] 
slope_combinations[`PRE+` == 1, PRE := "+"] 
slope_combinations[`Q+` == 1, Q := "+"] 
slope_combinations[`AET+` == 1, AET := "+"] 
slope_combinations[is.na(slope_combinations)] <- "-"
slope_combinations <- subset(slope_combinations, select = c("comb", "TWS", "PRE", "Q", "AET"))
slope_combinations <- melt(slope_combinations, id = "comb")

slope_dt_summary <- slope_dt[, .(comb_count = .N), .(comb, pet_method)]
slope_dt_summary <- copy(slope_dt_summary[!is.na(comb)])
slope_dt_summary <- slope_dt_summary[slope_dt_summary[, order(pet_method)]]
slope_dt_summary[, total_count := sum(comb_count), .(comb)]
slope_dt_summary[, proportional_count := comb_count/total_count]
slope_dt_summary[, label_pos := cumsum(proportional_count), .(comb)] 
slope_dt_summary[, label_pos := abs(label_pos-1)] 
slope_dt_summary[, label_pos := label_pos + proportional_count/2] 

# plotting

plot_comb_table <- ggplot(slope_combinations)+
  geom_tile(aes(x = comb, y = variable, fill = value), color = "white", lwd = 0.8, linetype = 1)+
  theme_minimal()+
  scale_fill_manual(values = c("royalblue1", "firebrick1"))+
  geom_text(aes(label = value, x = comb, y = variable), size = 4, family = "Helvetica")+
  labs(x = "", y = "")+
  theme(axis.text.x = element_blank(),
        legend.position = "none")
  

plot_comb <- ggplot(slope_dt_summary)+
  geom_bar(aes(y = comb_count, x = comb), stat = "identity")+
  theme_minimal()+
  labs(y = "Catchment count", x = '')+
  theme(axis.text.x = element_blank(),
        legend.position = "none")

plot_prop_comb <- ggplot(slope_dt_summary, aes(x = comb, y = proportional_count))+
  geom_bar(aes(fill = pet_method), stat = "identity")+
  theme_minimal()+
  scale_fill_manual(name = 'Method',
                    values = c(TH = "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                               MB = "#FDBF6F", HM = "gold1", HS = "pink", JH = "darkturquoise", 
                               MD = "green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
  )+
  geom_text(aes(label = comb_count, x = comb, y = label_pos), size = 4, family = "Helvetica")+
  labs(y = "Catchments", x = '')+
  theme(axis.text = element_blank())

# ggarrange
ggarrange(plot_comb_table, plot_comb, plot_prop_comb, nrow = 3, align = "hv", heights = c(0.5, 1, 2))

# Saving plot 
ggsave(paste0(SAVE_PATH_REV,"fig06.png"), width = 15, height = 20, units = c("mm"), dpi = 300, scale = 15)
