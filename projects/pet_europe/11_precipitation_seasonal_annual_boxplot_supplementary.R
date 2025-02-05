################################################################
# Boxplot at seasonal scale   
###############################################################
# Required library 
library(ggh4x)
library(fst)
library(data.table)
library(ggplot2)
library(tidyverse)
library(trend)
library(ggpubr)
library(sf)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/figures/"

#loading data 
basin_classification <- read_fst(paste0(FILE_PATH, "basin_classification.fst"), as.data.table = TRUE)
seasonal_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc_seasonal.fst"), as.data.table = TRUE)

#sen slope computation
seasonal_slope_dt <- seasonal_dt[ season_year >= 1980 & season_year <= 2019,
                                  .(sen_slope = sens.slope(seasonal_value, conf.level = 0.95)[[1]]),
                                  by = .(basin, pet_method, season, variable)]

#Merging slope datatable with basin types
seasonal_slope_dt <- merge(seasonal_slope_dt, basin_classification, by = "basin")

# Outlier classification 

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

seasonal_slope_dt[, basin_type := as.factor(basin_type)]
seasonal_slope_dt[, outlier := is_outlier(sen_slope), by = .(pet_method, variable, season, basin_type)]

# Ordering pet methods based on temperature, radiation and combinational type
pet_method_order <- c("pet_th", "pet_br", "pet_bc", "pet_od", "pet_mb", "pet_hm", "pet_hs", "pet_jh", 
                      "pet_eop", "pet_pt", "pet_pm", "pet_co2")
seasonal_slope_dt$pet_method <- factor(seasonal_slope_dt$pet_method, levels = pet_method_order)

#changing name of pet methods and hydrological components  
levels(seasonal_slope_dt$pet_method) <- c(pet_th = "TH", pet_br = "BR" , pet_bc = "BC", pet_od = "OD", 
                                          pet_mb = "MB", pet_hm = "HM", pet_hs = "HS", pet_jh = "JH", 
                                          pet_eop = "EOP", pet_pt = "PT", pet_pm = "PM", pet_co2 = "CO2"  )
levels(seasonal_slope_dt$variable) <- c(twsc = "TWSC", pet = "PET", aet = "AET", q = "Q", tws = "TWS", pre = "PRE")

#laoding data 
basin_classification <- read_fst(paste0(FILE_PATH, "basin_classification.fst"), as.data.table = TRUE)
monthly_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc.fst"), as.data.table = TRUE)
monthly_dt[, YEAR := year(date)]

#Computing monthly to annual scale
yearly_dt <- monthly_dt[, .(value = sum(value, na.rm = TRUE)), by = .(basin, pet_method, variable, YEAR)]

#averaging the TWS by diving 12 
yearly_dt[variable == "tws", value := value/12]

#Slope agreement for period 1980 to 2019 
slope_dt <- yearly_dt[YEAR >= 1980 & YEAR <= 2019, .(sen_slope = sens.slope(value, conf.level = 0.95)[[1]]), by = .(basin, pet_method, variable)]

#Merging slope datatable with basin types
slope_dt <- merge(slope_dt, basin_classification, by = "basin")

# Outlier classification 

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

slope_dt[, basin_type := as.factor(basin_type)]
slope_dt[, outlier := is_outlier(sen_slope), by = .(pet_method, variable, basin_type)]

# Ordering pet methods based on temperature, radiation and combinational type
pet_method_order <- c("pet_th", "pet_br", "pet_bc", "pet_od", "pet_mb", "pet_hm", "pet_hs", "pet_jh", 
                      "pet_eop", "pet_pt", "pet_pm", "pet_co2")
slope_dt$pet_method <- factor(slope_dt$pet_method, levels = pet_method_order)

#changing name of pet methods and hydrological components  
levels(slope_dt$pet_method) <- c(pet_th = "TH", pet_br = "BR" , pet_bc = "BC", pet_od = "OD", 
                                 pet_mb = "MB", pet_hm = "HM", pet_hs = "HS", pet_jh = "JH", 
                                 pet_eop = "EOP", pet_pt = "PT", pet_pm = "PM", pet_co2 = "CO2"  )
levels(slope_dt$variable) <- c(twsc = "TWSC", pet = "PET", aet = "AET", q = "Q", tws = "TWS", pre = "PRE" )

pre_annual <- slope_dt[variable == "PRE" & pet_method == "HS"]
pre_annual[, season := "Annual"]
pre <- seasonal_slope_dt[variable == "PRE" & pet_method == "HS"]
pre <- rbind(pre, pre_annual)

p1 <- ggplot(data = pre[variable == "PRE"]) +
  geom_boxplot(aes( x = variable, y = sen_slope), na.rm = TRUE, outlier.shape = NA, width = 0.2, position = "dodge")+
  facet_grid(basin_type ~factor(season, levels=c('DJF','MAM','JJA','SON', "Annual")), scales = "free",  labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited")))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)+
  ylim(-2,2)+
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black",),  
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 14, color = "black", face = "bold"),
    legend.direction = "horizontal", 
    legend.text = element_text(size = 14, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.border = element_rect(color = "black", linewidth = 1),
    strip.background = element_rect(color = "black", linewidth = 1),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm"), 
    legend.position = "none"
  ) 
p1

#Saving plot  
ggsave(file = paste0(SAVE_PATH,"boxplot_pre_seasonal.png"), p1 , width = 10, height = 11, units = "in", dpi = 300)



