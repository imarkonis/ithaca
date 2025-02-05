#############################################################
#Boxplot at annual scale 
#############################################################
library(fst)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(trend)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/figures/"

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

# Assuming your data.table is named dt
slope_dt[, method_type := fifelse(pet_method %in% c("pet_eop"), "rad",
                                  fifelse(pet_method %in% c("pet_pm", "pet_co2", "pet_pt"), "comb", "temp"))]


# Outlier classification 

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

slope_dt[, basin_type := as.factor(basin_type)]
slope_dt[, outlier := is_outlier(sen_slope), by = .(pet_method, variable, basin_type)]

# Ordering pet methods based on temperature, radiation and combinational type
pet_method_order <- c("pet_th","pet_bc", "pet_hm", "pet_od", "pet_mb","pet_br", "pet_hs", "pet_jh", 
                      "pet_eop", "pet_pt", "pet_pm", "pet_co2")
slope_dt$pet_method <- factor(slope_dt$pet_method, levels = pet_method_order)

#changing name of pet methods and hydrological components  
levels(slope_dt$pet_method) <- c(pet_th = "TH", pet_bc = "BC", pet_hm = "HM", pet_od = "OD", 
                                 pet_mb = "MB", pet_br = "BR" , pet_hs = "HS", pet_jh = "JH", 
                                 pet_eop = "MD", pet_pt = "PT", pet_pm = "PM", pet_co2 = "CO2"  )
levels(slope_dt$variable) <- c(twsc = "TWSC", pet = "PET", aet = "AET", q = "Q", tws = "TWS", pre = "PRE" )

##############################################
#Boxplots 
#############################################
p1 <- ggplot(data = slope_dt[variable == "PET" & outlier == "FALSE"]) +
  geom_boxplot(aes( y = sen_slope, x = pet_method, group = pet_method, fill = method_type), na.rm = TRUE, outlier.shape = NA, width = 0.5, position = position_dodge2(width = 0.3)) +
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  facet_grid(rows = vars(basin_type),cols = vars(variable), scales = "free_y",
             labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited")))+
  scale_fill_discrete(labels = c(temp = "Temperature-Based", rad = "Radiation-Based", comb = "Combinational"))+
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black",),  
    axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1, family = "Helvetica", face = "bold" ), 
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

p2 <- ggplot(data = slope_dt[variable == "AET" & outlier == "FALSE"]) +
  geom_boxplot(aes( y = sen_slope, x = pet_method, group = pet_method, fill = method_type), na.rm = TRUE, outlier.shape = NA, width = 0.5, position = position_dodge2(width = 0.3)) +
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  facet_grid(rows = vars(basin_type),cols = vars(variable), scales = "free_y",
             labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited")))+
  scale_fill_discrete(labels = c(temp = "Temperature-Based", rad = "Radiation-Based", comb = "Combinational"))+
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black",),  
    axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1, family = "Helvetica", face = "bold" ), 
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

p3 <- ggplot(data = slope_dt[variable == "Q" & outlier == "FALSE"]) +
  geom_boxplot(aes( y = sen_slope, x = pet_method, group = pet_method, fill = method_type), na.rm = TRUE, outlier.shape = NA, width = 0.5, position = position_dodge2(width = 0.3)) +
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  facet_grid(rows = vars(basin_type),cols = vars(variable), scales = "free_y",
             labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited")))+
  scale_fill_discrete(labels = c(temp = "Temperature-Based", rad = "Radiation-Based", comb = "Combinational"))+
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black",),  
    axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1, family = "Helvetica", face = "bold" ), 
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

p4 <- ggplot(data = slope_dt[variable == "TWS" & outlier == "FALSE"]) +
  geom_boxplot(aes( y = sen_slope, x = pet_method, group = pet_method, fill = method_type), na.rm = TRUE, outlier.shape = NA, width = 0.5, position = position_dodge2(width = 0.3)) +
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  facet_grid(rows = vars(basin_type),cols = vars(variable), scales = "free_y",
             labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited")))+
  scale_fill_discrete(labels = c(temp = "Temperature-Based", rad = "Radiation-Based", comb = "Combinational"))+
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black",),  
    axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1, family = "Helvetica", face = "bold" ), 
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
  )+
  scale_y_continuous(
    breaks = function(x) seq(floor(min(x)), ceiling(max(x)), length.out = 6)
  )

legend <- get_legend(ggplot(data = slope_dt[variable == "PET" & outlier == "FALSE"]) +
                       geom_boxplot(aes(y = sen_slope, x = pet_method, group = pet_method, fill = method_type), na.rm = TRUE, outlier.shape = NA, width = 0.5, position = position_dodge2(width = 0.3)) +
                       labs(y = NULL, x = NULL) +
                       geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
                       facet_grid(rows = vars(basin_type), cols = vars(variable), scales = "free_y",
                                  labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited"))) +
                       scale_fill_discrete(labels = c(temp = "Temperature-Based", rad = "Radiation-Based", comb = "Combinational"))+
                       theme_bw() +
                       theme(
                         text = element_text(family = "Helvetica"),
                         axis.text.y = element_text(size = 14, color = "black",),  
                         axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1, family = "Helvetica", face = "bold" ), 
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
                       ) +
                       guides(fill = guide_legend(title = "Type: ", reverse=TRUE, title.position = "left", nrow = 1, label.position = "right", keywidth = unit(1, "cm")))
) 


#arranging the plots in grid
p6 <- grid.arrange(p1, p2, p3, p4, legend, ncol=2, nrow = 3, 
                   layout_matrix = rbind(c(1,2), c(3,4), c(5,5)),
                   widths = c(2.7, 2.7), heights = c(2.5, 2.5,0.2))

#Saving plot  
ggsave(file = paste0(SAVE_PATH, "boxplot.png"), p6, width = 10, height = 11, units = "in", dpi = 300)


