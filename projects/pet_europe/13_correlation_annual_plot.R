# Load the libraries
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

# Reshape data from long to wide format
slope_wide <- dcast(slope_dt, basin + pet_method + method_type + outlier + basin_type ~ variable, value.var = "sen_slope")


# Plot PET vs AET
pet_vs_aet <- ggplot(slope_wide[outlier == "FALSE" & basin_type != "water_limited"], aes(x = PET, y = AET, color = pet_method)) +
  geom_point(size = 1, alpha = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = pet_method)) +
  stat_cor(
    aes(color = pet_method, label = paste0("R = ", ..r..)), 
    label.x.npc = 0.95, 
    label.y.npc = 1, 
    hjust = 0,
    digits = 2,
    show.legend = FALSE,
    output.type = "text"
  ) +
  labs(y = expression("AET (mm/year"^2*")"), x = expression("PET (mm/year"^2*")")) +
  scale_color_manual(name = 'Method',
                     values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                MB ="#FDBF6F", HM= "gold1", HS = "deeppink1", JH = "darkturquoise", 
                                MD ="green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
  )+
  facet_grid(rows = vars(basin_type), scales = "free_y",
             labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited"))) +
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black", family = "Helvetica"),  
    axis.text.x = element_text(size = 14, color = "black", hjust = 0.5, family = "Helvetica"), 
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12, color = "black", family = "Helvetica"),
    strip.text = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.direction = "horizontal", 
    legend.text = element_text(size = 12, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.border = element_rect(color = "black", linewidth = 1),
    strip.background = element_rect(color = "black", linewidth = 1),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm"), 
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(label = NULL)))  
pet_vs_aet

ggsave(file = paste0(SAVE_PATH, "correlation_pet_annual_boxplot_supplementary.pdf"), pet_vs_aet , width = 8.5, height = 6, units = "in", dpi = 300)


# Plot PRE vs Q 
pre_vs_q <- ggplot(slope_wide[outlier == "FALSE"  & basin_type != "water_limited"], aes(x = PRE, y = Q, color = pet_method)) +
  geom_point(size = 1, alpha = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = pet_method)) +
  stat_cor(
    aes(color = pet_method, label = paste0("R = ", ..r..)), 
    label.x.npc = 0.95, 
    label.y.npc = 0.8, 
    hjust = 0,
    digits = 2,
    show.legend = FALSE,  
    output.type = "text"
  ) +
  labs(y = expression("Q (mm/year"^2*")"), x = expression("PRE (mm/year"^2*")")) +
  scale_color_manual(name = 'Method',
                     values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                MB ="#FDBF6F", HM= "gold1", HS = "deeppink1", JH = "darkturquoise", 
                                MD ="green1", PT = "brown", PM = "lightblue1", CO2 = "yellow3" ),
  )+
  facet_grid(rows = vars(basin_type), scales = "free_y",
             labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited"))) +
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.y = element_text(size = 14, color = "black", family = "Helvetica"),  
    axis.text.x = element_text(size = 14, color = "black", hjust = 0.5, family = "Helvetica"), 
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12, color = "black", family = "Helvetica"),
    strip.text = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.direction = "horizontal", 
    legend.text = element_text(size = 12, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.border = element_rect(color = "black", linewidth = 1),
    strip.background = element_rect(color = "black", linewidth = 1),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm"), 
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(label = NULL)))  

pre_vs_q

ggsave(file = paste0(SAVE_PATH, "correlation_pre_annual_boxplot_supplementary.pdf"), pre_vs_q , width = 8.5, height = 6, units = "in", dpi = 300)


