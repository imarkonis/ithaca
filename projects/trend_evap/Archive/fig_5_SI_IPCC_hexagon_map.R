source('source/evap_trend.R')

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_trend_bootstrap.rds"))  
data_trend <- data_trend[!is.na(IPCC_ref_region)] 
data_trend_Q <- data_trend[, .(Q25 = quantile(slope, 0.25), Q75 = quantile(slope, 0.75)), IPCC_ref_region]
data_trend_Q[, Q_fold := abs(Q75)/abs(Q25)]
data_trend_Q[abs(Q25) > abs(Q75), Q_fold := abs(Q25)/abs(Q75)]
data_trend_Q[, sign := "same"]
data_trend_Q[Q75/Q25 < 0, sign := "different"]

data_trend_Q[Q_fold > 3.2 & sign == "different", problem := "High variety & different sign"] 

data_trend_Q[Q_fold > 3.2 & sign == "same", problem := "High variety & same sign"] 

data_trend_Q[Q_fold <= 3.2 & sign == "different" & abs(Q25) > 0.5 & abs(Q75) > 0.5, problem := "Low variety & different sign & large trend"] 

data_trend_Q[Q_fold <= 3.2 & sign == "same", problem := "Low variety & same sign"] 

data_trend_Q[Q_fold <= 3.2 & sign == "different" & (abs(Q25) < 0.5 | abs(Q75) < 0.5), problem := "Low variety & different sign & small trend"] 


# read the input datasets #### -------------------------------------------------
ipcc_data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_uncertainty_bootstrap.rds"))
ipcc_hexagon <- read.csv(paste0(PATH_IPCC_data,"/gloabl_ipcc_ref_hexagons.csv")) #don't use fread


# preprocess --------------------------------------------------------------
setnames(data_trend_Q, "IPCC_ref_region", "Acronym")

setDT(ipcc_hexagon)
data <- ipcc_hexagon[data_trend_Q, on = 'Acronym']


# Identify the rows corresponding to Madagascar, NAU, CAU, EAU, and SAU hexagons
med_rows <- which(data$Acronym %in% c("NAU", "CAU", "EAU", "SAU"))
med_rows_nz <- which(data$Acronym == "NZ")
med_rows_mdg <- which(data$Acronym == "MDG")
med_rows_gic <- which(data$Acronym == "GIC")

# Define the amount by which you want to shift leftward

shift_lon_gic <- 7  # You can adjust this value based on your preference
shift_lat_gic <- -4

data$long[med_rows_gic] <- data$long[med_rows_gic] - shift_lon_gic
data$lat[med_rows_gic] <- data$lat[med_rows_gic] - shift_lat_gic
data$V1[med_rows_gic] <- data$V1[med_rows_gic] - shift_lon_gic
data$V2[med_rows_gic] <- data$V2[med_rows_gic] - shift_lat_gic

shift_lon_mdg <- 7  # You can adjust this value based on your preference
shift_lat_mdg <- 3

# Shift the longitude (long) values for Madagascar hexagon
data$long[med_rows_mdg] <- data$long[med_rows_mdg] - shift_lon_mdg
data$lat[med_rows_mdg] <- data$lat[med_rows_mdg] - shift_lat_mdg
data$V1[med_rows_mdg] <- data$V1[med_rows_mdg] - shift_lon_mdg
data$V2[med_rows_mdg] <- data$V2[med_rows_mdg] - shift_lat_mdg
# Define the amount by which you want to shift leftward
shift_lon_amount <- 5  
shift_lat_amount <- 12

shift_lon_amount_nz <- 10 
shift_lat_amount_nz <- 9

# Shift the longitude (long) and latitude (lat) values for the specified hexagons
data$long[med_rows] <- data$long[med_rows] + shift_lon_amount
data$lat[med_rows] <- data$lat[med_rows] + shift_lat_amount
data$V1[med_rows] <- data$V1[med_rows] + shift_lon_amount
data$V2[med_rows] <- data$V2[med_rows] + shift_lat_amount

data$long[med_rows_nz] <- data$long[med_rows_nz] + shift_lon_amount_nz
data$lat[med_rows_nz] <- data$lat[med_rows_nz] + shift_lat_amount_nz
data$V1[med_rows_nz] <- data$V1[med_rows_nz] + shift_lon_amount_nz
data$V2[med_rows_nz] <- data$V2[med_rows_nz] + shift_lat_amount_nz


data_acron_pos <- data[ipcc_fraction_percent < 20]
data_acron_neg <- data[ipcc_fraction_percent_neg < 20]

cols_problem <- c("#330000","orange", "darkred", "lightblue", "darkblue")

# plot #### 

ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = problem, group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym), size = 4, color = "White") +
  coord_equal() + 
  scale_fill_manual(values = cols_problem) + 
  labs(x = NULL, y = NULL, fill = "") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.3, size = 24, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25,"cm"),
        legend.text = element_text(size = 16), 
        legend.title = element_text(hjust = 0.5, size = 16),
        legend.justification = "center") +
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "ipcc_hexagon_problems.png"),
       width = 15, height = 7, units = "in", dpi = 600)



ggplot(data) +
  geom_scatterpie(aes(x = long, y = lat, fill = problem, group = group), cols = LETTERS[1:5])
