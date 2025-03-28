# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# plot global precip trend uncertinities using IPCC regions as hexagons ####

source('source/changing_prec.R')

# read the input datasets #### -------------------------------------------------
uncert <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_b_ipcc_uncertainty_bootstrap.rds"))
ipcc_hexagon <- read.csv(paste0(PATH_IPCC_data,"/gloabl_ipcc_ref_hexagons.csv")) #don't use fread


# preprocess --------------------------------------------------------------

ipcc_uncert <- uncert[trend == "uncertain"]
ipcc_uncert[, ipcc_fraction_percent := round(ipcc_fraction * 100, 0)]
setnames(ipcc_uncert, "IPCC_ref_region", "Acronym")

setDT(ipcc_hexagon)
data <- ipcc_hexagon[ipcc_uncert, on = 'Acronym']

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


data_acron <- data[ipcc_fraction_percent < 20]

# plot --------------------------------------------------------------------

summary(data)
hist(data$ipcc_fraction_percent)

ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = ipcc_fraction_percent, group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym), size = 4, color = "White") +
  geom_text(data = data_acron, aes(V1, V2, label = Acronym), size = 4, color = "black") +
  coord_equal() + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(10, 20, 30, 40, 50), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Area fraction (%)") + 
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
  ggtitle("Trend direction is uncertain")+
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill=guide_coloursteps(title.position="top"))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_i_map_global_trend_uncert_by_ipcc_bootstrap.png"),
       width = 8, height = 7, units = "in", dpi = 600)
