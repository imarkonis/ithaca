# plot global evapo trend uncertinities using IPCC regions as hexagons ####

source('source/evap_trend.R')

# read the input datasets #### -------------------------------------------------
uncert <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_uncertainty.rds"))
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
                    breaks = c(10, 20, 30, 40, 50, 60), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "IPCC fraction (%)") + 
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

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_global_trend_uncert_by_ipcc.png"),
       width = 8, height = 7, units = "in", dpi = 600)

############################################

#extra trials with coclors


library(dplyr)

# Define breaks for binning
breaks <- c(0, 20, 40, 60, 80, 100)

# Discretize ipcc_fraction_percent into bins
data <- mutate(data, ipcc_bin = cut(ipcc_fraction_percent, breaks = breaks))

c("#543005", "#8c510a", "#bf812d", "#e0e0e0")
# Manually specify colors for each bin
bin_colors <- c("#FFC281", "#bf812d",  "#8c510a", "#7F5619", "#543005","#4D3000")

# Plot with manual color scale
ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = ipcc_bin, group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym), size = 2, color = "White") +
  coord_equal() + 
  scale_fill_manual(values = bin_colors) + 
  labs(x = NULL, y = NULL, fill = "IPCC fraction (%)") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25, "cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(), 
        panel.border = element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill = guide_coloursteps(title.position = "top"))


library(viridis)

# Define breaks for binning
breaks <- c(0, 20, 40, 60, 80, 100)

# Discretize ipcc_fraction_percent into bins
data <- mutate(data, ipcc_bin = cut(ipcc_fraction_percent, breaks = breaks))

# Use Viridis color palette
bin_colors <- viridis_pal(option = "D", direction = -1)(length(unique(data$ipcc_bin)))

# Plot with Viridis color scale
ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = ipcc_bin, group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym), size = 2, color = "White") +
  coord_equal() + 
  scale_fill_manual(values = bin_colors) + 
  labs(x = NULL, y = NULL, fill = "IPCC fraction (%)") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25, "cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(), 
        panel.border = element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill = guide_coloursteps(title.position = "top"))


library(viridis)

library(viridis)

# Define breaks for binning
breaks <- c(0, 10, 20, 40, 60, 80, 100)

# Discretize ipcc_fraction_percent into bins
data <- mutate(data, ipcc_bin = cut(ipcc_fraction_percent, breaks = breaks))

# Use Viridis color palette with modified yellow color
bin_colors <- viridis_pal(option = "G", direction = -1)(length(unique(data$ipcc_bin)))

# Plot with modified Viridis color scale
ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = ipcc_bin, group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym), size = 2, color = "White") +
  coord_equal() + 
  scale_fill_manual(values = bin_colors) + 
  labs(x = NULL, y = NULL, fill = "IPCC fraction (%)") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25, "cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(), 
        panel.border = element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill = guide_coloursteps(title.position = "top"))


# Define a custom color palette
custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

# Plot with the custom color palette
ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = ipcc_fraction_percent, group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym), size = 2, color = "White") +
  coord_equal() + 
  scale_fill_gradientn(colors = custom_palette,
                       breaks = c(10, 20, 30, 40, 50, 60),
                       labels = scales::percent,
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                                              title.position = "top")) +
  labs(x = NULL, y = NULL, fill = "IPCC fraction (%)") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25, "cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(), 
        panel.border = element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill = guide_coloursteps(title.position = "top"))


library(scales)
base_colors <- c("#543005", "#8c510a", "#bf812d")

# Number of shades per base color
num_shades <- 5

# Create a function to generate shades
generate_shades <- function(base_color, num_shades) {
  palette_func <- colorRampPalette(c("white", base_color))
  palette_func(num_shades)
}

# Generate shades for each base color
shades <- lapply(base_colors, generate_shades, num_shades)

# Plot the shades
plot(1:length(shades[[1]]), rep(1, length(shades[[1]])), pch = 15, col = shades[[1]], cex = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n")


# Define the base colors
base_colors <- c("#543005", "#8c510a", "#bf812d")

# Define lighter and darker shades manually
lighter_shades <- c("#f7e9d0", "#f7d9aa", "#f7e4cb")
darker_shades <- c("#271a05", "#4d3c0a", "#80642d")

# Combine the shades
shades <- c(lighter_shades, base_colors, darker_shades)

# Plot the shades
plot(1:length(shades), rep(1, length(shades)), pch = 15, col = shades, cex = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n")


