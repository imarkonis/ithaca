library(viridis)    # for better color scales
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(DescTools)

dataset_weights <- readRDS(paste0(PATH_OUTPUT, 'dataset_pair_weights.rds'))

# 1. Standardized IQR for raw dataset agreement
raw_agreement <- avail_flux_change[, .(
  raw_iqr = IQR(avail_change, na.rm = TRUE),
  raw_mean = mean(avail_change, na.rm = TRUE)
), by = .(lon, lat)][
  , raw_siqr := raw_iqr / abs(raw_mean)  # use abs to avoid division sign flips
]

# 2. Standardized IQR for weighted dataset agreement
weighted_agreement <- dataset_weights[, .(
  weighted_iqr = IQR(weight, na.rm = TRUE),
  weighted_mean = mean(weight, na.rm = TRUE)
), by = .(lon, lat)][
  , weighted_siqr := weighted_iqr / abs(weighted_mean)
]

# 3. Merge and compare
agreement_comparison <- merge(raw_agreement[, .(lon, lat, raw_siqr)],
                              weighted_agreement[, .(lon, lat, weighted_siqr)],
                              by = c("lon", "lat"))

# 4. Delta of standardised IQR
agreement_comparison[, delta_siqr := weighted_siqr - raw_siqr]




# Convert to sf object for mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

# Optional: crop lat-lon range
plot_data <- agreement_comparison[!is.na(delta_siqr)]

# Filter out NA values for plotting
plot_data <- agreement_comparison[!is.na(delta_siqr)]
plot_data[, delta_siqr_clipped := pmax(pmin(delta_siqr, 3), -3)]

# Plot
ggplot() +
  geom_tile(data = plot_data, aes(x = lon, y = lat, fill = delta_siqr_clipped)) +
  geom_sf(data = world, fill = NA, color = "gray40", size = 0.2) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    limits = c(-2, 2),
    oob = scales::squish,  # Ensures values outside limits are squished to boundary color
    name = "ΔsIQR\n(Weighted - Raw)"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(title = "Clipped ΔsIQR Between -2 and 2",
       subtitle = "Values < -2 or > 2 share same color as -2 / 2") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank())
