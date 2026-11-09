# -- 0. Load and prepare data -------------------------------------------------
source('source/twc_change.R')
library(magrittr)

# -- 1. Load data and land mask -----------------------------------------------
avail_flux_change_all <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_grid_all.rds'))
masks <- pRecipe::pRecipe_masks()

# Choose one dataset, apply land mask
avail_flux_change_classes <- avail_flux_change_all[dataset == 'WEIGHTED'] %>%
  merge(masks[land_mask == 'land'], by = c("lon", "lat"))

# -- 2. Classify grid cells --------------------------------------------------
to_plot <- copy(avail_flux_change_classes)[,
                                           Conditions := factor("Unknown",
                                                                levels = c('Wetter - Accelerated', 'Drier - Accelerated',
                                                                           'Wetter - Deccelerated', 'Drier - Deccelerated', "Unknown"))
]

to_plot[flux_change > 0 & avail_change > 0,  Conditions := 'Wetter - Accelerated']
to_plot[flux_change > 0 & avail_change < 0,  Conditions := 'Drier - Accelerated']
to_plot[flux_change < 0 & avail_change > 0,  Conditions := 'Wetter - Deccelerated']
to_plot[flux_change < 0 & avail_change < 0,  Conditions := 'Drier - Deccelerated']

# -- 3. Order biomes by proportion of 'Drier' cells --------------------------
drier_perc <- to_plot[Conditions %in% c('Drier - Accelerated', 'Drier - Deccelerated'),
                      .N, by = biome_short_class][
                        , drier_prop := N / to_plot[, .N, by=biome_short_class][.SD, on="biome_short_class"]$N]
ordered_biomes <- drier_perc[order(-drier_prop), biome_short_class]
to_plot[, biome_short_class := factor(biome_short_class, levels = ordered_biomes)]

# -- 4. Plot by biome --------------------------------------------------------
ggplot(to_plot) +
  geom_bar(aes(biome_short_class, fill = Conditions), position = "fill") +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 3, 2, 4)]) +
  geom_hline(yintercept = 0.5, col = 'black', lty = 'dashed') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Proportion of Grid Cells", x = "Biome")

# -- 5. Assign latitude zones and filter small zones -------------------------
to_plot[, lat_zone := cut(lat, breaks = seq(-90, 90, by = 10), include.lowest = TRUE, right = FALSE)]
to_plot[, lat_zone := factor(lat_zone, levels = unique(lat_zone[order(as.numeric(sub("\\[(.*),.*", "\\1", lat_zone)))]))]
to_plot[, n_gridcells_zone := .N, by = lat_zone]
to_plot <- to_plot[n_gridcells_zone > 1000, ]

# -- 6. Plot by latitude zone (stacked bar) ----------------------------------
ggplot(to_plot) +
  geom_bar(aes(lat_zone, fill = Conditions), position = "fill") +
  geom_hline(yintercept = 0.5, col = 'black', lty = 'dashed') +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 3, 2, 4)]) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  labs(x = "Latitude Zone (deg)", y = "Proportion of Grid Cells") +
  coord_flip()

# -- 7. Compute and plot lines for Drier and Accelerated ---------------------
to_plot[, conditions_flux := ifelse(flux_change > 0, 'Accelerated', 'Deccelerated')]
to_plot[, conditions_availability := ifelse(avail_change < 0, 'Drier', 'Wetter')]

# Drier/Wetter
prop_drier <- to_plot[, .N, by = .(lat_zone, conditions_availability)][
  , total := sum(N), by = lat_zone][
    , prop := N / total]
prop_drier_plot <- prop_drier[conditions_availability == "Drier", .(lat_zone, prop, category = "Drier")]

# Accelerated/Deccelerated
prop_acc <- to_plot[, .N, by = .(lat_zone, conditions_flux)][
  , total := sum(N), by = lat_zone][
    , prop := N / total]
prop_acc_plot <- prop_acc[conditions_flux == "Accelerated", .(lat_zone, prop, category = "Accelerated")]

to_plot_lines <- rbind(prop_drier_plot, prop_acc_plot)

ggplot(to_plot_lines, aes(x = lat_zone, y = prop, color = category, group = category)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(x = "Latitude Zone (deg)", y = "Proportion",
       title = "Proportion by Latitude Zone") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

# -- 8. Calculate water volume changes for each grid cell --------------------
# Merge with grid cell area
if (!"area" %in% names(to_plot)) {
  grid_cell_area <- unique(avail_flux_change_all[, .(lon, lat)]) %>% grid_area()
  to_plot <- merge(to_plot, grid_cell_area, by = c("lon", "lat"))
}

to_plot[, avail_change_vol := (avail_change / 1000) * area / 1e9]    # mm to m3, then to km3
to_plot[, flux_change_vol  := (flux_change  / 1000) * area / 1e9]    # mm to m3, then to km3

# -- 9. Aggregate and plot volume changes by latitude zone -------------------
to_plot_by_lat <- to_plot[, .(
  avail_change_km3 = sum(avail_change_vol, na.rm=TRUE),
  flux_change_km3  = sum(flux_change_vol, na.rm=TRUE)
), by = lat_zone]

zone_long <- melt(to_plot_by_lat, id.vars = "lat_zone",
                  variable.name = "Metric", value.name = "Value")

ggplot(zone_long, aes(x = lat_zone, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(x = "Latitude Zone", y = "Water Volume Change (km³)",
       title = "Net Water Availability and Acceleration by Latitude Zone") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()
