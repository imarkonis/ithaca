# -- 0. Load and prepare data -------------------------------------------------
source('source/twc_change.R')

library(data.table)
library(ggplot2)
library(magrittr)

M2_TO_KM2 <- 1e-6

# -- 1. Load data and land mask -----------------------------------------------
avail_flux_change_all <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_grid_all.rds'))
masks <- pRecipe::pRecipe_masks()

# -- 2. Apply land mask and join biome info ----------------------------------
avail_flux_change_classes <- merge(
  avail_flux_change_all,
  masks[land_mask == 'land', .(lon, lat, land_mask, biome_class, biome_short_class, ipcc_short_region)],
  by = c("lon", "lat")
)

# -- 3. Classify grid cells ---------------------------------------------------
avail_flux_change_classes[, Conditions := NULL]
avail_flux_change_classes[flux_change > 0 & avail_change > 0,  Conditions := 'Wetter - Accelerated']
avail_flux_change_classes[flux_change > 0 & avail_change < 0,  Conditions := 'Drier - Accelerated']
avail_flux_change_classes[flux_change < 0 & avail_change > 0,  Conditions := 'Wetter - Deccelerated']
avail_flux_change_classes[flux_change < 0 & avail_change < 0,  Conditions := 'Drier - Deccelerated']

# -- 4. Assign latitude zones -------------------------------------------------
avail_flux_change_classes[, lat_zone := cut(lat, breaks = seq(-90, 90, by = 10), include.lowest = TRUE, right = FALSE)]
avail_flux_change_classes[, lat_zone := factor(lat_zone, levels = unique(lat_zone[order(as.numeric(sub("\\[(.*),.*", "\\1", lat_zone)))]))]
avail_flux_change_classes[, n_gridcells_zone := .N, by = .(dataset, lat_zone)]
avail_flux_change_classes <- avail_flux_change_classes[n_gridcells_zone > 1000, ]

# -- 5. Add area if not present -----------------------------------------------
if (!"area" %in% names(avail_flux_change_classes)) {
  grid_cell_area <- unique(avail_flux_change_all[, .(lon, lat)]) %>% grid_area()
  avail_flux_change_classes <- merge(avail_flux_change_classes, grid_cell_area, by = c("lon", "lat"))
}

# -- 6. Plot by biome: all datasets, faceted ----------------------------------
# (Order by total 'Drier' proportion globally, for consistent legend)
drier_perc <- avail_flux_change_classes[
  Conditions %in% c('Drier - Accelerated', 'Drier - Deccelerated'),
  .N, by = biome_short_class
][
  , drier_prop := N / avail_flux_change_classes[, .N, by=biome_short_class][.SD, on="biome_short_class"]$N
]
ordered_biomes <- drier_perc[order(-drier_prop), biome_short_class]
avail_flux_change_classes[, biome_short_class := factor(biome_short_class, levels = ordered_biomes)]

ggplot(avail_flux_change_classes) +
  geom_bar(aes(dataset, fill = Conditions), position = "fill") +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 3, 2, 4)]) +
  geom_hline(yintercept = 0.5, col = 'black', lty = 'dashed') +
  theme_minimal() +
  facet_wrap(~ biome_short_class, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Proportion of Grid Cells", x = "Biome")

# -- 7. Stacked bar by latitude zone: all datasets, faceted -------------------
ggplot(avail_flux_change_classes) +
  geom_bar(aes(lat_zone, fill = Conditions), position = "fill") +
  geom_hline(yintercept = 0.5, col = 'black', lty = 'dashed') +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 3, 2, 4)]) +
  theme_minimal() +
  facet_wrap(~ dataset, ncol = 2) +
  theme(axis.text.y = element_text(size = 10)) +
  labs(x = "Latitude Zone (deg)", y = "Proportion of Grid Cells") +
  coord_flip()

# -- 8. Drier and Accelerated lines: all datasets, faceted --------------------
avail_flux_change_classes[, conditions_flux := ifelse(flux_change > 0, 'Accelerated', 'Deccelerated')]
avail_flux_change_classes[, conditions_availability := ifelse(avail_change < 0, 'Drier', 'Wetter')]

# Calculate proportion for each group
prop_lines <- rbind(
  avail_flux_change_classes[, .N, by = .(dataset, lat_zone, category = conditions_availability)][
    , .(prop = N / sum(N)), by = .(dataset, lat_zone, category)][category == "Drier"],
  avail_flux_change_classes[, .N, by = .(dataset, lat_zone, category = conditions_flux)][
    , .(prop = N / sum(N)), by = .(dataset, lat_zone, category)][category == "Accelerated"]
)

ggplot(prop_lines, aes(x = lat_zone, y = prop, color = category, group = category)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ dataset, ncol = 2) +
  labs(x = "Latitude Zone (deg)", y = "Proportion",
       title = "Proportion Drier and Accelerated by Latitude Zone") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

# -- 9. Net water volume change (km³) by lat zone and dataset -----------------
avail_flux_change_classes[, avail_change_vol := (avail_change / 1000) * area / 1e9]    # mm to m³, then km³
avail_flux_change_classes[, flux_change_vol  := (flux_change  / 1000) * area / 1e9]

to_plot_by_lat_ds <- avail_flux_change_classes[, .(
  avail_change_km3 = sum(avail_change_vol, na.rm=TRUE),
  flux_change_km3  = sum(flux_change_vol, na.rm=TRUE)
), by = .(dataset, lat_zone)]

zone_long <- melt(to_plot_by_lat_ds, id.vars = c("dataset", "lat_zone"),
                  variable.name = "Metric", value.name = "Value")

ggplot(zone_long, aes(x = lat_zone, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +
  geom_point() +
  facet_wrap(~ dataset, ncol = 2, scales = "free_y") +
  theme_minimal() +
  labs(x = "Latitude Zone", y = "Water Volume Change (km³)",
       title = "Net Water Availability and Acceleration by Latitude Zone (All Datasets)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

install.packages('ggforce')
library(ggforce)

to_plot_by_ipcc <- avail_flux_change_classes[, .(
  avail_change_km3 = sum(avail_change_vol, na.rm=TRUE),
  flux_change_km3  = sum(flux_change_vol, na.rm=TRUE)
), by = .(dataset, ipcc_short_region)]

ipcc_long <- melt(
  to_plot_by_ipcc, 
  id.vars = c("dataset", "ipcc_short_region"),
  variable.name = "Metric", value.name = "Value"
)

ggplot(ipcc_long, aes(x = dataset, y = Value, col = Metric, group = Metric)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  facet_wrap_paginate(~ ipcc_short_region, ncol = 4, nrow = 4, page = 3, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "IPCC Region",
    y = "Water Volume Change (km³)",
    title = "Net Water Availability and Acceleration by IPCC Region (All Datasets)",
    fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

to_plot_by_ipcc[, Conditions := NULL]
to_plot_by_ipcc[,
                                           Conditions := factor("Unknown",
                                                                levels = c('Wetter - Accelerated', 'Drier - Accelerated',
                                                                           'Wetter - Deccelerated', 'Drier - Deccelerated', "Unknown"))
]

to_plot_by_ipcc[flux_change_km3 > 0 & avail_change_km3 > 0,  Conditions := factor('Wetter - Accelerated')]
to_plot_by_ipcc[flux_change_km3 > 0 & avail_change_km3 < 0,  Conditions := factor('Drier - Accelerated')]
to_plot_by_ipcc[flux_change_km3 < 0 & avail_change_km3 > 0,  Conditions := factor('Wetter - Deccelerated')]
to_plot_by_ipcc[flux_change_km3 < 0 & avail_change_km3 < 0,  Conditions := factor('Drier - Deccelerated')]
to_plot <- to_plot_by_ipcc[dataset != 'HYBRID' & dataset != 'BEST KG']
#to_plot <- to_plot_by_ipcc[dataset != 'HYBRID']

ggplot(to_plot) +
  # Line from origin to each point
  geom_segment(aes(
    x = 0, y = 0,
    xend = avail_change_km3, yend = flux_change_km3,
    color = Conditions
  ), alpha = 0.3) +
  geom_point(aes(y = flux_change_km3, x = avail_change_km3, col = Conditions, shape = dataset), size = 2) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = PALETTES$water_cycle_change) +
  facet_wrap_paginate(~ ipcc_short_region, ncol = 4, nrow = 4, page = 1, scales = "free") +
  xlab(expression(atop(P - E))) +
  ylab(expression(atop((P + E) / 2))) +
  #geom_text(data = to_plot, aes(y = flux_change_km3, x = avail_change_km3, label = dataset), 
  #          vjust = -1.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))
