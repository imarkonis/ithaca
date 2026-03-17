# Libraries ====================================================================

install.packages('scatterpie')
install.packages('ggforce')

library(ggforce)
library(scatterpie)

source('source/twc_change.R')

# IPCC hexagon centroids ------------------------------------------------------
ipcc_hexagon <- data.table(
  read.csv("/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv")
)[, .(region = Acronym, x = CENTROIX, y = CENTROIY)]

ensemble_region_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_members_region_summary.Rds")
)[, .(scenario, region, storyline)]

# Keep only base scenario -----------------------------------------------------
to_plot <- merge(
  ensemble_region_global[scenario == "base", .(region, storyline)],
  ipcc_hexagon,
  by = "region",
  allow.cartesian = TRUE
)

# Rename to exact labels you want in legend/palette ---------------------------
to_plot[storyline == "wetter-accelerated", storyline := "Wetter - Accelerated"]
to_plot[storyline == "wetter-decelerated", storyline := "Wetter - Decelerated"]
to_plot[storyline == "drier-accelerated",  storyline := "Drier - Accelerated"]
to_plot[storyline == "drier-decelerated",  storyline := "Drier - Decelerated"]

# Count frequencies per region ------------------------------------------------
to_plot_donut <- to_plot[, .N, by = .(region, x, y, storyline)]

to_plot_donut <- dcast(
  to_plot_donut,
  region + x + y ~ storyline,
  value.var = "N",
  fill = 0
)

pie_cols <- c(
  "Wetter - Accelerated",
  "Wetter - Decelerated",
  "Drier - Accelerated",
  "Drier - Decelerated"
)

# Make sure all pie columns exist ---------------------------------------------
missing_cols <- setdiff(pie_cols, names(to_plot_donut))
if (length(missing_cols) > 0) {
  to_plot_donut[, (missing_cols) := 0]
}

# Radii -----------------------------------------------------------------------
to_plot_donut[, r_outer := 4]
to_plot_donut[, r_inner := 2.2]

# World map -------------------------------------------------------------------
world <- map_data("world")

ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "black",
    linewidth = 0.2
  ) +
  geom_scatterpie(
    data = to_plot_donut,
    aes(x = x, y = y, r = r_outer),
    cols = pie_cols,
    inherit.aes = FALSE
  ) +
  coord_quickmap() +
  scale_fill_manual(
    values = PALETTES$water_cycle_change
  ) +
  theme_void()
