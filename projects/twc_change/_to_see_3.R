install.packages('scatterpie')
library(scatterpie)
library(data.table)
library(ggplot2)

source('source/avail_flux_change.R')
source('source/evap_trend.R')

avail_flux <-  readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_period_mean.Rds'))
masks <- pRecipe::pRecipe_masks()
avail_flux <- merge(
  avail_flux, 
  masks[land_mask == 'land', .(lon, lat, ipcc_short_region)], 
  by = c('lon', 'lat')
)
setnames(avail_flux, "ipcc_short_region", "region")

# --- NEW: Merge grid area (assumes grid_area() returns m² column 'area') -----
if (!"area" %in% names(avail_flux)) {
  grid_cell_area <- unique(avail_flux[, .(lon, lat)]) %>% grid_area()
  avail_flux <- merge(avail_flux, grid_cell_area, by = c("lon", "lat"))
}

# --- Calculate water availability/flux volumes per cell (mm/year → km³/year) --
avail_flux[, water_avail_vol := (avail / 1000) * area / 1e9]  # mm to m, m² to km³
avail_flux[, water_flux_vol  := (flux / 1000) * area / 1e9]

# --- Aggregate mean values (mm/yr) and sum volumes (km³/yr) -------------------
# Water availability
water_avail <- avail_flux[, .(
  water_avail = mean(avail, na.rm = TRUE),
  water_avail_vol = sum(water_avail_vol, na.rm = TRUE)
), by = .(dataset, period, region)]

water_avail_wide <- dcast(water_avail, dataset + region ~ period,
                          value.var = c("water_avail", "water_avail_vol"))

# Calculate relative change (for mean mm/yr and for km³/yr)
for (col in c("water_avail", "water_avail_vol")) {
  water_avail_wide[, paste0("aft_2001_", col) := 
                     (get(paste0(col, "_aft_2001")) - get(paste0(col, "_pre_2001"))) / get(paste0(col, "_pre_2001"))]
  water_avail_wide[, paste0("pre_2001_", col) := 0]
  water_avail_wide[, paste0(col, "_change") := get(paste0("aft_2001_", col))]
}

# Melt for plotting
water_avail_melt <- melt(
  water_avail_wide, 
  id.vars = c('dataset', "region", "water_avail_change", "water_avail_vol_change"),
  measure.vars = patterns("water_avail_", "water_avail_vol_"),
  variable.name = 'period',
  value.name = c('water_avail', 'water_avail_vol')
)

# --- Repeat for flux ---------------------------------------------------------
water_flux <- avail_flux[, .(
  water_flux = mean(flux, na.rm = TRUE),
  water_flux_vol = sum(water_flux_vol, na.rm = TRUE)
), by = .(dataset, period, region)]

water_flux_wide <- dcast(water_flux, dataset + region ~ period,
                         value.var = c("water_flux", "water_flux_vol"))

for (col in c("water_flux", "water_flux_vol")) {
  water_flux_wide[, paste0("aft_2001_", col) := 
                    (get(paste0(col, "_aft_2001")) - get(paste0(col, "_pre_2001"))) / get(paste0(col, "_pre_2001"))]
  water_flux_wide[, paste0("pre_2001_", col) := 0]
  water_flux_wide[, paste0(col, "_change") := get(paste0("aft_2001_", col))]
}

water_flux_melt <- melt(
  water_flux_wide,
  id.vars = c('dataset', "region", "water_flux_change", "water_flux_vol_change"),
  measure.vars = patterns("water_flux_", "water_flux_vol_"),
  variable.name = 'period',
  value.name = c('water_flux', 'water_flux_vol')
)

# --- Combine for plotting, include both mean and volume ----------------------
water_avail_flux <- merge(
  water_avail_melt,
  water_flux_melt,
  by = c("dataset", "period", "region"),
  allow.cartesian = TRUE
)

# --- Assign change class -----------------------------------------------------
to_plot <- copy(avail_flux_change_classes)[,
                                           Conditions := factor("Unknown",
                                                                levels = c('Wetter - Accelerated', 'Drier - Accelerated',
                                                                           'Wetter - Deccelerated', 'Drier - Deccelerated', "Unknown"))
]

to_plot[flux_change > 0 & avail_change > 0,  Conditions := 'Wetter - Accelerated']
to_plot[flux_change > 0 & avail_change < 0,  Conditions := 'Drier - Accelerated']
to_plot[flux_change < 0 & avail_change > 0,  Conditions := 'Wetter - Deccelerated']
to_plot[flux_change < 0 & avail_change < 0,  Conditions := 'Drier - Deccelerated']

# --- Plot using volumes instead of mm/yr -------------------------------------
levels(water_avail_flux$period) <- c("1981-2000", "2001-2020")
setnames(water_avail_flux, 'period', "Period")
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(water_avail_flux) +
  geom_point(aes(y = water_flux_vol, x = water_avail_vol, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux_vol, x = water_avail_vol, group = dataset_pair, col = Conditions), 
            alpha = 0.5) +
  facet_wrap(~region, scales = 'free') +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[km"^3*"/year]"))) +
  ylab(expression(atop((P + E)/2~"[km"^3*"/year]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))
