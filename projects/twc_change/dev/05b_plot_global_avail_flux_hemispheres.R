# ============================================================================
# Plot hemispheric and latitude-zone Monte Carlo water-cycle change cloud
#
# This script:
# 1. Uses mc_region_biome_metrics_base from the 500-member base ensemble
# 2. Uses twc_grid_classes.Rds to define hemisphere/latitude-zone area weights
# 3. Aggregates region-biome MC metrics to hemispheres and hemisphere x lat zones
# 4. Plots the 500-member ensemble cloud in availability-flux change space
# ============================================================================

# Inputs ======================================================================

source("source/twc_change.R")

library(data.table)
library(ggplot2)
library(ggrepel)

mc_region_biome_metrics_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_metrics_base.Rds")
)

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Constants & Variables =======================================================

TROPIC_LAT <- 23.5
HIGH_LAT <- 60

SIGNIFICANCE_AREA_THRESHOLD <- 0.50

STORY_COLS <- c(
  "wetter_accelerated" = PALETTES$water_cycle_change[1],
  "wetter_decelerated" = PALETTES$water_cycle_change[2],
  "drier_accelerated" = PALETTES$water_cycle_change[3],
  "drier_decelerated" = PALETTES$water_cycle_change[4]
)

STORY_LABELS <- c(
  "wetter_accelerated" = "Wet and accelerated",
  "wetter_decelerated" = "Wet and decelerated",
  "drier_accelerated" = "Dry and accelerated",
  "drier_decelerated" = "Dry and decelerated"
)

# Functions ===================================================================

weighted_mean_safe <- function(x, w) {
  
  ok <- is.finite(x) & is.finite(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_frac_safe <- function(x, w) {
  
  ok <- !is.na(x) & is.finite(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(as.numeric(x[ok]) * w[ok]) / sum(w[ok])
}

make_axis_limit <- function(x, pad = 1.25) {
  
  lim <- max(abs(x), na.rm = TRUE) * pad
  
  if (!is.finite(lim) || lim == 0) {
    lim <- 1
  }
  
  lim
}

standardise_hemisphere <- function(x, lat) {
  
  x_chr <- tolower(as.character(x))
  
  fifelse(
    x_chr %in% c("north", "northern", "nh", "northern hemisphere"),
    "Northern Hemisphere",
    fifelse(
      x_chr %in% c("south", "southern", "sh", "southern hemisphere"),
      "Southern Hemisphere",
      fifelse(
        lat >= 0,
        "Northern Hemisphere",
        "Southern Hemisphere"
      )
    )
  )
}

standardise_lat_zone <- function(x, lat) {
  
  x_chr <- tolower(as.character(x))
  x_chr <- gsub("[^a-z]", "", x_chr)
  
  fifelse(
    x_chr %in% c("tropic", "tropics", "tropical"),
    "Tropics",
    fifelse(
      x_chr %in% c("extratropic", "extratropics", "extratropical", "midlatitude"),
      "Extra-tropics",
      fifelse(
        x_chr %in% c("highlatitude", "highlatitudes", "polar"),
        "High latitudes",
        fcase(
          abs(lat) <= TROPIC_LAT,
          "Tropics",
          abs(lat) > TROPIC_LAT & abs(lat) < HIGH_LAT,
          "Extra-tropics",
          abs(lat) >= HIGH_LAT,
          "High latitudes"
        )
      )
    )
  )
}

prepare_grid_weights <- function(twc_grid, group_cols) {
  
  grid_dt <- as.data.table(copy(twc_grid))
  
  required_cols <- c("lon", "lat", "region", "biome", "area")
  missing_cols <- setdiff(required_cols, names(grid_dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "twc_grid is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  grid_dt[
    ,
    `:=`(
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  if ("hemisphere" %in% names(grid_dt)) {
    grid_dt[
      ,
      hemisphere := standardise_hemisphere(hemisphere, lat)
    ]
  } else {
    grid_dt[
      ,
      hemisphere := fifelse(
        lat >= 0,
        "Northern Hemisphere",
        "Southern Hemisphere"
      )
    ]
  }
  
  if ("lat_zone" %in% names(grid_dt)) {
    grid_dt[
      ,
      lat_zone := standardise_lat_zone(lat_zone, lat)
    ]
  } else {
    grid_dt[
      ,
      lat_zone := fcase(
        abs(lat) <= TROPIC_LAT,
        "Tropics",
        abs(lat) > TROPIC_LAT & abs(lat) < HIGH_LAT,
        "Extra-tropics",
        abs(lat) >= HIGH_LAT,
        "High latitudes"
      )
    ]
  }
  
  grid_dt <- grid_dt[
    is.finite(lon) &
      is.finite(lat) &
      !is.na(region) &
      !is.na(biome) &
      !is.na(hemisphere) &
      !is.na(lat_zone) &
      is.finite(area) &
      area > 0
  ]
  
  grid_dt <- unique(
    grid_dt,
    by = c("lon", "lat")
  )
  
  weight_dt <- grid_dt[
    ,
    .(
      area = sum(area, na.rm = TRUE),
      n_cells = .N
    ),
    by = c(group_cols, "region", "biome")
  ]
  
  setkeyv(
    weight_dt,
    c("region", "biome")
  )
  
  weight_dt[]
}

prepare_metric_input <- function(mc_region_biome_metrics_base) {
  
  metrics_dt <- as.data.table(copy(mc_region_biome_metrics_base))
  
  required_cols <- c(
    "sim",
    "scenario",
    "region",
    "biome",
    "variable",
    "mean_1982_2001",
    "mean_2002_2021",
    "diff_2002_2021_minus_1982_2001",
    "diff_percent",
    "diff_p_value",
    "diff_stat_sig",
    "slope_full",
    "slope_full_p_value",
    "slope_full_stat_sig"
  )
  
  missing_cols <- setdiff(required_cols, names(metrics_dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "mc_region_biome_metrics_base is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  metrics_dt[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      variable = as.character(variable)
    )
  ]
  
  metrics_dt[]
}

aggregate_metrics_to_group <- function(metrics_dt, weight_dt, group_cols) {
  
  joined_dt <- weight_dt[
    metrics_dt,
    on = .(region, biome),
    allow.cartesian = TRUE,
    nomatch = 0L
  ]
  
  out <- joined_dt[
    ,
    .(
      mean_1982_2001 = weighted_mean_safe(mean_1982_2001, area),
      mean_2002_2021 = weighted_mean_safe(mean_2002_2021, area),
      diff_change = weighted_mean_safe(
        diff_2002_2021_minus_1982_2001,
        area
      ),
      diff_percent = weighted_mean_safe(diff_percent, area),
      diff_stat_sig_area_frac = weighted_frac_safe(diff_stat_sig, area),
      slope_full = weighted_mean_safe(slope_full, area),
      slope_stat_sig_area_frac = weighted_frac_safe(slope_full_stat_sig, area),
      area = sum(area, na.rm = TRUE),
      n_region_biomes = uniqueN(paste(region, biome)),
      n_cells = sum(n_cells, na.rm = TRUE)
    ),
    by = c("sim", "scenario", group_cols, "variable")
  ]
  
  setkeyv(
    out,
    c("sim", "scenario", group_cols, "variable")
  )
  
  out[]
}

prepare_avail_flux_cloud <- function(group_metric_dt, group_cols) {
  
  dt <- as.data.table(copy(group_metric_dt))
  
  avail_dt <- dt[
    variable == "avail",
    c(
      "sim",
      "scenario",
      group_cols,
      "diff_change",
      "diff_stat_sig_area_frac",
      "slope_full",
      "slope_stat_sig_area_frac"
    ),
    with = FALSE
  ]
  
  setnames(
    avail_dt,
    old = c(
      "diff_change",
      "diff_stat_sig_area_frac",
      "slope_full",
      "slope_stat_sig_area_frac"
    ),
    new = c(
      "avail_change",
      "avail_diff_sig_area_frac",
      "avail_slope",
      "avail_slope_sig_area_frac"
    )
  )
  
  flux_dt <- dt[
    variable == "flux",
    c(
      "sim",
      "scenario",
      group_cols,
      "diff_change",
      "diff_stat_sig_area_frac",
      "slope_full",
      "slope_stat_sig_area_frac"
    ),
    with = FALSE
  ]
  
  setnames(
    flux_dt,
    old = c(
      "diff_change",
      "diff_stat_sig_area_frac",
      "slope_full",
      "slope_stat_sig_area_frac"
    ),
    new = c(
      "flux_change",
      "flux_diff_sig_area_frac",
      "flux_slope",
      "flux_slope_sig_area_frac"
    )
  )
  
  out <- merge(
    avail_dt,
    flux_dt,
    by = c("sim", "scenario", group_cols),
    all = FALSE
  )
  
  out <- out[
    is.finite(avail_change) &
      is.finite(flux_change)
  ]
  
  out[
    ,
    storyline := fcase(
      avail_change >= 0 & flux_change >= 0,
      "wetter_accelerated",
      avail_change < 0 & flux_change >= 0,
      "drier_accelerated",
      avail_change >= 0 & flux_change < 0,
      "wetter_decelerated",
      avail_change < 0 & flux_change < 0,
      "drier_decelerated"
    )
  ]
  
  out[
    ,
    `:=`(
      sig_story_both =
        avail_diff_sig_area_frac >= SIGNIFICANCE_AREA_THRESHOLD &
        flux_diff_sig_area_frac >= SIGNIFICANCE_AREA_THRESHOLD,
      sig_story_either =
        avail_diff_sig_area_frac >= SIGNIFICANCE_AREA_THRESHOLD |
        flux_diff_sig_area_frac >= SIGNIFICANCE_AREA_THRESHOLD
    )
  ]
  
  out[
    ,
    storyline := factor(
      storyline,
      levels = names(STORY_LABELS),
      labels = STORY_LABELS
    )
  ]
  
  out[]
}

make_axis_dt <- function(dt, facet_col, pad = 1.25) {
  
  axis_dt <- dt[
    ,
    .(
      x_abs = make_axis_limit(avail_change, pad = pad),
      y_abs = make_axis_limit(flux_change, pad = pad)
    ),
    by = facet_col
  ]
  
  axis_dt[
    ,
    .(
      x = c(-x_abs, x_abs, -x_abs, x_abs),
      y = c(-y_abs, -y_abs, y_abs, y_abs)
    ),
    by = facet_col
  ]
}

make_median_dt <- function(plot_dt, facet_col) {
  
  plot_dt[
    ,
    .(
      avail_change = median(avail_change, na.rm = TRUE),
      flux_change = median(flux_change, na.rm = TRUE)
    ),
    by = facet_col
  ]
}

make_avail_flux_cloud_plot <- function(
    plot_dt,
    facet_col,
    title,
    subtitle,
    ncol = 2
) {
  
  plot_dt <- as.data.table(copy(plot_dt))
  
  axis_dt <- make_axis_dt(
    dt = plot_dt,
    facet_col = facet_col
  )
  
  median_dt <- make_median_dt(
    plot_dt = plot_dt,
    facet_col = facet_col
  )
  
  ggplot() +
    
    geom_blank(
      data = axis_dt,
      aes(
        x = x,
        y = y
      )
    ) +
    
    annotate(
      "rect",
      xmin = -Inf,
      xmax = 0,
      ymin = 0,
      ymax = Inf,
      fill = STORY_COLS["drier_accelerated"],
      alpha = 0.09
    ) +
    annotate(
      "rect",
      xmin = 0,
      xmax = Inf,
      ymin = 0,
      ymax = Inf,
      fill = STORY_COLS["wetter_accelerated"],
      alpha = 0.09
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = 0,
      ymin = -Inf,
      ymax = 0,
      fill = STORY_COLS["drier_decelerated"],
      alpha = 0.09
    ) +
    annotate(
      "rect",
      xmin = 0,
      xmax = Inf,
      ymin = -Inf,
      ymax = 0,
      fill = STORY_COLS["wetter_decelerated"],
      alpha = 0.09
    ) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.40,
      colour = "grey35"
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.40,
      colour = "grey35"
    ) +
    
    geom_abline(
      slope = -0.5,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.40,
      colour = "grey30"
    ) +
    geom_abline(
      slope = 0.5,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.40,
      colour = "grey30"
    ) +
    
    geom_point(
      data = plot_dt,
      aes(
        x = avail_change,
        y = flux_change
      ),
      colour = "grey78",
      alpha = 0.32,
      size = 2.2
    ) +
    
    geom_point(
      data = plot_dt[sig_story_either == TRUE],
      aes(
        x = avail_change,
        y = flux_change
      ),
      colour = "grey50",
      alpha = 0.45,
      size = 2.6
    ) +
    
    geom_point(
      data = plot_dt[sig_story_both == TRUE],
      aes(
        x = avail_change,
        y = flux_change
      ),
      shape = 0,
      colour = "grey25",
      size = 3.2,
      stroke = 0.55
    ) +
    
    geom_point(
      data = median_dt,
      aes(
        x = avail_change,
        y = flux_change
      ),
      shape = 13,
      size = 5.8,
      stroke = 0.8,
      colour = "black"
    ) +
    
    facet_wrap(
      as.formula(paste0("~", facet_col)),
      ncol = ncol,
      scales = "free"
    ) +
    
    scale_x_continuous(
      name = expression(Delta(P - E) ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = expression(Delta((P + E) / 2) ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma,
      expand = c(0, 0)
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      caption = paste0(
        "Grey points: 500 base ensemble members. ",
        "Darker grey: at least one axis has significant area support ≥ ",
        SIGNIFICANCE_AREA_THRESHOLD,
        ". Squares: both axes have significant area support ≥ ",
        SIGNIFICANCE_AREA_THRESHOLD,
        ". Cross: ensemble median. Dashed diagonals mark ΔP = 0 and ΔE = 0."
      )
    ) +
    
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey93"),
      legend.position = "none",
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(
        size = 8.5,
        colour = "grey45",
        hjust = 0,
        margin = margin(t = 6)
      ),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9),
      plot.margin = margin(8, 14, 8, 8)
    )
}

# Analysis ====================================================================

metrics_dt <- prepare_metric_input(
  mc_region_biome_metrics_base = mc_region_biome_metrics_base
)

# Hemispheres -----------------------------------------------------------------

hemi_weights <- prepare_grid_weights(
  twc_grid = twc_grid,
  group_cols = "hemisphere"
)

mc_hemi_metrics <- aggregate_metrics_to_group(
  metrics_dt = metrics_dt,
  weight_dt = hemi_weights,
  group_cols = "hemisphere"
)

mc_hemi_cloud <- prepare_avail_flux_cloud(
  group_metric_dt = mc_hemi_metrics,
  group_cols = "hemisphere"
)

mc_hemi_cloud[
  ,
  hemisphere := factor(
    hemisphere,
    levels = c("Northern Hemisphere", "Southern Hemisphere")
  )
]

# Hemisphere x latitude zones -------------------------------------------------

zone_weights <- prepare_grid_weights(
  twc_grid = twc_grid,
  group_cols = c("hemisphere", "lat_zone")
)

mc_zone_metrics <- aggregate_metrics_to_group(
  metrics_dt = metrics_dt,
  weight_dt = zone_weights,
  group_cols = c("hemisphere", "lat_zone")
)

mc_zone_cloud <- prepare_avail_flux_cloud(
  group_metric_dt = mc_zone_metrics,
  group_cols = c("hemisphere", "lat_zone")
)

mc_zone_cloud[
  ,
  lat_zone := factor(
    lat_zone,
    levels = c("Tropics", "Extra-tropics", "High latitudes")
  )
]

mc_zone_cloud[
  ,
  hemisphere := factor(
    hemisphere,
    levels = c("Northern Hemisphere", "Southern Hemisphere")
  )
]

mc_zone_cloud[
  ,
  hemisphere_zone := factor(
    paste(hemisphere, lat_zone, sep = "\n"),
    levels = as.vector(
      outer(
        c("Northern Hemisphere", "Southern Hemisphere"),
        c("Tropics", "Extra-tropics", "High latitudes"),
        paste,
        sep = "\n"
      )
    )
  )
]

# Outputs =====================================================================

saveRDS(
  mc_hemi_metrics,
  file.path(PATH_OUTPUT_DATA, "mc_hemisphere_metrics_base.Rds")
)

saveRDS(
  mc_zone_metrics,
  file.path(PATH_OUTPUT_DATA, "mc_hemisphere_zone_metrics_base.Rds")
)

saveRDS(
  mc_hemi_cloud,
  file.path(PATH_OUTPUT_DATA, "mc_hemisphere_avail_flux_cloud_base.Rds")
)

saveRDS(
  mc_zone_cloud,
  file.path(PATH_OUTPUT_DATA, "mc_hemisphere_zone_avail_flux_cloud_base.Rds")
)

# Plot ========================================================================

p_hemi <- make_avail_flux_cloud_plot(
  plot_dt = mc_hemi_cloud,
  facet_col = "hemisphere",
  title = "Hemispheric water-cycle change in availability and flux space",
  subtitle = "Base weighting scenario, 500 Monte Carlo ensemble members",
  ncol = 2
)

p_zone <- make_avail_flux_cloud_plot(
  plot_dt = mc_zone_cloud,
  facet_col = "hemisphere_zone",
  title = "Hemispheric latitude-zone water-cycle change in availability and flux space",
  subtitle = paste0(
    "Base weighting scenario, 500 Monte Carlo ensemble members. ",
    "Latitude zones use |lat| ≤ ",
    TROPIC_LAT,
    ", ",
    TROPIC_LAT,
    " < |lat| < ",
    HIGH_LAT,
    ", and |lat| ≥ ",
    HIGH_LAT,
    "."
  ),
  ncol = 3
)

p_hemi
p_zone

# Save ========================================================================

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "mc_hemisphere_avail_flux_cloud_base.png"
  ),
  plot = p_hemi,
  width = 11,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "mc_hemisphere_zone_avail_flux_cloud_base.png"
  ),
  plot = p_zone,
  width = 13,
  height = 8.5,
  dpi = 300
)

# Validation ==================================================================

cat("\nHemispheric cloud summary:\n")
print(
  mc_hemi_cloud[
    ,
    .(
      n_members = .N,
      avail_q05 = quantile(avail_change, 0.05, na.rm = TRUE),
      avail_q50 = median(avail_change, na.rm = TRUE),
      avail_q95 = quantile(avail_change, 0.95, na.rm = TRUE),
      flux_q05 = quantile(flux_change, 0.05, na.rm = TRUE),
      flux_q50 = median(flux_change, na.rm = TRUE),
      flux_q95 = quantile(flux_change, 0.95, na.rm = TRUE)
    ),
    by = hemisphere
  ]
)

cat("\nHemisphere x latitude-zone cloud summary:\n")
print(
  mc_zone_cloud[
    ,
    .(
      n_members = .N,
      avail_q05 = quantile(avail_change, 0.05, na.rm = TRUE),
      avail_q50 = median(avail_change, na.rm = TRUE),
      avail_q95 = quantile(avail_change, 0.95, na.rm = TRUE),
      flux_q05 = quantile(flux_change, 0.05, na.rm = TRUE),
      flux_q50 = median(flux_change, na.rm = TRUE),
      flux_q95 = quantile(flux_change, 0.95, na.rm = TRUE)
    ),
    by = .(hemisphere, lat_zone)
  ][
    order(hemisphere, lat_zone)
  ]
)

cat("\nFinished plotting hemispheric Monte Carlo availability-flux clouds.\n")