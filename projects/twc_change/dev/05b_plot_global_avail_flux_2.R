# ============================================================================
# Plot global availability versus flux change for Monte Carlo storyline analysis
#
# This script:
# 1. Uses MC metric outputs from the new workflow
# 2. Plots base 500-member ensemble in availability-flux change space
# 3. Adds observational dataset points estimated from dataset_region_biome_year
# 4. Adds quadrant shading and P/E driver diagonals
# 5. Creates storyline distribution donuts for scenario sensitivity
# ============================================================================

# Libraries ===================================================================

library(ggrepel)
library(patchwork)
source("source/twc_change.R")

# Inputs ======================================================================

mc_global_metrics_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_metrics_base.Rds")
)

mc_global_metrics_scenarios <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_metrics_scenarios.Rds")
)

dataset_region_biome_year <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_biome_year.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Constants & Variables =======================================================

PERIOD_1 <- 1982:2001
PERIOD_2 <- 2002:2021

THRES_SIGNIFICANCE <- 0.05

SIGNIFICANCE_MODE <- "diff"
# Use "diff"  for period-difference p values
# Use "slope" for full-period trend p values

FIG_DIR <- if (exists("PATH_FIGURES")) {
  PATH_FIGURES
} else if (exists("PATH_OUTPUT_FIGURES")) {
  PATH_OUTPUT_FIGURES
} else {
  PATH_OUTPUT_DATA
}

dir.create(
  FIG_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

STORY_LEVELS <- c(
  "wetter-accelerated",
  "drier-accelerated",
  "wetter-decelerated",
  "drier-decelerated"
)

STORY_LABELS <- c(
  "wetter-accelerated" = "Wet & Accelerated",
  "drier-accelerated" = "Dry & Accelerated",
  "wetter-decelerated" = "Wet & Decelerated",
  "drier-decelerated" = "Dry & Decelerated"
)

if (
  exists("PALETTES") &&
  "water_cycle_change" %in% names(PALETTES) &&
  length(PALETTES$water_cycle_change) >= 4
) {
  STORY_COLS <- c(
    "wetter-accelerated" = PALETTES$water_cycle_change[1],
    "drier-accelerated" = PALETTES$water_cycle_change[3],
    "wetter-decelerated" = PALETTES$water_cycle_change[2],
    "drier-decelerated" = PALETTES$water_cycle_change[4]
  )
} else {
  STORY_COLS <- c(
    "wetter-accelerated" = "#2b83ba",
    "drier-accelerated" = "#d7191c",
    "wetter-decelerated" = "#abdda4",
    "drier-decelerated" = "#fdae61"
  )
}

# Functions ===================================================================

label_scenario <- function(x) {
  
  out <- gsub("_", " ", x)
  out <- tools::toTitleCase(out)
  out
}

lighten_col <- function(hex, amount = 0.55) {
  
  m <- col2rgb(hex) / 255
  l <- m + (1 - m) * amount
  
  rgb(l[1, ], l[2, ], l[3, ])
}

get_cell_area_column <- function(dt) {
  
  possible_area_cols <- c(
    "cell_area",
    "area",
    "area_km2",
    "area_m2",
    "area_sum"
  )
  
  area_cols <- intersect(possible_area_cols, names(dt))
  
  if (length(area_cols) == 0L) {
    return(NA_character_)
  }
  
  area_cols[1]
}

prepare_region_biome_area <- function(twc_grid_classes) {
  
  grid_dt <- as.data.table(copy(twc_grid_classes))
  
  required_cols <- c("lon", "lat", "region", "biome")
  missing_cols <- setdiff(required_cols, names(grid_dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "twc_grid_classes is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  area_col <- get_cell_area_column(grid_dt)
  
  if (is.na(area_col)) {
    
    message(
      "No explicit cell-area column found. ",
      "Using cos(latitude) as relative area weight."
    )
    
    grid_dt[
      ,
      cell_area := cos(lat * pi / 180)
    ]
    
  } else {
    
    message("Using existing area column: ", area_col)
    
    if (area_col != "cell_area") {
      setnames(
        grid_dt,
        old = area_col,
        new = "cell_area"
      )
    }
  }
  
  grid_dt[
    ,
    `:=`(
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  grid_dt <- grid_dt[
    is.finite(lon) &
      is.finite(lat) &
      !is.na(region) &
      !is.na(biome) &
      is.finite(cell_area) &
      cell_area > 0,
    .(lon, lat, region, biome, cell_area)
  ]
  
  grid_dt <- unique(
    grid_dt,
    by = c("lon", "lat")
  )
  
  grid_dt[
    ,
    .(
      area_weight = sum(cell_area, na.rm = TRUE)
    ),
    by = .(region, biome)
  ]
}

weighted_mean_safe <- function(value, weight) {
  
  ok <- is.finite(value) &
    is.finite(weight) &
    weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(value[ok] * weight[ok]) / sum(weight[ok])
}

safe_lm_slope <- function(year, value) {
  
  ok <- is.finite(year) & is.finite(value)
  
  if (sum(ok) < 3L) {
    return(
      data.table(
        slope_full = NA_real_,
        slope_full_p_value = NA_real_,
        n_years_full = sum(ok)
      )
    )
  }
  
  year_ok <- year[ok]
  value_ok <- value[ok]
  
  fit <- tryCatch(
    lm(value_ok ~ year_ok),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(
      data.table(
        slope_full = NA_real_,
        slope_full_p_value = NA_real_,
        n_years_full = length(year_ok)
      )
    )
  }
  
  coef_dt <- summary(fit)$coefficients
  
  data.table(
    slope_full = unname(coef_dt["year_ok", "Estimate"]),
    slope_full_p_value = unname(coef_dt["year_ok", "Pr(>|t|)"]),
    n_years_full = length(year_ok)
  )
}

safe_t_test_p <- function(value_1, value_2) {
  
  value_1 <- value_1[is.finite(value_1)]
  value_2 <- value_2[is.finite(value_2)]
  
  if (length(value_1) < 3L || length(value_2) < 3L) {
    return(NA_real_)
  }
  
  if (sd(value_1) == 0 && sd(value_2) == 0) {
    return(NA_real_)
  }
  
  tryCatch(
    t.test(value_2, value_1)$p.value,
    error = function(e) NA_real_
  )
}

prepare_dataset_global_year <- function(
    dataset_region_biome_year,
    region_biome_area
) {
  
  dt <- as.data.table(copy(dataset_region_biome_year))
  
  required_cols <- c(
    "dataset",
    "region",
    "biome",
    "year",
    "prec",
    "evap"
  )
  
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "dataset_region_biome_year is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  dt[
    ,
    `:=`(
      dataset = as.character(dataset),
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  dt <- region_biome_area[
    dt,
    on = .(region, biome),
    nomatch = 0L
  ]
  
  out <- dt[
    ,
    .(
      prec = weighted_mean_safe(prec, area_weight),
      evap = weighted_mean_safe(evap, area_weight)
    ),
    by = .(dataset, year)
  ]
  
  setkey(
    out,
    dataset,
    year
  )
  
  out
}

estimate_dataset_global_metrics <- function(dataset_global_year) {
  
  dt <- as.data.table(copy(dataset_global_year))
  
  dt[
    ,
    `:=`(
      avail = prec - evap,
      flux = (prec + evap) / 2
    )
  ]
  
  long_dt <- melt(
    dt,
    id.vars = c("dataset", "year"),
    measure.vars = c("prec", "evap", "avail", "flux"),
    variable.name = "variable",
    value.name = "value"
  )
  
  out <- long_dt[
    ,
    {
      value_1 <- value[year %in% PERIOD_1]
      value_2 <- value[year %in% PERIOD_2]
      
      mean_1 <- mean(value_1, na.rm = TRUE)
      mean_2 <- mean(value_2, na.rm = TRUE)
      
      if (!is.finite(mean_1)) {
        mean_1 <- NA_real_
      }
      
      if (!is.finite(mean_2)) {
        mean_2 <- NA_real_
      }
      
      slope_dt <- safe_lm_slope(
        year = year,
        value = value
      )
      
      .(
        mean_1982_2001 = mean_1,
        mean_2002_2021 = mean_2,
        diff_2002_2021_minus_1982_2001 = mean_2 - mean_1,
        diff_p_value = safe_t_test_p(value_1, value_2),
        slope_full = slope_dt$slope_full,
        slope_full_p_value = slope_dt$slope_full_p_value,
        n_years_full = slope_dt$n_years_full
      )
    },
    by = .(dataset, variable)
  ]
  
  out[]
}

prepare_avail_flux_dt <- function(metric_dt, id_cols) {
  
  dt <- as.data.table(copy(metric_dt))
  
  required_cols <- c(
    id_cols,
    "variable",
    "diff_2002_2021_minus_1982_2001",
    "diff_p_value",
    "slope_full",
    "slope_full_p_value"
  )
  
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "Metric table is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  avail_dt <- dt[
    variable == "avail",
    c(
      id_cols,
      "diff_2002_2021_minus_1982_2001",
      "diff_p_value",
      "slope_full",
      "slope_full_p_value"
    ),
    with = FALSE
  ]
  
  setnames(
    avail_dt,
    old = c(
      "diff_2002_2021_minus_1982_2001",
      "diff_p_value",
      "slope_full",
      "slope_full_p_value"
    ),
    new = c(
      "avail_abs_change",
      "avail_diff_p",
      "avail_slope",
      "avail_slope_p"
    )
  )
  
  flux_dt <- dt[
    variable == "flux",
    c(
      id_cols,
      "diff_2002_2021_minus_1982_2001",
      "diff_p_value",
      "slope_full",
      "slope_full_p_value"
    ),
    with = FALSE
  ]
  
  setnames(
    flux_dt,
    old = c(
      "diff_2002_2021_minus_1982_2001",
      "diff_p_value",
      "slope_full",
      "slope_full_p_value"
    ),
    new = c(
      "flux_abs_change",
      "flux_diff_p",
      "flux_slope",
      "flux_slope_p"
    )
  )
  
  out <- merge(
    avail_dt,
    flux_dt,
    by = id_cols,
    all = FALSE
  )
  
  out[]
}

add_storyline <- function(dt, significance_mode = "diff") {
  
  dt <- as.data.table(copy(dt))
  
  dt <- dt[
    is.finite(avail_abs_change) &
      is.finite(flux_abs_change)
  ]
  
  dt[
    ,
    storyline := fcase(
      avail_abs_change >= 0 & flux_abs_change >= 0,
      "wetter-accelerated",
      avail_abs_change < 0 & flux_abs_change >= 0,
      "drier-accelerated",
      avail_abs_change >= 0 & flux_abs_change < 0,
      "wetter-decelerated",
      avail_abs_change < 0 & flux_abs_change < 0,
      "drier-decelerated"
    )
  ]
  
  if (significance_mode == "diff") {
    
    dt[
      ,
      `:=`(
        sig_story_both =
          avail_diff_p < THRES_SIGNIFICANCE &
          flux_diff_p < THRES_SIGNIFICANCE,
        sig_story_either =
          avail_diff_p < THRES_SIGNIFICANCE |
          flux_diff_p < THRES_SIGNIFICANCE
      )
    ]
    
  } else if (significance_mode == "slope") {
    
    dt[
      ,
      `:=`(
        sig_story_both =
          avail_slope_p < THRES_SIGNIFICANCE &
          flux_slope_p < THRES_SIGNIFICANCE,
        sig_story_either =
          avail_slope_p < THRES_SIGNIFICANCE |
          flux_slope_p < THRES_SIGNIFICANCE
      )
    ]
    
  } else {
    
    stop("significance_mode must be either 'diff' or 'slope'.")
  }
  
  dt[
    ,
    storyline := factor(
      storyline,
      levels = STORY_LEVELS
    )
  ]
  
  dt[]
}

make_dataset_cols <- function(datasets) {
  
  datasets <- sort(unique(as.character(datasets)))
  
  if (length(datasets) <= 8L) {
    cols <- RColorBrewer::brewer.pal(
      max(3L, length(datasets)),
      "Set2"
    )[seq_along(datasets)]
  } else {
    cols <- scales::hue_pal()(length(datasets))
  }
  
  setNames(cols, datasets)
}

prepare_donut_dt <- function(dt, scenario_filter = NULL) {
  
  dt <- as.data.table(copy(dt))
  
  if (!is.null(scenario_filter)) {
    dt <- dt[scenario %in% scenario_filter]
  }
  
  dt <- dt[!is.na(storyline)]
  
  dt[
    ,
    fill_key := paste0(
      as.character(storyline),
      ifelse(sig_story_both, "_sig", "_nonsig")
    )
  ]
  
  donut_fill_levels <- as.vector(
    rbind(
      paste0(STORY_LEVELS, "_sig"),
      paste0(STORY_LEVELS, "_nonsig")
    )
  )
  
  out <- dt[
    ,
    .N,
    by = fill_key
  ]
  
  out[
    ,
    frac := N / sum(N)
  ]
  
  out <- merge(
    data.table(fill_key = donut_fill_levels),
    out,
    by = "fill_key",
    all.x = TRUE
  )
  
  out[
    is.na(N),
    `:=`(
      N = 0L,
      frac = 0
    )
  ]
  
  out[
    ,
    fill_key := factor(
      fill_key,
      levels = donut_fill_levels
    )
  ]
  
  out[]
}

build_avail_flux_scatter <- function(
    ens_dt,
    ds_dt,
    base_med_dt,
    plot_title = "Global availability versus flux change"
) {
  
  ens_dt <- as.data.table(copy(ens_dt))
  ds_dt <- as.data.table(copy(ds_dt))
  base_med_dt <- as.data.table(copy(base_med_dt))
  
  x_abs <- max(
    abs(c(
      ens_dt$avail_abs_change,
      ds_dt$avail_abs_change,
      base_med_dt$avail_med
    )),
    na.rm = TRUE
  ) * 1.5
  
  y_abs <- max(
    abs(c(
      ens_dt$flux_abs_change,
      ds_dt$flux_abs_change,
      base_med_dt$flux_med
    )),
    na.rm = TRUE
  ) * 1.5
  
  if (!is.finite(x_abs) || x_abs == 0) {
    x_abs <- 1
  }
  
  if (!is.finite(y_abs) || y_abs == 0) {
    y_abs <- 1
  }
  
  quad_fills <- list(
    list(
      xmin = -Inf,
      xmax = 0,
      ymin = 0,
      ymax = Inf,
      key = "drier-accelerated"
    ),
    list(
      xmin = 0,
      xmax = Inf,
      ymin = 0,
      ymax = Inf,
      key = "wetter-accelerated"
    ),
    list(
      xmin = -Inf,
      xmax = 0,
      ymin = -Inf,
      ymax = 0,
      key = "drier-decelerated"
    ),
    list(
      xmin = 0,
      xmax = Inf,
      ymin = -Inf,
      ymax = 0,
      key = "wetter-decelerated"
    )
  )
  
  pad <- 0.04
  
  quad_corners <- data.frame(
    x = c(
      -x_abs + pad * x_abs,
      x_abs - pad * x_abs,
      -x_abs + pad * x_abs,
      x_abs - pad * x_abs
    ),
    y = c(
      y_abs - pad * y_abs,
      y_abs - pad * y_abs,
      -y_abs + pad * y_abs,
      -y_abs + pad * y_abs
    ),
    hjust = c(0, 1, 0, 1),
    vjust = c(1, 1, 0, 0),
    label = c(
      "Dry & Accelerated",
      "Wet & Accelerated",
      "Dry & Decelerated",
      "Wet & Decelerated"
    ),
    key = c(
      "drier-accelerated",
      "wetter-accelerated",
      "drier-decelerated",
      "wetter-decelerated"
    ),
    stringsAsFactors = FALSE
  )
  
  quad_corners$colour <- STORY_COLS[quad_corners$key]
  
  ds_cols <- make_dataset_cols(ds_dt$dataset)
  
  diag_x <- -0.75 * min(x_abs, 1.8 * y_abs)
  
  p <- ggplot()
  
  for (q in quad_fills) {
    p <- p +
      annotate(
        "rect",
        xmin = q$xmin,
        xmax = q$xmax,
        ymin = q$ymin,
        ymax = q$ymax,
        fill = STORY_COLS[q$key],
        alpha = 0.11
      )
  }
  
  p <- p +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.35,
      colour = "grey35"
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.35,
      colour = "grey35"
    )
  
  p <- p +
    geom_point(
      data = ens_dt,
      aes(
        x = avail_abs_change,
        y = flux_abs_change
      ),
      colour = "grey80",
      alpha = 0.28,
      size = 2.2
    ) +
    geom_point(
      data = ens_dt[sig_story_either == TRUE],
      aes(
        x = avail_abs_change,
        y = flux_abs_change
      ),
      colour = "grey50",
      alpha = 0.40,
      size = 2.6
    ) +
    geom_point(
      data = ens_dt[sig_story_both == TRUE],
      aes(
        x = avail_abs_change,
        y = flux_abs_change
      ),
      shape = 0,
      size = 3.6,
      stroke = 0.6,
      colour = "grey30"
    )
  
  p <- p +
    geom_point(
      data = ds_dt,
      aes(
        x = avail_abs_change,
        y = flux_abs_change,
        colour = dataset
      ),
      size = 5.5,
      shape = 16
    ) +
    geom_point(
      data = ds_dt,
      aes(
        x = avail_abs_change,
        y = flux_abs_change
      ),
      size = 5.5,
      shape = 1,
      colour = "black"
    ) +
    geom_text_repel(
      data = ds_dt,
      aes(
        x = avail_abs_change,
        y = flux_abs_change,
        label = dataset
      ),
      size = 5.2,
      show.legend = FALSE,
      box.padding = 0.50,
      point.padding = 0.30,
      force = 1.5,
      max.overlaps = Inf,
      segment.colour = NA
    )
  
  p <- p +
    geom_point(
      data = base_med_dt,
      aes(
        x = avail_med,
        y = flux_med
      ),
      shape = 13,
      size = 7.0,
      stroke = 0.7,
      colour = "black"
    )
  
  for (i in seq_len(nrow(quad_corners))) {
    p <- p +
      annotate(
        "label",
        x = quad_corners$x[i],
        y = quad_corners$y[i],
        label = quad_corners$label[i],
        hjust = quad_corners$hjust[i],
        vjust = quad_corners$vjust[i],
        size = 4.8,
        fontface = "bold",
        colour = quad_corners$colour[i],
        fill = scales::alpha(quad_corners$colour[i], 0.15),
        label.size = 0.4,
        label.r = unit(0.15, "lines")
      )
  }
  
  p <- p +
    geom_abline(
      slope = -0.5,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "grey30"
    ) +
    geom_abline(
      slope = 0.5,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "grey30"
    ) +
    annotate(
      "text",
      x = diag_x,
      y = -0.5 * diag_x,
      label = expression(Delta * P == 0),
      size = 4.0,
      angle = -20,
      fontface = "bold",
      colour = "grey25"
    ) +
    annotate(
      "text",
      x = diag_x,
      y = 0.5 * diag_x,
      label = expression(Delta * E == 0),
      size = 4.0,
      angle = 20,
      fontface = "bold",
      colour = "grey25"
    )
  
  p <- p +
    annotate(
      "label",
      x = 0,
      y = 0.93 * y_abs,
      label = "ΔP > 0\nΔE > 0",
      size = 4.1,
      colour = "black",
      fill = "white",
      fontface = "bold",
      label.size = 0.35,
      label.r = unit(0.12, "lines"),
      vjust = 1
    ) +
    annotate(
      "label",
      x = 0,
      y = -0.93 * y_abs,
      label = "ΔP < 0\nΔE < 0",
      size = 4.1,
      colour = "black",
      fill = "white",
      fontface = "bold",
      label.size = 0.35,
      label.r = unit(0.12, "lines"),
      vjust = 0
    ) +
    annotate(
      "label",
      x = 0.90 * x_abs,
      y = 0,
      label = "ΔP > 0\nΔE < 0",
      size = 4.1,
      colour = "black",
      fill = "white",
      fontface = "bold",
      label.size = 0.35,
      label.r = unit(0.12, "lines"),
      hjust = 1
    ) +
    annotate(
      "label",
      x = -0.90 * x_abs,
      y = 0,
      label = "ΔP < 0\nΔE > 0",
      size = 4.1,
      colour = "black",
      fill = "white",
      fontface = "bold",
      label.size = 0.35,
      label.r = unit(0.12, "lines"),
      hjust = 0
    )
  
  cap <- paste0(
    "\u25a1 Both availability and flux period shifts significant (p < 0.05)",
    "  |  \u2297 Base ensemble median",
    "  |  Darker grey: either shift significant"
  )
  
  p <- p +
    scale_colour_manual(
      values = ds_cols,
      name = "Observational\ndataset"
    ) +
    scale_x_continuous(
      name = expression(Delta(P - E) ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma,
      limits = c(-x_abs, x_abs),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = expression(Delta(P + E) / 2 ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma,
      limits = c(-y_abs, y_abs),
      expand = c(0, 0)
    ) +
    labs(
      title = plot_title,
      caption = cap
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey93"),
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 13),
      plot.caption = element_text(
        size = 8,
        colour = "grey45",
        hjust = 0,
        margin = margin(t = 6)
      ),
      axis.title = element_text(size = 11)
    )
  
  p
}

# Analysis ====================================================================

scenario_levels <- unique(as.character(mc_global_metrics_scenarios$scenario))

scenario_levels <- c(
  "base",
  setdiff(scenario_levels, "base")
)

scenario_labels <- setNames(
  label_scenario(scenario_levels),
  scenario_levels
)

region_biome_area <- prepare_region_biome_area(
  twc_grid_classes = twc_grid_classes
)

dataset_global_year <- prepare_dataset_global_year(
  dataset_region_biome_year = dataset_region_biome_year,
  region_biome_area = region_biome_area
)

dataset_global_metrics <- estimate_dataset_global_metrics(
  dataset_global_year = dataset_global_year
)

ens_base <- prepare_avail_flux_dt(
  metric_dt = mc_global_metrics_base,
  id_cols = c("sim", "scenario")
)

ens_base <- add_storyline(
  dt = ens_base,
  significance_mode = SIGNIFICANCE_MODE
)

ens_scenarios <- prepare_avail_flux_dt(
  metric_dt = mc_global_metrics_scenarios,
  id_cols = c("sim", "scenario")
)

ens_scenarios <- add_storyline(
  dt = ens_scenarios,
  significance_mode = SIGNIFICANCE_MODE
)

ens_scenarios[
  ,
  scenario := factor(
    scenario,
    levels = scenario_levels
  )
]

ds_plot <- prepare_avail_flux_dt(
  metric_dt = dataset_global_metrics,
  id_cols = "dataset"
)

base_med <- ens_base[
  ,
  .(
    avail_med = median(avail_abs_change, na.rm = TRUE),
    flux_med = median(flux_abs_change, na.rm = TRUE)
  )
]

p_main <- build_avail_flux_scatter(
  ens_dt = ens_base,
  ds_dt = ds_plot,
  base_med_dt = base_med,
  plot_title = "Global availability versus flux change"
)

print(p_main)

# Donut plot ==================================================================

STORY_COLS_LIGHT <- setNames(
  vapply(
    STORY_COLS,
    lighten_col,
    character(1)
  ),
  names(STORY_COLS)
)

DONUT_FILL_VALS <- c(
  setNames(
    STORY_COLS,
    paste0(names(STORY_COLS), "_sig")
  ),
  setNames(
    STORY_COLS_LIGHT,
    paste0(names(STORY_COLS), "_nonsig")
  )
)

DONUT_FILL_LEVELS <- as.vector(
  rbind(
    paste0(STORY_LEVELS, "_sig"),
    paste0(STORY_LEVELS, "_nonsig")
  )
)

donut_all_dt <- prepare_donut_dt(
  dt = ens_scenarios
)

donut_all_dt[
  ,
  scenario_label := "All scenarios"
]

donut_scen_dt <- rbindlist(
  lapply(scenario_levels, function(s) {
    
    dt <- prepare_donut_dt(
      dt = ens_scenarios,
      scenario_filter = s
    )
    
    dt[
      ,
      scenario_label := scenario_labels[s]
    ]
    
    dt
  })
)

donut_long <- rbindlist(
  list(
    donut_all_dt,
    donut_scen_dt
  )
)

PANEL_LEVELS <- c(
  "All scenarios",
  unname(scenario_labels[scenario_levels])
)

donut_long[
  ,
  scenario_label := factor(
    scenario_label,
    levels = PANEL_LEVELS
  )
]

p_donuts <- ggplot(
  donut_long,
  aes(
    x = 2,
    y = frac,
    fill = fill_key
  )
) +
  geom_col(
    width = 0.85,
    colour = "white",
    linewidth = 0.35
  ) +
  coord_polar(
    theta = "y",
    start = 0
  ) +
  xlim(
    0.5,
    2.5
  ) +
  facet_wrap(
    ~ scenario_label,
    nrow = 3
  ) +
  scale_fill_manual(
    values = DONUT_FILL_VALS,
    breaks = paste0(STORY_LEVELS, "_nonsig"),
    labels = unname(STORY_LABELS[STORY_LEVELS]),
    drop = FALSE,
    name = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      keywidth = unit(0.70, "cm"),
      keyheight = unit(0.55, "cm"),
      label.theme = element_text(size = 9),
      override.aes = list(colour = NA)
    )
  ) +
  theme_void(base_size = 13) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 11,
      margin = margin(b = 3)
    ),
    legend.position = "bottom",
    legend.margin = margin(t = 4),
    legend.spacing.x = unit(0.20, "cm"),
    plot.title = element_text(
      face = "bold",
      size = 15,
      margin = margin(b = 1)
    ),
    plot.margin = margin(6, 6, 6, 6)
  )

p_combined <- p_main + p_donuts +
  plot_layout(
    widths = c(2, 1)
  )

print(p_combined)

# Outputs =====================================================================

ggsave(
  filename = file.path(
    FIG_DIR,
    "global_availability_flux_storyline_base_scenarios.png"
  ),
  plot = p_combined,
  width = 8.5,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    FIG_DIR,
    "global_availability_flux_storyline_base.png"
  ),
  plot = p_main,
  width = 6.5,
  height = 6.5,
  dpi = 300
)

saveRDS(
  ens_base,
  file.path(
    PATH_OUTPUT_DATA,
    "global_availability_flux_storyline_base_plot_data.Rds"
  )
)

saveRDS(
  ens_scenarios,
  file.path(
    PATH_OUTPUT_DATA,
    "global_availability_flux_storyline_scenarios_plot_data.Rds"
  )
)

saveRDS(
  dataset_global_metrics,
  file.path(
    PATH_OUTPUT_DATA,
    "dataset_global_metrics.Rds"
  )
)

# Validation ==================================================================

cat("\nSaved figures to:\n")
cat(
  file.path(
    FIG_DIR,
    "global_availability_flux_storyline_base_scenarios.png"
  ),
  "\n"
)

cat(
  file.path(
    FIG_DIR,
    "global_availability_flux_storyline_base.png"
  ),
  "\n"
)

cat("\nBase ensemble storyline distribution:\n")
print(
  ens_base[
    ,
    .N,
    by = .(storyline, sig_story_both)
  ][
    ,
    frac := N / sum(N)
  ][]
)

cat("\nScenario sensitivity storyline distribution:\n")
print(
  ens_scenarios[
    ,
    .N,
    by = .(scenario, storyline, sig_story_both)
  ][
    ,
    frac := N / sum(N),
    by = scenario
  ][
    order(scenario, storyline, sig_story_both)
  ]
)

cat("\nFinished global availability-flux storyline plotting.\n")