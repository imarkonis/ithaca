# ============================================================================
# Extrapolate region-biome Monte Carlo metrics to grid cells
#
# This script:
# 1. Reads region-biome Monte Carlo metrics from the base 500-member ensemble
# 2. Reads the complete TWC grid
# 3. Summarizes ensemble metrics per region x biome x variable
# 4. Assigns region-biome summaries to all matching grid cells
# 5. Creates grid-ready metric and storyline objects for global maps
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

mc_region_biome_metrics_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_metrics_base.Rds")
)

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_complete_grid.Rds")
)

# Constants & Variables =======================================================

SAVE_MEMBER_LEVEL_GRID <- FALSE

STORY_LEVELS <- c(
  "wetter-accelerated",
  "drier-accelerated",
  "wetter-decelerated",
  "drier-decelerated"
)

# Functions ===================================================================

q_safe <- function(x, p) {
  
  x <- x[is.finite(x)]
  
  if (length(x) == 0L) {
    return(NA_real_)
  }
  
  unname(
    quantile(
      x,
      probs = p,
      na.rm = TRUE,
      type = 7
    )
  )
}

frac_safe <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0L) {
    return(NA_real_)
  }
  
  mean(x)
}

prepare_grid <- function(twc_grid) {
  
  grid_dt <- as.data.table(copy(twc_grid))
  
  required_cols <- c("lon", "lat", "region", "biome")
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
  
  grid_dt <- grid_dt[
    is.finite(lon) &
      is.finite(lat) &
      !is.na(region) &
      !is.na(biome)
  ]
  
  grid_dt <- unique(
    grid_dt,
    by = c("lon", "lat")
  )
  
  grid_dt[]
}

prepare_metrics <- function(mc_region_biome_metrics_base) {
  
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

summarise_metric_ensemble <- function(metrics_dt) {
  
  metric_summary <- metrics_dt[
    ,
    .(
      n_sims = uniqueN(sim),
      
      mean_1982_2001_q50 = q_safe(mean_1982_2001, 0.50),
      mean_2002_2021_q50 = q_safe(mean_2002_2021, 0.50),
      
      diff_q05 = q_safe(diff_2002_2021_minus_1982_2001, 0.05),
      diff_q25 = q_safe(diff_2002_2021_minus_1982_2001, 0.25),
      diff_q50 = q_safe(diff_2002_2021_minus_1982_2001, 0.50),
      diff_q75 = q_safe(diff_2002_2021_minus_1982_2001, 0.75),
      diff_q95 = q_safe(diff_2002_2021_minus_1982_2001, 0.95),
      diff_mean = mean(diff_2002_2021_minus_1982_2001, na.rm = TRUE),
      diff_sd = sd(diff_2002_2021_minus_1982_2001, na.rm = TRUE),
      diff_positive_frac = mean(
        diff_2002_2021_minus_1982_2001 > 0,
        na.rm = TRUE
      ),
      diff_negative_frac = mean(
        diff_2002_2021_minus_1982_2001 < 0,
        na.rm = TRUE
      ),
      diff_stat_sig_frac = frac_safe(diff_stat_sig),
      
      diff_percent_q50 = q_safe(diff_percent, 0.50),
      
      slope_q05 = q_safe(slope_full, 0.05),
      slope_q25 = q_safe(slope_full, 0.25),
      slope_q50 = q_safe(slope_full, 0.50),
      slope_q75 = q_safe(slope_full, 0.75),
      slope_q95 = q_safe(slope_full, 0.95),
      slope_mean = mean(slope_full, na.rm = TRUE),
      slope_sd = sd(slope_full, na.rm = TRUE),
      slope_positive_frac = mean(
        slope_full > 0,
        na.rm = TRUE
      ),
      slope_negative_frac = mean(
        slope_full < 0,
        na.rm = TRUE
      ),
      slope_stat_sig_frac = frac_safe(slope_full_stat_sig)
    ),
    by = .(scenario, region, biome, variable)
  ]
  
  if ("variable_label" %in% names(metrics_dt)) {
    
    labels_dt <- unique(
      metrics_dt[
        ,
        .(variable, variable_label)
      ]
    )
    
    metric_summary <- labels_dt[
      metric_summary,
      on = "variable"
    ]
  }
  
  setkey(
    metric_summary,
    scenario,
    region,
    biome,
    variable
  )
  
  metric_summary[]
}

prepare_storyline_members <- function(metrics_dt) {
  
  avail_dt <- metrics_dt[
    variable == "avail",
    .(
      sim,
      scenario,
      region,
      biome,
      avail_change = diff_2002_2021_minus_1982_2001,
      avail_diff_p = diff_p_value,
      avail_diff_sig = diff_stat_sig,
      avail_slope = slope_full,
      avail_slope_p = slope_full_p_value,
      avail_slope_sig = slope_full_stat_sig
    )
  ]
  
  flux_dt <- metrics_dt[
    variable == "flux",
    .(
      sim,
      scenario,
      region,
      biome,
      flux_change = diff_2002_2021_minus_1982_2001,
      flux_diff_p = diff_p_value,
      flux_diff_sig = diff_stat_sig,
      flux_slope = slope_full,
      flux_slope_p = slope_full_p_value,
      flux_slope_sig = slope_full_stat_sig
    )
  ]
  
  storyline_dt <- merge(
    avail_dt,
    flux_dt,
    by = c("sim", "scenario", "region", "biome"),
    all = FALSE
  )
  
  storyline_dt <- storyline_dt[
    is.finite(avail_change) &
      is.finite(flux_change)
  ]
  
  storyline_dt[
    ,
    storyline := fcase(
      avail_change >= 0 & flux_change >= 0,
      "wetter-accelerated",
      avail_change < 0 & flux_change >= 0,
      "drier-accelerated",
      avail_change >= 0 & flux_change < 0,
      "wetter-decelerated",
      avail_change < 0 & flux_change < 0,
      "drier-decelerated"
    )
  ]
  
  storyline_dt[
    ,
    `:=`(
      sig_story_both_diff = avail_diff_sig & flux_diff_sig,
      sig_story_either_diff = avail_diff_sig | flux_diff_sig,
      sig_story_both_slope = avail_slope_sig & flux_slope_sig,
      sig_story_either_slope = avail_slope_sig | flux_slope_sig
    )
  ]
  
  storyline_dt[
    ,
    storyline := factor(
      storyline,
      levels = STORY_LEVELS
    )
  ]
  
  storyline_dt[]
}

summarise_storyline_ensemble <- function(storyline_dt) {
  
  story_frac <- storyline_dt[
    ,
    .N,
    by = .(scenario, region, biome, storyline)
  ][
    ,
    frac := N / sum(N),
    by = .(scenario, region, biome)
  ]
  
  story_frac[
    ,
    storyline_key := gsub(
      "-",
      "_",
      as.character(storyline)
    )
  ]
  
  story_wide <- dcast(
    story_frac,
    scenario + region + biome ~ storyline_key,
    value.var = "frac",
    fill = 0
  )
  
  story_mode <- story_frac[
    order(-frac)
  ][
    ,
    .SD[1],
    by = .(scenario, region, biome)
  ][
    ,
    .(
      scenario,
      region,
      biome,
      story_mode = as.character(storyline),
      story_mode_frac = frac
    )
  ]
  
  story_sig <- storyline_dt[
    ,
    .(
      n_sims = uniqueN(sim),
      sig_story_both_diff_frac = frac_safe(sig_story_both_diff),
      sig_story_either_diff_frac = frac_safe(sig_story_either_diff),
      sig_story_both_slope_frac = frac_safe(sig_story_both_slope),
      sig_story_either_slope_frac = frac_safe(sig_story_either_slope),
      avail_change_q50 = q_safe(avail_change, 0.50),
      flux_change_q50 = q_safe(flux_change, 0.50),
      avail_slope_q50 = q_safe(avail_slope, 0.50),
      flux_slope_q50 = q_safe(flux_slope, 0.50)
    ),
    by = .(scenario, region, biome)
  ]
  
  out <- merge(
    story_mode,
    story_wide,
    by = c("scenario", "region", "biome"),
    all = TRUE
  )
  
  out <- merge(
    out,
    story_sig,
    by = c("scenario", "region", "biome"),
    all = TRUE
  )
  
  setkey(
    out,
    scenario,
    region,
    biome
  )
  
  out[]
}

join_summary_to_grid <- function(grid_dt, summary_dt) {
  
  missing_summary <- unique(
    grid_dt[, .(region, biome)]
  )[
    !unique(summary_dt[, .(region, biome)]),
    on = .(region, biome)
  ]
  
  if (nrow(missing_summary) > 0L) {
    cat("\nRegion x biome combinations in grid but missing from summary:\n")
    print(missing_summary)
    stop("Cannot assign metrics to all grid cells.")
  }
  
  out <- merge(
    grid_dt,
    summary_dt,
    by = c("region", "biome"),
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  
  setcolorder(
    out,
    c(
      "lon",
      "lat",
      "region",
      "biome",
      setdiff(
        names(out),
        c("lon", "lat", "region", "biome")
      )
    )
  )
  
  out[]
}

make_member_grid <- function(grid_dt, metrics_dt) {
  
  keep_cols <- c(
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
  
  metrics_light <- metrics_dt[
    ,
    keep_cols,
    with = FALSE
  ]
  
  out <- merge(
    grid_dt,
    metrics_light,
    by = c("region", "biome"),
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  
  setcolorder(
    out,
    c(
      "lon",
      "lat",
      "region",
      "biome",
      setdiff(
        names(out),
        c("lon", "lat", "region", "biome")
      )
    )
  )
  
  out[]
}

# Analysis ====================================================================

grid_dt <- prepare_grid(
  twc_grid = twc_grid
)

metrics_dt <- prepare_metrics(
  mc_region_biome_metrics_base = mc_region_biome_metrics_base
)

metric_summary <- summarise_metric_ensemble(
  metrics_dt = metrics_dt
)

storyline_members <- prepare_storyline_members(
  metrics_dt = metrics_dt
)

storyline_summary <- summarise_storyline_ensemble(
  storyline_dt = storyline_members
)

grid_metric_summary <- join_summary_to_grid(
  grid_dt = grid_dt,
  summary_dt = metric_summary
)

grid_storyline_summary <- join_summary_to_grid(
  grid_dt = grid_dt,
  summary_dt = storyline_summary
)

if (isTRUE(SAVE_MEMBER_LEVEL_GRID)) {
  
  grid_member_metrics <- make_member_grid(
    grid_dt = grid_dt,
    metrics_dt = metrics_dt
  )
}

# Outputs =====================================================================

saveRDS(
  metric_summary,
  file.path(
    PATH_OUTPUT_DATA,
    "mc_region_biome_metric_summary_base.Rds"
  )
)

saveRDS(
  storyline_summary,
  file.path(
    PATH_OUTPUT_DATA,
    "mc_region_biome_storyline_summary_base.Rds"
  )
)

saveRDS(
  grid_metric_summary,
  file.path(
    PATH_OUTPUT_DATA,
    "mc_grid_metric_summary_base.Rds"
  )
)

saveRDS(
  grid_storyline_summary,
  file.path(
    PATH_OUTPUT_DATA,
    "mc_grid_storyline_summary_base.Rds"
  )
)

if (isTRUE(SAVE_MEMBER_LEVEL_GRID)) {
  
  if (exists("write_fst")) {
    
    write_fst(
      x = grid_member_metrics,
      path = file.path(
        PATH_OUTPUT_DATA,
        "mc_grid_member_metrics_base.fst"
      ),
      compress = 50
    )
    
  } else {
    
    saveRDS(
      grid_member_metrics,
      file.path(
        PATH_OUTPUT_DATA,
        "mc_grid_member_metrics_base.Rds"
      )
    )
  }
}

# Validation ==================================================================

cat("\nSaved region-biome summaries:\n")
cat(
  file.path(
    PATH_OUTPUT_DATA,
    "mc_region_biome_metric_summary_base.Rds"
  ),
  "\n"
)
cat(
  file.path(
    PATH_OUTPUT_DATA,
    "mc_region_biome_storyline_summary_base.Rds"
  ),
  "\n"
)

cat("\nSaved grid-ready summaries:\n")
cat(
  file.path(
    PATH_OUTPUT_DATA,
    "mc_grid_metric_summary_base.Rds"
  ),
  "\n"
)
cat(
  file.path(
    PATH_OUTPUT_DATA,
    "mc_grid_storyline_summary_base.Rds"
  ),
  "\n"
)

cat("\nGrid metric summary preview:\n")
print(
  grid_metric_summary[
    order(lon, lat, variable)
  ][
    1:40
  ]
)

cat("\nGrid storyline summary preview:\n")
print(
  grid_storyline_summary[
    order(lon, lat)
  ][
    1:40
  ]
)

cat("\nMetric summary dimensions:\n")
print(
  data.table(
    object = c(
      "metric_summary",
      "storyline_summary",
      "grid_metric_summary",
      "grid_storyline_summary"
    ),
    n_rows = c(
      nrow(metric_summary),
      nrow(storyline_summary),
      nrow(grid_metric_summary),
      nrow(grid_storyline_summary)
    )
  )
)

cat("\nFinished extrapolating region-biome metrics to grid cells.\n")