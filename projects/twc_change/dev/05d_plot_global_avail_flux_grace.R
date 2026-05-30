# ============================================================================
# Plot global TWC change for top GRACE-consistent Monte Carlo simulations
#
# This script:
# 1. Reads the saved simulation-level GRACE agreement ranking
# 2. Reconstructs agreement, disagreement, and non-significant fractions
# 3. Re-ranks simulations across all scenarios by regional P - E agreement with
#    GRACE
# 4. Builds a local-to-global simulation ID crosswalk because the GRACE ranking
#    uses local simulation IDs, while the global ensemble table uses global IDs
# 5. Selects the top 100 GRACE-consistent simulations across all scenarios
# 6. Joins these simulations to global Monte Carlo TWC change summaries
# 7. Plots global availability change against flux change, with Monte Carlo
#    points coloured by scenario
# 8. Adds observational dataset points and the top-100 simulation centroid
# 9. Saves the selected simulations, crosswalk, global table, and figure
# ============================================================================


# Inputs ======================================================================

## Libraries ==================================================================

library(data.table)
library(ggplot2)
library(ggrepel)
library(scales)
library(grid)

source("source/twc_change.R")


## Data ========================================================================

sim_grace_long <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_simulation_rank.Rds")
)

ensemble_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "member_level_global_summary.Rds")
)

dataset_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_summary.Rds")
)

sim_grace_long <- as.data.table(sim_grace_long)
ensemble_global <- as.data.table(ensemble_global)
dataset_global <- as.data.table(dataset_global)


# Constants & Variables =======================================================

TOP_N_GRACE_SIMS <- 100L
THRES_SIGNIFICANCE <- 0.05

GRACE_CATEGORY_LEVELS <- c(
  "agreement",
  "disagreement",
  "non_significant"
)

SCENARIO_LEVELS <- c(
  "base",
  "clim_dominant",
  "evap_dominant",
  "prec_dominant",
  "trend_dominant"
)

SCENARIO_LABELS <- c(
  "base" = "Base",
  "clim_dominant" = "Climate dominated",
  "evap_dominant" = "Evaporation dominated",
  "prec_dominant" = "Precipitation dominated",
  "trend_dominant" = "Trend dominated"
)

SCENARIO_COLS <- c(
  "Base" = "#4D4D4D",
  "Climate dominated" = "#1B9E77",
  "Evaporation dominated" = "#D95F02",
  "Precipitation dominated" = "#7570B3",
  "Trend dominated" = "#E7298A"
)

STORY_LEVELS <- c(
  "wetter-accelerated",
  "drier-accelerated",
  "wetter-decelerated",
  "drier-decelerated"
)

STORY_COLS <- c(
  "wetter-accelerated" = PALETTES$water_cycle_change[1],
  "drier-accelerated" = PALETTES$water_cycle_change[3],
  "wetter-decelerated" = PALETTES$water_cycle_change[2],
  "drier-decelerated" = PALETTES$water_cycle_change[4]
)


# Functions ===================================================================

standardize_scenario <- function(x) {
  
  x0 <- trimws(as.character(x))
  x0 <- tolower(x0)
  x0 <- gsub("[[:space:]-]+", "_", x0)
  
  out <- x0
  
  out[x0 %chin% c("base")] <- "base"
  
  out[
    x0 %chin% c(
      "clim_dominant",
      "climate_dominant",
      "climate_dominated"
    )
  ] <- "clim_dominant"
  
  out[
    x0 %chin% c(
      "evap_dominant",
      "evaporation_dominant",
      "evaporation_dominated"
    )
  ] <- "evap_dominant"
  
  out[
    x0 %chin% c(
      "prec_dominant",
      "precipitation_dominant",
      "precipitation_dominated"
    )
  ] <- "prec_dominant"
  
  out[
    x0 %chin% c(
      "trend_dominant",
      "trend_dominated"
    )
  ] <- "trend_dominant"
  
  out
}


prepare_sim_grace_rank <- function(sim_grace_long) {
  
  dt <- copy(as.data.table(sim_grace_long))
  
  dt[
    ,
    `:=`(
      scenario_code = standardize_scenario(scenario),
      sim_id_local = as.integer(sim_id),
      category = as.character(category)
    )
  ]
  
  dt[category == "agreement_fraction", category := "agreement"]
  dt[category == "disagreement_fraction", category := "disagreement"]
  dt[category == "non_significant_fraction", category := "non_significant"]
  
  dt <- dt[
    category %in% GRACE_CATEGORY_LEVELS
  ]
  
  sim_rank <- dt[
    ,
    .(
      grace_agreement_fraction = sum(
        fifelse(category == "agreement", fraction, 0),
        na.rm = TRUE
      ),
      grace_disagreement_fraction = sum(
        fifelse(category == "disagreement", fraction, 0),
        na.rm = TRUE
      ),
      grace_non_significant_fraction = sum(
        fifelse(category == "non_significant", fraction, 0),
        na.rm = TRUE
      )
    ),
    by = .(scenario_code, sim_id_local)
  ]
  
  sim_rank[
    ,
    `:=`(
      scenario_label = SCENARIO_LABELS[scenario_code],
      simulation_id_local = paste(scenario_code, sim_id_local, sep = "_")
    )
  ]
  
  setorder(
    sim_rank,
    -grace_agreement_fraction,
    grace_disagreement_fraction,
    grace_non_significant_fraction,
    scenario_code,
    sim_id_local
  )
  
  sim_rank[
    ,
    grace_agreement_rank := seq_len(.N)
  ]
  
  setcolorder(
    sim_rank,
    c(
      "grace_agreement_rank",
      "scenario_code",
      "scenario_label",
      "sim_id_local",
      "simulation_id_local",
      "grace_agreement_fraction",
      "grace_disagreement_fraction",
      "grace_non_significant_fraction"
    )
  )
  
  sim_rank[]
}


build_sim_id_crosswalk <- function(ensemble_global) {
  
  ens <- copy(as.data.table(ensemble_global))
  
  ens[
    ,
    `:=`(
      scenario_code = standardize_scenario(scenario),
      sim_id_global = as.integer(sim_id)
    )
  ]
  
  sim_id_crosswalk <- unique(
    ens[
      ,
      .(
        scenario_code,
        sim_id_global
      )
    ]
  )
  
  sim_id_crosswalk[
    ,
    scenario_order := match(scenario_code, SCENARIO_LEVELS)
  ]
  
  setorder(
    sim_id_crosswalk,
    scenario_order,
    scenario_code,
    sim_id_global
  )
  
  sim_id_crosswalk[
    ,
    sim_id_local := seq_len(.N),
    by = scenario_code
  ]
  
  sim_id_crosswalk[
    ,
    scenario_label := SCENARIO_LABELS[scenario_code]
  ]
  
  sim_id_crosswalk[
    ,
    scenario_order := NULL
  ]
  
  setcolorder(
    sim_id_crosswalk,
    c(
      "scenario_code",
      "scenario_label",
      "sim_id_local",
      "sim_id_global"
    )
  )
  
  sim_id_crosswalk[]
}


prepare_top_grace_global <- function(
    ensemble_global,
    top_grace_sims,
    sim_id_crosswalk,
    thres_significance
) {
  
  ens <- copy(as.data.table(ensemble_global))
  top_sims <- copy(as.data.table(top_grace_sims))
  id_map <- copy(as.data.table(sim_id_crosswalk))
  
  ens[
    ,
    `:=`(
      scenario_code = standardize_scenario(scenario),
      sim_id_global = as.integer(sim_id)
    )
  ]
  
  top_sims[
    ,
    `:=`(
      scenario_code = standardize_scenario(scenario_code),
      sim_id_local = as.integer(sim_id_local)
    )
  ]
  
  top_sims_global <- merge(
    top_sims,
    id_map[
      ,
      .(
        scenario_code,
        sim_id_local,
        sim_id_global
      )
    ],
    by = c("scenario_code", "sim_id_local"),
    all.x = TRUE
  )
  
  if (any(is.na(top_sims_global$sim_id_global))) {
    stop(
      "Some top GRACE simulations could not be mapped from local to global sim_id."
    )
  }
  
  out <- merge(
    ens,
    top_sims_global[
      ,
      .(
        scenario_code,
        sim_id_global,
        sim_id_local,
        scenario_label,
        simulation_id_local,
        grace_agreement_rank,
        grace_agreement_fraction,
        grace_disagreement_fraction,
        grace_non_significant_fraction
      )
    ],
    by = c("scenario_code", "sim_id_global"),
    all = FALSE
  )
  
  if ("region" %in% names(out)) {
    out <- out[
      region == "GLOBAL"
    ]
  }
  
  out <- out[
    is.finite(avail_abs_change) &
      is.finite(flux_abs_change)
  ]
  
  out[
    ,
    `:=`(
      sig_story_both =
        avail_mk_p < thres_significance &
        flux_mk_p < thres_significance,
      
      sig_story_either =
        avail_mk_p < thres_significance |
        flux_mk_p < thres_significance,
      
      scenario_label = factor(
        scenario_label,
        levels = unname(SCENARIO_LABELS[SCENARIO_LEVELS])
      )
    )
  ]
  
  out[]
}


prepare_dataset_global <- function(dataset_global) {
  
  dt <- copy(as.data.table(dataset_global))
  
  if ("region" %in% names(dt)) {
    dt <- dt[
      region == "GLOBAL"
    ]
  }
  
  dt[
    is.finite(avail_abs_change) &
      is.finite(flux_abs_change),
    .(
      dataset,
      avail_abs_change,
      flux_abs_change
    )
  ]
}


build_top_grace_global_plot <- function(
    ens_dt,
    ds_dt,
    centroid_dt,
    top_n,
    scenario_count_text
) {
  
  x_abs <- max(
    abs(ens_dt$avail_abs_change),
    abs(ds_dt$avail_abs_change),
    na.rm = TRUE
  ) * 1.5
  
  y_abs <- max(
    abs(ens_dt$flux_abs_change),
    abs(ds_dt$flux_abs_change),
    na.rm = TRUE
  ) * 1.5
  
  quad_fills <- list(
    list(
      xmin = -Inf,
      xmax = 0,
      ymin = 0,
      ymax = Inf,
      storyline_key = "drier-accelerated"
    ),
    list(
      xmin = 0,
      xmax = Inf,
      ymin = 0,
      ymax = Inf,
      storyline_key = "wetter-accelerated"
    ),
    list(
      xmin = -Inf,
      xmax = 0,
      ymin = -Inf,
      ymax = 0,
      storyline_key = "drier-decelerated"
    ),
    list(
      xmin = 0,
      xmax = Inf,
      ymin = -Inf,
      ymax = 0,
      storyline_key = "wetter-decelerated"
    )
  )
  
  quad_corners <- data.table(
    x = c(
      -x_abs * 0.96,
      x_abs * 0.96,
      -x_abs * 0.96,
      x_abs * 0.96
    ),
    y = c(
      y_abs * 0.96,
      y_abs * 0.96,
      -y_abs * 0.96,
      -y_abs * 0.96
    ),
    hjust = c(0, 1, 0, 1),
    vjust = c(1, 1, 0, 0),
    label = c(
      "Dry & Accelerated",
      "Wet & Accelerated",
      "Dry & Decelerated",
      "Wet & Decelerated"
    ),
    storyline_key = c(
      "drier-accelerated",
      "wetter-accelerated",
      "drier-decelerated",
      "wetter-decelerated"
    )
  )
  
  quad_corners[
    ,
    colour := unname(STORY_COLS[storyline_key])
  ]
  
  p <- ggplot()
  
  for (q in quad_fills) {
    p <- p +
      annotate(
        "rect",
        xmin = q$xmin,
        xmax = q$xmax,
        ymin = q$ymin,
        ymax = q$ymax,
        fill = unname(STORY_COLS[q$storyline_key]),
        alpha = 0.11
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
    
    # Monte Carlo cloud, original grey styling
    geom_point(
      data = ens_dt,
      aes(
        x = avail_abs_change,
        y = flux_abs_change
      ),
      colour = "grey80",
      alpha = 0.35,
      size = 2.4
    ) +
    geom_point(
      data = ens_dt[sig_story_either == TRUE],
      aes(
        x = avail_abs_change,
        y = flux_abs_change
      ),
      colour = "grey45",
      alpha = 0.55,
      size = 2.8
    ) +
    geom_point(
      data = ens_dt[sig_story_both == TRUE],
      aes(
        x = avail_abs_change,
        y = flux_abs_change
      ),
      shape = 0,
      size = 3.8,
      stroke = 0.7,
      colour = "grey25"
    ) +
    
    # Observational datasets, original dataset colouring
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
      shape = 1
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
    ) +
    
    # Top-100 centroid
    geom_point(
      data = centroid_dt,
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
        fill = alpha(quad_corners$colour[i], 0.15),
        label.size = 0.4,
        label.r = unit(0.15, "lines")
      )
  }
  
  p <- p +
    annotate(
      "text",
      x = -0.85 * x_abs,
      y = -0.5 * -0.89 * x_abs,
      label = expression(Delta * P == 0),
      size = 4.0,
      angle = -20,
      fontface = "bold",
      colour = "grey25"
    ) +
    annotate(
      "text",
      x = -0.85 * x_abs,
      y = 0.5 * -0.81 * x_abs,
      label = expression(Delta * E == 0),
      size = 4.0,
      angle = 20,
      fontface = "bold",
      colour = "grey25"
    ) +
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
    ) +
    scale_colour_brewer(
      palette = "Set2",
      name = "Observational\ndataset"
    ) +
    scale_x_continuous(
      name = expression(Delta(P - E) ~ "[mm" ~ yr^{-1} * "]"),
      labels = comma,
      limits = c(-x_abs, x_abs),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = expression(Delta(P + E) / 2 ~ "[mm" ~ yr^{-1} * "]"),
      labels = comma,
      limits = c(-y_abs, y_abs),
      expand = c(0, 0)
    ) +
    labs(
      title = paste0(
        "Global availability versus flux change for top ",
        top_n,
        " GRACE-consistent simulations"
      ),
      caption = paste0(
        "Top simulations are ranked across all scenarios by regional P - E ",
        "agreement with GRACE. Agreement requires same slope sign and at ",
        "least one significant trend. Circle-X marks the top-100 centroid.\n",
        scenario_count_text
      )
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

## Reconstruct and rank simulations across all scenarios =======================

sim_grace_rank <- prepare_sim_grace_rank(sim_grace_long)

## Build local to global simulation ID crosswalk ===============================

sim_id_crosswalk <- build_sim_id_crosswalk(ensemble_global)

## Select top GRACE-consistent simulations =====================================

top_grace_sims <- sim_grace_rank[
  grace_agreement_rank <= TOP_N_GRACE_SIMS
]

## Prepare global Monte Carlo and observational dataset data ===================

ens_top100_global <- prepare_top_grace_global(
  ensemble_global = ensemble_global,
  top_grace_sims = top_grace_sims,
  sim_id_crosswalk = sim_id_crosswalk,
  thres_significance = THRES_SIGNIFICANCE
)

ds_global_plot <- prepare_dataset_global(dataset_global)

## Summaries ===================================================================

sim_id_crosswalk_summary <- sim_id_crosswalk[
  ,
  .(
    n = .N,
    local_min = min(sim_id_local),
    local_max = max(sim_id_local),
    global_min = min(sim_id_global),
    global_max = max(sim_id_global)
  ),
  by = scenario_label
][
  order(match(scenario_label, unname(SCENARIO_LABELS[SCENARIO_LEVELS])))
]

top_grace_scenario_counts <- top_grace_sims[
  ,
  .N,
  by = scenario_label
][
  order(match(scenario_label, unname(SCENARIO_LABELS[SCENARIO_LEVELS])))
]

ens_top100_scenario_counts <- ens_top100_global[
  ,
  .N,
  by = scenario_label
][
  order(match(scenario_label, unname(SCENARIO_LABELS[SCENARIO_LEVELS])))
]

scenario_count_text <- ens_top100_scenario_counts[
  ,
  paste0(
    "Plotted top-100 scenario composition: ",
    paste(
      paste0(as.character(scenario_label), " = ", N),
      collapse = "; "
    )
  )
]

top100_global_centroid <- ens_top100_global[
  ,
  .(
    avail_med = median(avail_abs_change, na.rm = TRUE),
    flux_med = median(flux_abs_change, na.rm = TRUE),
    grace_agreement_med = median(grace_agreement_fraction, na.rm = TRUE),
    grace_disagreement_med = median(grace_disagreement_fraction, na.rm = TRUE),
    grace_non_significant_med = median(
      grace_non_significant_fraction,
      na.rm = TRUE
    )
  )
]


# Outputs =====================================================================

saveRDS(
  sim_id_crosswalk,
  file.path(PATH_OUTPUT_DATA, "mc_sim_id_local_global_crosswalk.Rds")
)

saveRDS(
  sim_grace_rank,
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_simulation_rank_wide.Rds")
)

saveRDS(
  top_grace_sims,
  file.path(PATH_OUTPUT_DATA, "top100_grace_consistent_simulations.Rds")
)

saveRDS(
  ens_top100_global,
  file.path(PATH_OUTPUT_DATA, "top100_grace_consistent_global_change.Rds")
)


# Validation ==================================================================

print("Simulation ID crosswalk:")
print(sim_id_crosswalk_summary)

print("Top 100 selected simulations before joining:")
print(top_grace_scenario_counts)

print("Top 100 simulations after local to global sim_id mapping and joining:")
print(ens_top100_scenario_counts)

print("First 20 joined simulations:")
print(
  ens_top100_global[
    order(grace_agreement_rank),
    .(
      grace_agreement_rank,
      scenario_code,
      scenario_label,
      sim_id_local,
      sim_id_global,
      avail_abs_change,
      flux_abs_change
    )
  ][1:20]
)

print("Top 100 global centroid:")
print(top100_global_centroid)

p_top100_global_change <- build_top_grace_global_plot(
  ens_dt = ens_top100_global,
  ds_dt = ds_global_plot,
  centroid_dt = top100_global_centroid,
  top_n = TOP_N_GRACE_SIMS,
  scenario_count_text = scenario_count_text
)

print(p_top100_global_change)

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "global_avail_flux_top100_grace_consistent_simulations.png"
  ),
  plot = p_top100_global_change,
  width = 8.5,
  height = 6.5,
  dpi = 300
)