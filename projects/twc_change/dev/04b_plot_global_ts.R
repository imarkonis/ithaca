# ============================================================================
# Plot global yearly TWC storylines for:
# 1. base scenario only
# 2. soft scenarios excluding base
# 3. top1 scenarios only
#
# Each plot shows:
# - ensemble q05 to q95 envelope
# - ensemble q50 line
# - deterministic scenario line
# - original dataset yearly series
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

library(data.table)
library(ggplot2)

# Inputs ======================================================================

ensemble_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_global_yearly.Rds")
)

dataset_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_yearly.Rds")
)

scenario_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_yearly.Rds")
)

# Helpers =====================================================================

add_twc_vars <- function(dt) {
  dt <- copy(as.data.table(dt))
  dt[, avail := prec - evap]
  dt[, flux := (prec + evap) / 2]
  dt
}

to_long_twc <- function(dt, id_cols) {
  melt(
    add_twc_vars(dt),
    id.vars = id_cols,
    measure.vars = c("prec", "evap", "avail", "flux"),
    variable.name = "variable",
    value.name = "value"
  )[
    ,
    variable := factor(
      variable,
      levels = c("prec", "evap", "avail", "flux"),
      labels = c("P", "E", "P-E", "(P+E)/2")
    )
  ]
}

summarize_ensemble_envelope <- function(dt_ens_long) {
  dt_ens_long[
    ,
    .(
      q05 = quantile(value, 0.05, na.rm = TRUE),
      q50 = quantile(value, 0.50, na.rm = TRUE),
      q95 = quantile(value, 0.95, na.rm = TRUE)
    ),
    by = .(scenario, year, variable)
  ]
}

make_global_plot <- function(
    scenario_subset,
    plot_title,
    ncol,
    dataset_alpha = 0.20
) {
  
  # Ensemble (for envelope only) ----------------------------------------------
  ens_long <- to_long_twc(
    ensemble_global_yearly[scenario %in% scenario_subset],
    id_cols = c("scenario", "sim_id", "year")
  )
  
  ens_sum <- ens_long[
    ,
    .(
      q05 = quantile(value, 0.05, na.rm = TRUE),
      q95 = quantile(value, 0.95, na.rm = TRUE)
    ),
    by = .(scenario, year, variable)
  ]
  
  # Scenario (black line) -----------------------------------------------------
  sc_long <- to_long_twc(
    scenario_global_yearly[scenario %in% scenario_subset],
    id_cols = c("scenario", "year", "n_sim")
  )
  
  # Dataset mean (red line) ---------------------------------------------------
  ds_long <- to_long_twc(
    dataset_global_yearly,
    id_cols = c("dataset", "year")
  )
  
  ds_mean <- ds_long[
    ,
    .(value = mean(value, na.rm = TRUE)),
    by = .(year, variable)
  ]
  
  # Plot ----------------------------------------------------------------------
  ggplot() +
    geom_ribbon(
      data = ens_sum,
      aes(
        x = year,
        ymin = q05,
        ymax = q95
      ),
      fill = "grey70",
      alpha = 0.35
    ) +
    
    # Scenario (black)
    geom_line(
      data = sc_long,
      aes(
        x = year,
        y = value
      ),
      colour = "black",
      linewidth = 0.8
    ) +
    
    # Dataset mean (red)
    geom_line(
      data = ds_mean,
      aes(
        x = year,
        y = value
      ),
      colour = "firebrick",
      linewidth = 0.8
    ) +
    
    # Optional: individual datasets (very light)
    geom_line(
      data = ds_long,
      aes(
        x = year,
        y = value,
        group = dataset
      ),
      colour = "steelblue4",
      alpha = dataset_alpha,
      linewidth = 0.25
    ) +
    
    facet_wrap(
      ~ scenario + variable,
      ncol = ncol,
      scales = "free_y"
    ) +
    labs(
      title = plot_title,
      x = NULL,
      y = NULL
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "grey95"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Scenario groups ==============================================================

all_scenarios <- unique(scenario_global_yearly$scenario)

base_scenario <- "base"

top1_scenarios <- grep("_top1$", all_scenarios, value = TRUE)

soft_scenarios <- setdiff(
  all_scenarios,
  c(base_scenario, top1_scenarios)
)

# Plots =======================================================================

# 1. Base only -> 2 x 2
p_base <- make_global_plot(
  scenario_subset = base_scenario,
  plot_title = "Global yearly TWC storylines: base scenario",
  ncol = 2
)

# 2. Other soft scenarios -> 4 x 4
# rows = 4 variables, cols = 4 scenarios
p_soft <- make_global_plot(
  scenario_subset = soft_scenarios,
  plot_title = "Global yearly TWC storylines: soft scenarios",
  ncol = length(soft_scenarios)
)

# 3. Top1 scenarios only -> 5 x 4
# rows = 4 variables, cols = number of top1 scenarios
p_top1 <- make_global_plot(
  scenario_subset = top1_scenarios,
  plot_title = "Global yearly TWC storylines: top1 scenarios",
  ncol = length(top1_scenarios)
)

# Save ========================================================================

ggsave(
  file.path(PATH_FIGURES, "global_storylines_base_2x2.png"),
  p_base,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  file.path(PATH_FIGURES, "global_storylines_soft_4x4.png"),
  p_soft,
  width = 16,
  height = 12,
  dpi = 300
)

ggsave(
  file.path(PATH_FIGURES, "global_storylines_top1_5x4.png"),
  p_top1,
  width = 20,
  height = 12,
  dpi = 300
)