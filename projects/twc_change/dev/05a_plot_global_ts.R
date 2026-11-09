# ============================================================================
# Plot global yearly TWC storylines
#
# This script:
# 1. Builds a main figure using ensemble members from all soft scenarios pooled
# 2. Overlays the five deterministic soft-scenario yearly series in the main figure
# 3. Builds a supplementary figure with one panel set per soft scenario
# 4. Overlays original dataset yearly series and their mean in both figures
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

ensemble_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_yearly_prec_evap.Rds")
)

dataset_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_yearly_prec_evap.Rds")
)

scenario_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_yearly_prec_evap.Rds")
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

summarize_ensemble_envelope <- function(dt_ens_long, group_cols) {
  dt_ens_long[
    ,
    .(
      q05 = quantile(value, 0.05, na.rm = TRUE),
      q50 = quantile(value, 0.50, na.rm = TRUE),
      q95 = quantile(value, 0.95, na.rm = TRUE)
    ),
    by = group_cols
  ]
}

# Scenario groups ==============================================================

all_scenarios <- unique(scenario_global_yearly$scenario)

top1_scenarios <- grep("_top1$", all_scenarios, value = TRUE)

soft_scenarios <- setdiff(all_scenarios, top1_scenarios)

scenario_labels <- c(
  "base" = "Base",
  "clim_dominant" = "Climate dominated",
  "evap_dominant" = "Evaporation dominated",
  "prec_dominant" = "Precipitation dominated",
  "trend_dominant" = "Trend dominated"
)

scenario_colors <- c(
  "base" = "black",
  "clim_dominant" = "#1b9e77",
  "evap_dominant" = "#d95f02",
  "prec_dominant" = "#7570b3",
  "trend_dominant" = "#e7298a"
)

# Prepare long tables =========================================================

ens_long <- to_long_twc(
  ensemble_global_yearly[scenario %in% soft_scenarios],
  id_cols = c("scenario", "sim_id", "year")
)

sc_long <- to_long_twc(
  scenario_global_yearly[scenario %in% soft_scenarios],
  id_cols = c("scenario", "year", "n_sim")
)

ds_long <- to_long_twc(
  dataset_global_yearly,
  id_cols = c("dataset", "year")
)

ds_mean <- ds_long[
  ,
  .(value = mean(value, na.rm = TRUE)),
  by = .(year, variable)
]

sc_long[
  ,
  scenario := factor(
    scenario,
    levels = names(scenario_labels),
    labels = unname(scenario_labels)
  )
]

# Main figure data ============================================================

ens_all_sum <- summarize_ensemble_envelope(
  dt_ens_long = ens_long,
  group_cols = c("year", "variable")
)

# Supplementary figure data ===================================================

ens_by_scenario_sum <- summarize_ensemble_envelope(
  dt_ens_long = ens_long,
  group_cols = c("scenario", "year", "variable")
)

ens_by_scenario_sum[
  ,
  scenario := factor(
    scenario,
    levels = names(scenario_labels),
    labels = unname(scenario_labels)
  )
]

# Main plot ===================================================================

p_main <- ggplot() +
  geom_ribbon(
    data = ens_all_sum,
    aes(
      x = year,
      ymin = q05,
      ymax = q95
    ),
    fill = "grey70",
    alpha = 0.35
  ) +
  geom_line(
    data = ens_all_sum,
    aes(
      x = year,
      y = q50
    ),
    colour = "grey20",
    linewidth = 0.8
  ) +
  geom_line(
    data = ds_long,
    aes(
      x = year,
      y = value,
      group = dataset
    ),
    colour = "steelblue4",
    alpha = 0.20,
    linewidth = 0.25
  ) +
  geom_line(
    data = ds_mean,
    aes(
      x = year,
      y = value
    ),
    colour = "firebrick",
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ variable,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_colour_manual(values = scenario_colors[soft_scenarios]) +
  labs(
    title = "Global yearly TWC storylines: pooled ensemble across soft scenarios",
    x = NULL,
    y = NULL,
    colour = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_main
# Supplementary plot ==========================================================

p_supp <- ggplot() +
  geom_ribbon(
    data = ens_by_scenario_sum,
    aes(
      x = year,
      ymin = q05,
      ymax = q95
    ),
    fill = "grey70",
    alpha = 0.35
  ) +
  geom_line(
    data = ens_by_scenario_sum,
    aes(
      x = year,
      y = q50
    ),
    colour = "grey20",
    linewidth = 0.8
  ) +
  geom_line(
    data = ds_long,
    aes(
      x = year,
      y = value,
      group = dataset
    ),
    colour = "steelblue4",
    alpha = 0.20,
    linewidth = 0.25
  ) +
  geom_line(
    data = ds_mean,
    aes(
      x = year,
      y = value
    ),
    colour = "firebrick",
    linewidth = 0.9
  ) +
  geom_line(
    data = sc_long,
    aes(
      x = year,
      y = value
    ),
    colour = "black",
    linewidth = 0.8
  ) +
  facet_wrap(
    scenario ~  variable,
    nrow = length(levels(sc_long$scenario)),
    scales = "free_y"
  ) +
  labs(
    title = "Global yearly TWC storylines: soft scenarios",
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

p_supp
# Save ========================================================================

ggsave(
  file.path(PATH_FIGURES, "global_storylines_pooled_soft_scenarios.png"),
  p_main,
  width = 11,
  height = 8,
  dpi = 300
)

ggsave(
  file.path(PATH_FIGURES, "global_storylines_soft_scenarios_supplement.png"),
  p_supp,
  width = 18,
  height = 12,
  dpi = 300
)