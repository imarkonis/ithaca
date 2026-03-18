# ============================================================================
# Plot global availability versus flux change for storyline analysis
#
# This script:
# 1. Prepares global avail-flux coordinates for Monte Carlo members, datasets,
#    scenario summaries, and top1 summaries
# 2. Plots the base scenario for the main paper
# 3. Plots all scenarios in a faceted supplementary figure
# 4. Summarizes the relative distribution of significant storyline classes
# ============================================================================

# Libraries ===================================================================

library(ggrepel)

source("source/twc_change.R")

# Inputs ======================================================================

ensemble_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "member_level_global_summary.Rds")
)

scenario_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_summary.Rds")
)

dataset_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_summary.Rds")
)

scenario_vs_top1_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_global_summary.Rds")
)

# Constants ===================================================================

THRES_SIGNIFICANCE <- 0.05

STORY_LEVELS <- c(
  "wetter-accelerated",
  "drier-accelerated",
  "wetter-decelerated",
  "drier-decelerated"
)

SCENARIO_LEVELS <- c(
  "base",
  "clim_dominant",
  "evap_dominant",
  "prec_dominant",
  "trend_dominant"
)

STORY_COLS <- c(
  "wetter-accelerated" = PALETTES$water_cycle_change[1],
  "drier-accelerated"  = PALETTES$water_cycle_change[3],
  "wetter-decelerated" = PALETTES$water_cycle_change[2],
  "drier-decelerated"  = PALETTES$water_cycle_change[4]
)

# Helpers =====================================================================

prepare_ensemble_plot_dt <- function(dt, thres_significance) {
  dt <- copy(as.data.table(dt))
  
  dt <- dt[
    source_type == "mc" &
      is.finite(avail_abs_change) &
      is.finite(flux_abs_change)
  ]
  
  dt[, sig_story_both := 
       avail_mk_p < thres_significance &
       flux_mk_p < thres_significance]
  
  dt[, sig_story_either := 
       avail_mk_p < thres_significance |
       flux_mk_p < thres_significance]
  
  dt[, scenario := factor(scenario, levels = SCENARIO_LEVELS)]
  
  dt
}

prepare_scenario_plot_dt <- function(dt) {
  dt <- copy(as.data.table(dt))
  
  dt <- dt[
    region == "GLOBAL" &
      is.finite(avail_abs_q50) &
      is.finite(flux_abs_q50),
    .(
      scenario,
      avail_med = avail_abs_q50,
      flux_med = flux_abs_q50
    )
  ]
  
  dt[, scenario := factor(scenario, levels = SCENARIO_LEVELS)]
  
  dt
}

prepare_top1_plot_dt <- function(dt) {
  dt <- copy(as.data.table(dt))
  
  dt <- dt[
    region == "GLOBAL" &
      is.finite(top1_avail_abs_change) &
      is.finite(top1_flux_abs_change),
    .(
      scenario,
      top1_avail = top1_avail_abs_change,
      top1_flux = top1_flux_abs_change
    )
  ]
  
  dt[, scenario := factor(scenario, levels = SCENARIO_LEVELS)]
  
  dt
}

prepare_dataset_plot_dt <- function(dt_dataset, scenarios) {
  dt_dataset <- copy(as.data.table(dt_dataset))
  
  out <- CJ(
    scenario = scenarios,
    dataset = sort(unique(dt_dataset$dataset))
  )[
    dt_dataset[, .(dataset, avail_abs_change, flux_abs_change)],
    on = "dataset"
  ][
    is.finite(avail_abs_change) & is.finite(flux_abs_change)
  ]
  
  out[, scenario := factor(scenario, levels = SCENARIO_LEVELS)]
  
  out
}

build_avail_flux_plot <- function(
    ens_dt,
    ds_dt,
    med_dt,
    top1_dt,
    scenario_filter = NULL,
    facet = FALSE,
    plot_title,
    plot_subtitle
) {
  if (!is.null(scenario_filter)) {
    ens_dt <- ens_dt[scenario %in% scenario_filter]
    ds_dt <- ds_dt[scenario %in% scenario_filter]
    med_dt <- med_dt[scenario %in% scenario_filter]
    top1_dt <- top1_dt[scenario %in% scenario_filter]
  }
  
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4) +
    
    geom_point(
      data = ens_dt,
      aes(x = avail_abs_change, y = flux_abs_change),
      color = "grey75",
      alpha = 0.35,
      size = 1.8
    ) +
    
    geom_point(
      data = ens_dt[sig_story_either == TRUE],
      aes(x = avail_abs_change, y = flux_abs_change),
      color = "grey50",
      alpha = 0.45,
      size = 1.9
    ) +
    
    geom_point(
      data = ens_dt[sig_story_both == TRUE],
      aes(x = avail_abs_change, y = flux_abs_change),
      shape = 0,
      size = 2.8,
      stroke = 0.8,
      color = "grey45"
    ) +
    
    geom_point(
      data = ds_dt,
      aes(x = avail_abs_change, y = flux_abs_change, color = dataset),
      size = 3
    ) +
    
    geom_text_repel(
      data = ds_dt,
      aes(
        x = avail_abs_change,
        y = flux_abs_change,
        label = dataset,
        color = dataset
      ),
      size = 3,
      show.legend = FALSE,
      box.padding = 0.4,
      point.padding = 0.3,
      force = 1.2,
      max.overlaps = Inf,
      segment.color = "grey50",
      segment.size = 0.3
    ) +
    
    geom_point(
      data = med_dt,
      aes(x = avail_med, y = flux_med),
      shape = 4,
      stroke = 1.5,
      size = 5,
      color = "black"
    ) +
    
    geom_point(
      data = top1_dt,
      aes(x = top1_avail, y = top1_flux),
      shape = 17,
      size = 4,
      color = "red3"
    ) +
    
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Availability change, Δ(P-E)",
      y = "Flux change, Δ(P+E)/2",
      color = "Dataset"
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  if (isTRUE(facet)) {
    p <- p + facet_wrap(~ scenario)
  }
  
  p
}

# Plot data ===================================================================

ens_plot <- prepare_ensemble_plot_dt(
  dt = ensemble_global,
  thres_significance = THRES_SIGNIFICANCE
)

med_plot <- prepare_scenario_plot_dt(scenario_global)

top1_plot <- prepare_top1_plot_dt(scenario_vs_top1_global)

ds_plot <- prepare_dataset_plot_dt(
  dt_dataset = dataset_global,
  scenarios = SCENARIO_LEVELS
)

# Main figure: base only ======================================================

p_base <- build_avail_flux_plot(
  ens_dt = ens_plot,
  ds_dt = ds_plot,
  med_dt = med_plot,
  top1_dt = top1_plot,
  scenario_filter = "base",
  facet = FALSE,
  plot_title = "Global availability versus flux change",
  plot_subtitle = paste(
    "Grey = ensemble members;",
    "darker grey = either availability or flux significant;",
    "grey squares = both significant;",
    "colored = datasets;",
    "black X = scenario median;",
    "red triangle = top1"
  )
)

print(p_base)

# Supplementary figure: all scenarios =========================================

p_supp <- build_avail_flux_plot(
  ens_dt = ens_plot,
  ds_dt = ds_plot,
  med_dt = med_plot,
  top1_dt = top1_plot,
  facet = TRUE,
  plot_title = "Global availability versus flux change by scenario",
  plot_subtitle = paste(
    "Grey = ensemble members;",
    "darker grey = either availability or flux significant;",
    "grey squares = both significant;",
    "colored = datasets;",
    "black X = scenario median;",
    "red triangle = top1"
  )
)

print(p_supp)

# Storyline distribution among significant members ============================

bar_dt <- copy(ens_plot)

bar_dt[, storyline := factor(storyline, levels = STORY_LEVELS)]
bar_dt <- bar_dt[sig_story_both == TRUE]

bar_dt <- bar_dt[
  ,
  .N,
  by = .(scenario, storyline)
]

bar_dt <- CJ(
  scenario = factor(SCENARIO_LEVELS, levels = SCENARIO_LEVELS),
  storyline = factor(STORY_LEVELS, levels = STORY_LEVELS)
)[bar_dt, on = .(scenario, storyline)]

bar_dt[is.na(N), N := 0L]
bar_dt[, frac := N / sum(N), by = scenario]
bar_dt[, label := fifelse(frac >= THRES_SIGNIFICANCE, sprintf("%.2f", frac), "")]

p_bar <- ggplot(bar_dt, aes(x = scenario, y = frac, fill = storyline)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = STORY_COLS, drop = FALSE) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Relative distribution of significant storyline classes by scenario",
    subtitle = "Only members with significant availability and flux trends (p < 0.05)",
    x = NULL,
    y = "Fraction of significant ensemble members",
    fill = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_bar)

# Save ========================================================================

ggsave(
  filename = file.path(PATH_FIGURES, "global_avail_flux_base.png"),
  plot = p_base,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(PATH_FIGURES, "global_avail_flux_by_scenario.png"),
  plot = p_supp,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(PATH_FIGURES, "global_storyline_distribution_significant.png"),
  plot = p_bar,
  width = 8,
  height = 5,
  dpi = 300
)