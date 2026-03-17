# ------------------------------------------------------------------
# Inputs assumed already loaded:
#   ensemble_members_global
#   dataset_global
#   scenario_global
#   scenario_vs_top1_global
#
# Paper: base in main, the other four in supplementary
# ------------------------------------------------------------------

# Libraries ==================================================================

library(ggrepel)

source("source/twc_change.R")

# Inputs =====================================================================

ensemble_members_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_members_global_summary.Rds")
)

ensemble_members_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_members_region_summary.Rds")
)

scenario_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_summary.Rds")
)

scenario_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_region_summary.Rds")
)

dataset_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_summary.Rds")
)

dataset_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_summary.Rds")
)

scenario_vs_top1_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_global_summary.Rds")
)

scenario_vs_top1_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_region_summary.Rds")
)

# Constants & Variables ========================================================

thres_significance <- 0.05

# Ensemble members: Monte Carlo only
ens_plot <- copy(ensemble_members_global)[
  source_type == "mc" &
    is.finite(avail_abs_change) &
    is.finite(flux_abs_change)
]
ens_plot[, sig_story_both := 
     flux_mk_p < 0.01]
ens_plot[, sig_story_either := avail_mk_p < thres_significance | 
     flux_mk_p < thres_significance]

# Scenario medians from soft summaries
med_plot <- copy(scenario_global)[
  region == "GLOBAL" &
    is.finite(avail_abs_q50) &
    is.finite(flux_abs_q50),
  .(
    scenario,
    avail_med = avail_abs_q50,
    flux_med = flux_abs_q50
  )
]

# Top1 points from comparison table
top1_plot <- copy(scenario_vs_top1_global)[
  region == "GLOBAL" &
    is.finite(top1_avail_abs_change) &
    is.finite(top1_flux_abs_change),
  .(
    scenario,
    top1_avail = top1_avail_abs_change,
    top1_flux = top1_flux_abs_change
  )
]

# Dataset points repeated across scenario panels
scenarios <- sort(unique(ens_plot$scenario))

ds_plot <- CJ(
  scenario = scenarios,
  dataset = sort(unique(dataset_global$dataset))
)[
  dataset_global[, .(dataset, avail_abs_change, flux_abs_change)],
  on = "dataset"
][
  is.finite(avail_abs_change) & is.finite(flux_abs_change)
]

# Useful vertical offset for labels
y_nudge <- 0.02 * max(abs(c(
  ens_plot$flux_abs_change,
  ds_plot$flux_abs_change,
  med_plot$flux_med,
  top1_plot$top1_flux
)), na.rm = TRUE)

# Analysis =====================================================================
## Plot main
p <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4) +
  
  # Ensemble cloud
  geom_point(
    data = ens_plot[scenario == "base"],
    aes(x = avail_abs_change, y = flux_abs_change),
    color = "grey70",
    alpha = 0.35,
    size = 1.8
  ) +
  
  # Significant ensemble members (both) as grey squares
  geom_point(
    data = ens_plot[scenario == "base" & sig_story_both == TRUE],
    aes(x = avail_abs_change, y = flux_abs_change),
    shape = 0,
    size = 2.8,
    stroke = 0.8,
    color = "grey65"
  ) +
  
  # Significant ensemble members (either) as darker points
  geom_point(
    data = ens_plot[scenario == "base" & sig_story_either == TRUE],
    aes(x = avail_abs_change, y = flux_abs_change),
    alpha = 0.35,
    color = "grey50"
  ) +
  
  # Original datasets
  geom_point(
    data = ds_plot[scenario == "base"],
    aes(x = avail_abs_change, y = flux_abs_change, color = dataset),
    size = 3
  ) +
  geom_text_repel(
    data = ds_plot[scenario == "base"],
    aes(x = avail_abs_change, y = flux_abs_change, label = dataset, color = dataset),
    size = 3,
    show.legend = FALSE,
    box.padding = 0.4,
    point.padding = 0.3,
    force = 1.2,
    max.overlaps = Inf,
    segment.color = "grey50",
    segment.size = 0.3
  ) +
  
  # Ensemble median as black X
  geom_point(
    data = med_plot[scenario == "base"],
    aes(x = avail_med, y = flux_med),
    shape = 4,
    stroke = 1.5,
    size = 5,
    color = "black"
  ) +
  
  # Top1 as different color
  geom_point(
    data = top1_plot[scenario == "base"],
    aes(x = top1_avail, y = top1_flux),
    shape = 17,
    size = 4,
    color = "red3"
  ) +
  
  labs(
    title = "Global availability versus flux change",
    subtitle = "Grey = ensemble members, grey squares = significant ensemble members (both), 
    darker points = significant ensemble members (either), colored = datasets, 
    black X = ensemble median, red triangle = best performer",
    x = "Availability change, Δ(P-E)",
    y = "Flux change, Δ(P+E)/2",
    color = "Dataset"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(p)


## Statistical significance
dt <- copy(ensemble_members_global)
dt <- dt[source_type == "mc"]

# Define significance rule for storyline space:
# both availability and flux trends significant at 95% level
dt[, sig_story_both := avail_mk_p < thres_significance & 
     flux_mk_p < thres_significance]

dt[, sig_story_either := avail_mk_p < thres_significance | 
     flux_mk_p < thres_significance]

# Keep order fixed
story_levels <- c(
  "wetter-accelerated",
  "drier-accelerated",
  "wetter-decelerated",
  "drier-decelerated"
)

dt[, storyline := factor(storyline, levels = story_levels)]

# Count and convert to fractions within scenario
bar_dt <- dt[
  ,
  .N,
  by = .(scenario, storyline)
]

bar_dt <- CJ(
  scenario = sort(unique(dt$scenario)),
  storyline = factor(story_levels, levels = story_levels)
)[bar_dt, on = .(scenario, storyline)]

bar_dt[is.na(N), N := 0L]
bar_dt[, frac := N / sum(N), by = scenario]

# Optional labels for segments that are not tiny
bar_dt[, label := fifelse(frac >= thres_significance, sprintf("%.2f", frac), "")]

# Colors
story_cols <- c(
  "wetter-accelerated" = PALETTES$water_cycle_change[1],
  "drier-accelerated"  = PALETTES$water_cycle_change[3],
  "wetter-decelerated" = PALETTES$water_cycle_change[2],
  "drier-decelerated"  = PALETTES$water_cycle_change[4]
)

# Plot
p <- ggplot(bar_dt, aes(x = scenario, y = frac, fill = storyline)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = story_cols, drop = FALSE) +
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

print(p)

## Plot supplementary
ens_plot_base <- ens_plot[scenario == 'base']

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4) +
  
  # Ensemble cloud
  geom_point(
    data = ens_plot,
    aes(x = avail_abs_change, y = flux_abs_change),
    color = "grey70",
    alpha = 0.35,
    size = 1.8
  ) +
  
  # Original datasets
  geom_point(
    data = ds_plot,
    aes(x = avail_abs_change, y = flux_abs_change, color = dataset),
    size = 3
  ) +
  geom_text_repel(
    data = ds_plot,
    aes(x = avail_abs_change, y = flux_abs_change, label = dataset, color = dataset),
    size = 3,
    show.legend = FALSE,
    box.padding = 0.4,
    point.padding = 0.3,
    force = 1.2,
    max.overlaps = Inf,
    segment.color = "grey50",
    segment.size = 0.3
  ) +
  # Ensemble median as black X
  geom_point(
    data = med_plot,
    aes(x = avail_med, y = flux_med),
    shape = 4,
    stroke = 1.5,
    size = 5,
    color = "black"
  ) +
  
  # Top1 as different color
  geom_point(
    data = top1_plot,
    aes(x = top1_avail, y = top1_flux),
    shape = 17,
    size = 4,
    color = "red3"
  ) +
  
  facet_wrap(~ scenario) +
  labs(
    title = "Global availability versus flux change by scenario",
    subtitle = "Grey = ensemble members, colored = datasets, black X = ensemble median, red triangle = top1",
    x = "Availability change, Δ(P-E)",
    y = "Flux change, Δ(P+E)/2",
    color = "Dataset"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(p)
