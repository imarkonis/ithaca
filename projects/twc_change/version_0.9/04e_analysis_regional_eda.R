#Supplementary

ensemble_region_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_members_region_summary.Rds")
)


library(data.table)
library(ggplot2)

# Ensemble members: Monte Carlo only
ens_plot <- copy(ensemble_members_region)[
  source_type == "mc" &
    is.finite(avail_abs_change) &
    is.finite(flux_abs_change)
]

# Scenario medians
med_plot <- copy(scenario_region)[
  is.finite(avail_abs_q50) &
    is.finite(flux_abs_q50),
  .(
    scenario,
    region,
    avail_med = avail_abs_q50,
    flux_med = flux_abs_q50
  )
]

# Top1 points
top1_plot <- copy(scenario_vs_top1_region)[
  is.finite(top1_avail_abs_change) &
    is.finite(top1_flux_abs_change),
  .(
    scenario,
    region,
    top1_avail = top1_avail_abs_change,
    top1_flux = top1_flux_abs_change
  )
]

# Dataset points
# dataset_region has no scenario column, so we repeat each region-dataset point
# across all scenario panels
scenarios <- sort(unique(ens_plot$scenario))

ds_base <- copy(dataset_region)[
  is.finite(avail_abs_change) &
    is.finite(flux_abs_change),
  .(
    region,
    dataset,
    avail_abs_change,
    flux_abs_change
  )
]

ds_plot <- rbindlist(lapply(scenarios, function(scn) {
  tmp <- copy(ds_base)
  tmp[, scenario := scn]
  tmp[]
}), use.names = TRUE)

# Optional label offset per scenario
y_nudge <- 0.02 * max(abs(c(
  ens_plot$flux_abs_change,
  ds_plot$flux_abs_change,
  med_plot$flux_med,
  top1_plot$top1_flux
)), na.rm = TRUE)

for (scn in sort(unique(ens_plot$scenario))) {
  
  p_scn <- ggplot() +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4) +
    
    # Ensemble members
    geom_point(
      data = ens_plot[scenario == scn],
      aes(x = avail_abs_change, y = flux_abs_change),
      color = "grey70",
      alpha = 0.35,
      size = 1.5
    ) +
    
    # Dataset points
    geom_point(
      data = ds_plot[scenario == scn],
      aes(x = avail_abs_change, y = flux_abs_change, color = dataset),
      size = 2.5
    ) +
    
    # Optional dataset labels
    geom_text(
      data = ds_plot[scenario == scn],
      aes(x = avail_abs_change, y = flux_abs_change, label = dataset, color = dataset),
      nudge_y = y_nudge,
      size = 2.3,
      show.legend = FALSE
    ) +
    
    # Soft median
    geom_point(
      data = med_plot[scenario == scn],
      aes(x = avail_med, y = flux_med),
      shape = 4,
      stroke = 1.3,
      size = 4.5,
      color = "black"
    ) +
    
    # Top1
    geom_point(
      data = top1_plot[scenario == scn],
      aes(x = top1_avail, y = top1_flux),
      shape = 17,
      size = 3.5,
      color = "red3"
    ) +
    
    facet_wrap(~ region, scales = "free") +
    labs(
      title = paste("Regional availability versus flux change:", scn),
      subtitle = "Grey = ensemble members, colored = datasets, black X = ensemble median, red triangle = top1",
      x = "Availability change, Δ(P-E)",
      y = "Flux change, Δ(P+E)/2",
      color = "Dataset"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  print(p_scn)
}
