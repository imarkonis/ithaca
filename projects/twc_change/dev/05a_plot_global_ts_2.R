# ============================================================================
# Plot global annual Monte Carlo time series
# ============================================================================

# Inputs ======================================================================

source("source/twc_change.R")

mc_global_year_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_year_base.Rds")
)

mc_global_year_scenarios <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_year_scenarios.Rds")
)

# Constants & Variables =======================================================

OUT_DIR <- if (exists("PATH_OUTPUT_FIGURES")) {
  PATH_OUTPUT_FIGURES
} else {
  PATH_OUTPUT_DATA
}

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

FIG_WIDTH <- 11
FIG_HEIGHT <- 7
FIG_DPI <- 300

# Functions ===================================================================

prepare_global_plot_data <- function(mc_global_year) {
  
  dt <- as.data.table(copy(mc_global_year))
  
  required_cols <- c("sim", "scenario", "year", "prec", "evap")
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "mc_global_year is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  dt[
    ,
    `:=`(
      avail = prec - evap,
      flux = (prec + evap) / 2
    )
  ]
  
  plot_dt <- melt(
    dt,
    id.vars = c("sim", "scenario", "year"),
    measure.vars = c("prec", "evap", "avail", "flux"),
    variable.name = "variable",
    value.name = "value"
  )
  
  plot_dt[
    ,
    variable := factor(
      variable,
      levels = c("prec", "evap", "avail", "flux"),
      labels = c(
        "Precipitation",
        "Evaporation",
        "Water availability",
        "Water flux"
      )
    )
  ]
  
  plot_dt[
    ,
    scenario := factor(scenario)
  ]
  
  setkey(
    plot_dt,
    scenario,
    variable,
    year,
    sim
  )
  
  plot_dt[]
}

summarise_global_plot_data <- function(plot_dt) {
  
  summary_dt <- plot_dt[
    ,
    .(
      value_p05 = quantile(value, 0.05, na.rm = TRUE),
      value_p25 = quantile(value, 0.25, na.rm = TRUE),
      value_med = median(value, na.rm = TRUE),
      value_p75 = quantile(value, 0.75, na.rm = TRUE),
      value_p95 = quantile(value, 0.95, na.rm = TRUE),
      n_sims = uniqueN(sim)
    ),
    by = .(scenario, year, variable)
  ]
  
  setkey(
    summary_dt,
    scenario,
    variable,
    year
  )
  
  summary_dt[]
}

plot_global_mc_summary <- function(
    summary_dt,
    plot_title,
    plot_subtitle = NULL,
    show_ribbons = TRUE
) {
  
  p <- ggplot(
    summary_dt,
    aes(
      x = year,
      colour = scenario,
      fill = scenario
    )
  )
  
  if (show_ribbons) {
    
    p <- p +
      geom_ribbon(
        aes(
          ymin = value_p05,
          ymax = value_p95
        ),
        alpha = 0.10,
        colour = NA
      ) +
      geom_ribbon(
        aes(
          ymin = value_p25,
          ymax = value_p75
        ),
        alpha = 0.20,
        colour = NA
      )
  }
  
  p <- p +
    geom_line(
      aes(
        y = value_med
      ),
      linewidth = 0.8
    ) +
    facet_wrap(
      ~ variable,
      scales = "free_y",
      ncol = 2
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = "Annual mean",
      colour = "Scenario",
      fill = "Scenario"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      strip.background = element_rect(
        fill = "grey90",
        colour = NA
      ),
      panel.grid.minor = element_blank()
    ) +
    guides(
      colour = guide_legend(
        nrow = 2,
        byrow = TRUE
      ),
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    )
  
  p
}

# Analysis ====================================================================

plot_dt_base <- prepare_global_plot_data(
  mc_global_year = mc_global_year_base
)

summary_dt_base <- summarise_global_plot_data(
  plot_dt = plot_dt_base
)

plot_dt_scenarios <- prepare_global_plot_data(
  mc_global_year = mc_global_year_scenarios
)

summary_dt_scenarios <- summarise_global_plot_data(
  plot_dt = plot_dt_scenarios
)

p_global_base <- plot_global_mc_summary(
  summary_dt = summary_dt_base,
  plot_title = "Global annual Monte Carlo time series",
  plot_subtitle = "Base weighting scenario, 500 ensemble members",
  show_ribbons = TRUE
)

p_global_scenarios <- plot_global_mc_summary(
  summary_dt = summary_dt_scenarios,
  plot_title = "Global annual Monte Carlo time series",
  plot_subtitle = "Weighting scenario sensitivity, 100 ensemble members per scenario",
  show_ribbons = FALSE
)


# Validation ==================================================================

print(p_global_base)

print(p_global_scenarios)

cat("\nBase summary preview:\n")
print(
  summary_dt_base[
    order(scenario, variable, year)
  ][
    1:40
  ]
)

cat("\nScenario sensitivity summary preview:\n")
print(
  summary_dt_scenarios[
    order(scenario, variable, year)
  ][
    1:40
  ]
)

cat("\nSaved figures to:\n")
cat(file.path(OUT_DIR, "global_mc_time_series_base.png"), "\n")
cat(file.path(OUT_DIR, "global_mc_time_series_scenarios.png"), "\n")

cat("\nFinished global Monte Carlo time series plotting.\n")