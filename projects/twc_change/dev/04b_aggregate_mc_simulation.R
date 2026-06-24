# Functions ===================================================================

prepare_global_plot_data <- function(mc_global_year) {
  
  dt <- as.data.table(copy(mc_global_year))
  
  required_cols <- c("scenario", "year", "prec", "evap")
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "mc_global_year is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!"sim" %in% names(dt)) {
    dt[, sim := 1L]
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
  
  plot_dt[]
}

# Analysis ====================================================================

plot_dt <- prepare_global_plot_data(
  mc_global_year = mc_global_year
)

p_global_ts <- ggplot(
  plot_dt,
  aes(
    x = year,
    y = value,
    colour = scenario,
    group = interaction(sim, scenario)
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Global annual Monte Carlo time series",
    subtitle = "One ensemble member across weighting scenarios",
    x = NULL,
    y = "Annual mean",
    colour = "Scenario"
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
    )
  )


print(p_global_ts)
