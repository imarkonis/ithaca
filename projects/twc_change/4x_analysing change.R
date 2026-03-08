# Libraries ====================================================================

library(data.table)
library(ggplot2)
source("source/twc_change.R")

# Inputs =======================================================================

twc_dataset <- readRDS(file.path(PATH_OUTPUT_DATA, "twc_dataset.Rds"))
REGION_CLASS <- readRDS(file.path(PATH_OUTPUT_DATA, "region_classes.Rds"))
water_energy_limited <- readRDS(file.path(PATH_OUTPUT_DATA,
                                          "ipcc_water_energy_limited.Rds"))

# Helpers ======================================================================

theme_twc <- function() {
  theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      axis.title = element_text(size = 12),
      axis.text = element_text(color = "black"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      legend.position = "top",
      legend.title = element_blank()
    )
}

# Constants & Variables ========================================================

plot_dt <- copy(twc_dataset)[scenario == "base"]

plot_dt <- merge(
  plot_dt,
  REGION_CLASS,
  by = "region",
  all.x = TRUE
)

plot_dt <- merge(
  plot_dt,
  water_energy_limited,
  by = "region",
  all.x = TRUE
)

plot_dt <- plot_dt[
  is.finite(prec_med)  & is.finite(evap_med)  &
    is.finite(avail_med) & is.finite(flux_med)  &
    is.finite(prec_lo)   & is.finite(prec_hi)   &
    is.finite(evap_lo)   & is.finite(evap_hi)   &
    is.finite(avail_lo)  & is.finite(avail_hi)  &
    is.finite(flux_lo)   & is.finite(flux_hi)
]

# outcome classes from avail-flux space
plot_dt[, outcome_class := fifelse(
  avail_med >= 0 & flux_med >= 0, "wetter\naccelerated",
  fifelse(
    avail_med < 0 & flux_med >= 0, "drier\naccelerated",
    fifelse(
      avail_med >= 0 & flux_med < 0, "wetter\ndecelerated",
      "drier\ndecelerated"
    )
  )
)]

plot_dt[, outcome_class := factor(
  outcome_class,
  levels = c(
    "wetter\naccelerated",
    "drier\naccelerated",
    "wetter\ndecelerated",
    "drier\ndecelerated"
  )
)]

# outcome classes from flux space
plot_dt[, flux_class := fifelse(
  flux_med >= 0, "accelerated", "decelerated"
)]

plot_dt[, flux_class := factor(
  flux_class,
  levels = c(
    "accelerated",
    "decelerated"
  )
)]

# plot limits
x_lim_af <- max(abs(c(plot_dt$avail_lo, plot_dt$avail_hi)), na.rm = TRUE)
y_lim_af <- max(abs(c(plot_dt$flux_lo,  plot_dt$flux_hi)),  na.rm = TRUE)

lim_pe <- max(abs(c(
  plot_dt$prec_lo, plot_dt$prec_hi,
  plot_dt$evap_lo, plot_dt$evap_hi
)), na.rm = TRUE)

# labels
lab_dt_af <- get_top_labels(plot_dt, xvar = "avail_med", yvar = "flux_med", n = 12)

# Functions ====================================================================

## 1) Main plot: Δ(P-E) vs Δ((P+E)/2) 
plot_avail_flux <- function(dt,
                            color_var = NULL,
                            facet_var = NULL,
                            color_values = NULL,
                            label_top_n = 12,
                            xlim = c(-80, 80),
                            ylim = c(-80, 80),
                            point_size = 2.8,
                            label_size = 3.2,
                            quadrant_size = 3.5) {
  
  stopifnot(is.data.table(dt) || is.data.frame(dt))
  dt <- as.data.table(copy(dt))
  
  req_cols <- c("region", "avail_med", "flux_med", "avail_lo", "avail_hi", "flux_lo", "flux_hi")
  miss <- setdiff(req_cols, names(dt))
  if (length(miss) > 0) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }
  
  dt <- dt[
    is.finite(avail_med) & is.finite(flux_med) &
      is.finite(avail_lo) & is.finite(avail_hi) &
      is.finite(flux_lo) & is.finite(flux_hi)
  ]
  
  lab_dt <- get_top_labels(dt, xvar = "avail_med", yvar = "flux_med", n = label_top_n)
  
  x_lim <- max(abs(xlim))
  y_lim <- max(abs(ylim))
  
  # base mapping
  if (is.null(color_var)) {
    p <- ggplot(dt, aes(x = avail_med, y = flux_med))
  } else {
    p <- ggplot(dt, aes(x = avail_med, y = flux_med, color = .data[[color_var]]))
  }
  
  p <- p +
    geom_hline(yintercept = 0, linetype = 2, color = "grey45", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey45", linewidth = 0.4) +
    
    geom_segment(
      aes(x = avail_lo, xend = avail_hi, y = flux_med, yend = flux_med),
      inherit.aes = FALSE,
      color = "grey70", linewidth = 0.5
    ) +
    geom_segment(
      aes(x = avail_med, xend = avail_med, y = flux_lo, yend = flux_hi),
      inherit.aes = FALSE,
      color = "grey70", linewidth = 0.5
    ) +
    
    geom_point(size = point_size) +
    
    annotate("text", x =  0.72 * x_lim, y =  0.92 * y_lim,
             label = "wetter\naccelerated", size = quadrant_size) +
    annotate("text", x = -0.72 * x_lim, y =  0.92 * y_lim,
             label = "drier\naccelerated", size = quadrant_size) +
    annotate("text", x =  0.72 * x_lim, y = -0.92 * y_lim,
             label = "wetter\ndecelerated", size = quadrant_size) +
    annotate("text", x = -0.72 * x_lim, y = -0.92 * y_lim,
             label = "drier\ndecelerated", size = quadrant_size) +
    
    coord_equal(
      xlim = xlim,
      ylim = ylim,
      expand = TRUE
    ) +
    labs(
      x = expression(Delta(P-E)),
      y = expression(Delta((P+E)/2))
    ) +
    theme_twc()
  
  if (!is.null(color_var) && !is.null(color_values)) {
    p <- p + scale_color_manual(values = color_values)
  }
  
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }
  
  p
}

## 2) Secondary plot: Δ(P) vs Δ(E) 
plot_prec_evap <- function(dt,
                           color_var = NULL,
                           facet_var = NULL,
                           color_values = NULL,
                           label_top_n = 12,
                           xlim = c(-80, 80),
                           ylim = c(-80, 80),
                           point_size = 2.8,
                           label_size = 3.2,
                           guide_line_size = 0.5,
                           zero_line_size = 0.4,
                           show_diag_pos = TRUE,
                           show_diag_neg = TRUE,
                           highlight_var = NULL,
                           highlight_values = NULL,
                           highlight_shape = 0,
                           highlight_size = 3.2,
                           highlight_stroke = 0.9,
                           highlight_color = "black") {
  
  stopifnot(is.data.table(dt) || is.data.frame(dt))
  dt <- as.data.table(copy(dt))
  
  req_cols <- c("region", "prec_med", "evap_med", "prec_lo", "prec_hi", "evap_lo", "evap_hi")
  miss <- setdiff(req_cols, names(dt))
  if (length(miss) > 0) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }
  
  dt <- dt[
    is.finite(prec_med) & is.finite(evap_med) &
      is.finite(prec_lo) & is.finite(prec_hi) &
      is.finite(evap_lo) & is.finite(evap_hi)
  ]
  
  lab_dt <- get_top_labels(dt, xvar = "prec_med", yvar = "evap_med", n = label_top_n)
  y_nudge <- 0.03 * max(abs(ylim))
  
  if (is.null(color_var)) {
    p <- ggplot(dt, aes(x = prec_med, y = evap_med))
  } else {
    p <- ggplot(dt, aes(x = prec_med, y = evap_med, color = .data[[color_var]]))
  }
  
  p <- p +
    geom_hline(yintercept = 0, linetype = 2, color = "grey45", linewidth = zero_line_size) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey45", linewidth = zero_line_size)
  
  if (show_diag_pos) {
    p <- p + geom_abline(slope = 1, intercept = 0, color = "grey55", linewidth = guide_line_size)
  }
  
  if (show_diag_neg) {
    p <- p + geom_abline(slope = -1, intercept = 0, color = "grey75", linewidth = guide_line_size * 0.8)
  }
  
  p <- p +
    geom_segment(
      aes(x = prec_lo, xend = prec_hi, y = evap_med, yend = evap_med),
      inherit.aes = FALSE,
      color = "grey70", linewidth = 0.5
    ) +
    geom_segment(
      aes(x = prec_med, xend = prec_med, y = evap_lo, yend = evap_hi),
      inherit.aes = FALSE,
      color = "grey70", linewidth = 0.5
    ) +
    geom_point(size = point_size)
  
  # optional binary highlight layer
  if (!is.null(highlight_var) && !is.null(highlight_values)) {
    dt_high <- dt[get(highlight_var) %in% highlight_values]
    
    p <- p +
      geom_point(
        data = dt_high,
        aes(x = prec_med, y = evap_med),
        inherit.aes = FALSE,
        shape = highlight_shape,
        size = highlight_size,
        stroke = highlight_stroke,
        color = highlight_color
      )
  }
  
  p <- p +
    coord_equal(
      xlim = xlim,
      ylim = ylim,
      expand = TRUE
    ) +
    labs(
      x = expression(Delta(P)),
      y = expression(Delta(E))
    ) +
    theme_twc()
  
  if (!is.null(color_var) && !is.null(color_values)) {
    p <- p + scale_color_manual(values = color_values)
  }
  
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }
  
  p
}

# Analysis =====================================================================

p3 <- plot_avail_flux(
  dt = plot_dt,
  color_var = "hemisphere",
  facet_var = "circulation",
  color_values = c(north = "#3C78D8", south = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80)
)

p3

p_pe3 <- plot_prec_evap(
  dt = plot_dt,
  color_var = "limited",
  facet_var = "circulation",
  color_values = c(water = "#3C78D8", energy = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80),
  highlight_var = "flux_class",
  highlight_values = "accelerated",
  highlight_shape = 0,
  highlight_size = 4,
  highlight_color = "black"
)

p_pe3

p_pe3 <- plot_prec_evap(
  dt = plot_dt,
  color_var = "limited",
  facet_var = "circulation",
  color_values = c(water = "#3C78D8", energy = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80),
  highlight_var = "hemisphere",
  highlight_values = "south",
  highlight_shape = 0,
  highlight_size = 4,
  highlight_color = "grey20"
)

p_pe3


# Outputs ======================================================================

ggsave(
  filename = file.path(PATH_FIGURES, "avail_flux_scatter_base.png"),
  plot = p_avail_flux,
  width = 8,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(PATH_FIGURES, "prec_evap_by_avail_flux_class_base.png"),
  plot = p_prec_evap_by_outcome,
  width = 10,
  height = 8,
  dpi = 300
)

p_avail_flux
p_prec_evap_by_outcome

# Validation ===================================================================