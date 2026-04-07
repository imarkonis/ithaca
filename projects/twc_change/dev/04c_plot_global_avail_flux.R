# ============================================================================
# Plot global availability versus flux change for storyline analysis
#
# This script:
# 1. Prepares global avail-flux coordinates for Monte Carlo members, datasets,
#    scenario summaries, and top1 summaries
# 2. Plots all scenarios together in one main-panel figure
# 3. Plots one supplementary faceted figure with one panel per scenario
# 4. Summarizes the relative distribution of significant storyline classes
# ============================================================================

# Libraries ===================================================================

library(ggrepel)
library(patchwork)
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

# Constants (add to existing) =================================================

QUAD_FILL_ALPHA  <- 0.12
QUAD_LABEL_SIZE  <- 4.0
QUAD_LABEL_ALPHA <- 0.80
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

SCENARIO_LABELS <- c(
  "base" = "Base",
  "clim_dominant" = "Climate dominated",
  "evap_dominant" = "Evaporation dominated",
  "prec_dominant" = "Precipitation dominated",
  "trend_dominant" = "Trend dominated"
)

STORY_COLS <- c(
  "wetter-accelerated" = PALETTES$water_cycle_change[1],
  "drier-accelerated" = PALETTES$water_cycle_change[3],
  "wetter-decelerated" = PALETTES$water_cycle_change[2],
  "drier-decelerated" = PALETTES$water_cycle_change[4]
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

collapse_main_dataset_dt <- function(dt) {
  dt <- copy(as.data.table(dt))
  
  dt[
    ,
    .(
      avail_abs_change = median(avail_abs_change, na.rm = TRUE),
      flux_abs_change = median(flux_abs_change, na.rm = TRUE)
    ),
    by = dataset
  ]
}

collapse_main_median_dt <- function(dt) {
  dt <- copy(as.data.table(dt))
  
  dt[
    ,
    .(
      avail_med = median(avail_med, na.rm = TRUE),
      flux_med = median(flux_med, na.rm = TRUE)
    )
  ]
}

collapse_main_top1_dt <- function(dt) {
  dt <- copy(as.data.table(dt))
  
  dt[
    ,
    .(
      top1_avail = median(top1_avail, na.rm = TRUE),
      top1_flux = median(top1_flux, na.rm = TRUE)
    )
  ]
}

build_avail_flux_plot <- function(
    ens_dt,
    ds_dt,
    med_dt,
    top1_dt,
    scenario_filter = NULL,
    facet           = FALSE,
    collapse_main   = FALSE,
    annotate_quads  = FALSE,       # NEW
    plot_title,
    plot_subtitle   = NULL         # now optional; goes to caption when NULL
) {
  ens_dt  <- copy(as.data.table(ens_dt))
  ds_dt   <- copy(as.data.table(ds_dt))
  med_dt  <- copy(as.data.table(med_dt))
  top1_dt <- copy(as.data.table(top1_dt))
  
  if (!is.null(scenario_filter)) {
    ens_dt  <- ens_dt[scenario  %in% scenario_filter]
    ds_dt   <- ds_dt[scenario   %in% scenario_filter]
    med_dt  <- med_dt[scenario  %in% scenario_filter]
    top1_dt <- top1_dt[scenario %in% scenario_filter]
  }
  
  if (isTRUE(collapse_main) && !isTRUE(facet)) {
    ds_dt   <- collapse_main_dataset_dt(ds_dt)
    med_dt  <- collapse_main_median_dt(med_dt)
    top1_dt <- collapse_main_top1_dt(top1_dt)
  }
  
  if (isTRUE(facet)) {
    for (obj in c("ens_dt", "ds_dt", "med_dt", "top1_dt")) {
      dt <- get(obj)
      dt[, scenario_label := factor(
        SCENARIO_LABELS[as.character(scenario)],
        levels = unname(SCENARIO_LABELS[SCENARIO_LEVELS])
      )]
      assign(obj, dt)
    }
  }
  
  # ── base canvas ─────────────────────────────────────────────────────────────
  p <- ggplot()
  
  # ── quadrant shading ────────────────────────────────────────────────────────
  if (isTRUE(annotate_quads)) {
    QUAD_ALPHA <- 0.07
    p <- p +
      annotate("rect", xmin = -Inf, xmax = 0,   ymin = 0,    ymax =  Inf,
               fill = STORY_COLS["wetter-decelerated"], alpha = QUAD_ALPHA) +
      annotate("rect", xmin = 0,    xmax =  Inf, ymin = 0,    ymax =  Inf,
               fill = STORY_COLS["wetter-accelerated"],  alpha = QUAD_ALPHA) +
      annotate("rect", xmin = -Inf, xmax = 0,   ymin = -Inf, ymax =  0,
               fill = STORY_COLS["drier-decelerated"],   alpha = QUAD_ALPHA) +
      annotate("rect", xmin = 0,    xmax =  Inf, ymin = -Inf, ymax =  0,
               fill = STORY_COLS["drier-accelerated"],   alpha = QUAD_ALPHA)
  }
  
  # ── reference lines ─────────────────────────────────────────────────────────
  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey35") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey35")
  
  # ── ensemble cloud ───────────────────────────────────────────────────────────
  p <- p +
    geom_point(
      data  = ens_dt,
      aes(x = avail_abs_change, y = flux_abs_change),
      colour = "grey82", alpha = 0.30, size = 1.5
    ) +
    geom_point(
      data  = ens_dt[sig_story_either == TRUE],
      aes(x = avail_abs_change, y = flux_abs_change),
      colour = "grey55", alpha = 0.45, size = 1.8
    ) +
    geom_point(
      data   = ens_dt[sig_story_both == TRUE],
      aes(x  = avail_abs_change, y = flux_abs_change),
      shape  = 0, size = 2.6, stroke = 0.7, colour = "grey35"
    )
  
  # ── observational datasets ───────────────────────────────────────────────────
  p <- p +
    geom_point(
      data = ds_dt,
      aes(x = avail_abs_change, y = flux_abs_change, colour = dataset),
      size = 3.5, shape = 16
    ) +
    geom_text_repel(
      data = ds_dt,
      aes(x = avail_abs_change, y = flux_abs_change,
          label = dataset, colour = dataset),
      size         = 3.2,
      show.legend  = FALSE,
      box.padding  = 0.50,
      point.padding = 0.30,
      force        = 1.5,
      max.overlaps = Inf,
      segment.colour = "grey60",
      segment.size   = 0.3
    )
  
  # ── summary markers ──────────────────────────────────────────────────────────
  p <- p +
    geom_point(
      data   = med_dt,
      aes(x  = avail_med, y = flux_med),
      shape  = 4, stroke = 1.8, size = 5.0, colour = "black"
    ) +
    geom_point(
      data   = top1_dt,
      aes(x  = top1_avail, y = top1_flux),
      shape  = 17, size = 4.5, colour = "red3"
    )
  
  # ── quadrant corner labels ───────────────────────────────────────────────────
  if (isTRUE(annotate_quads)) {
    quad_ann <- data.frame(
      x      = c(-Inf,  Inf, -Inf,  Inf),
      y      = c( Inf,  Inf, -Inf, -Inf),
      label  = c("Wet & Decel.", "Wet & Accel.",
                 "Dry & Decel.", "Dry & Accel."),
      hjust  = c(-0.08,  1.08, -0.08,  1.08),
      vjust  = c( 1.60,  1.60, -0.60, -0.60),
      colour = unname(STORY_COLS[c(
        "wetter-decelerated", "wetter-accelerated",
        "drier-decelerated",  "drier-accelerated"
      )])
    )
    for (i in seq_len(nrow(quad_ann))) {
      p <- p + annotate(
        "text",
        x        = quad_ann$x[i],
        y        = quad_ann$y[i],
        label    = quad_ann$label[i],
        hjust    = quad_ann$hjust[i],
        vjust    = quad_ann$vjust[i],
        size     = 3.3,
        fontface = "italic",
        colour   = quad_ann$colour[i]
      )
    }
  }
  
  # ── scales & labels ──────────────────────────────────────────────────────────
  cap <- if (!is.null(plot_subtitle)) {
    plot_subtitle
  } else {
    paste0(
      "\u25a1 Both trends significant (p\u00a0<\u00a00.05)  \u2502  ",
      "\u2715 Scenario median  \u2502  ",
      "\u25b2 Top-1 member  \u2502  ",
      "Darker grey: either trend significant"
    )
  }
  
  p <- p +
    scale_x_continuous(
      name   = expression(Delta(P - E) ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma
    ) +
    scale_y_continuous(
      name   = expression(Delta(P + E) / 2 ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma
    ) +
    scale_colour_brewer(
      palette = "Set2",
      name    = "Observational\ndataset"
    ) +
    labs(
      title   = plot_title,
      caption = cap
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey93"),
      legend.position   = if (isTRUE(facet)) "bottom" else "right",
      legend.title      = element_text(size = 10, face = "bold"),
      legend.text       = element_text(size = 9),
      plot.title        = element_text(face = "bold", size = 13, hjust = 0),
      plot.caption      = element_text(size = 8, colour = "grey45", hjust = 0,
                                       margin = margin(t = 6)),
      axis.title        = element_text(size = 11)
    )
  
  if (isTRUE(facet)) {
    p <- p + facet_wrap(~scenario_label)
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

# Main figure: single-panel scatter ===========================================

build_avail_flux_scatter <- function(
    ens_dt,
    ds_dt,
    med_dt,
    plot_title = "Global availability vs. flux change"
) {
  ens_dt  <- copy(as.data.table(ens_dt))
  ds_dt   <- copy(as.data.table(ds_dt))
  med_dt  <- copy(as.data.table(med_dt))
  
  ds_dt  <- collapse_main_dataset_dt(ds_dt)
  med_dt <- collapse_main_median_dt(med_dt)
  
  # Symmetric axis limits so (0,0) sits at the visual centre
  x_abs <- max(abs(ens_dt$avail_abs_change), na.rm = TRUE) * 1.5
  y_abs <- max(abs(ens_dt$flux_abs_change),  na.rm = TRUE) * 1.5
  
  # ── correct quadrant mapping ──────────────────────────────────────────────
  # x > 0 → wetter  (ΔP-E > 0)   x < 0 → drier
  # y > 0 → accel   (ΔP+E > 0)   y < 0 → decel
  #
  #  top-left  : drier-accelerated    top-right : wetter-accelerated
  #  bot-left  : drier-decelerated    bot-right : wetter-decelerated
  
  quad_fills <- list(
    list(xmin=-Inf, xmax=0,   ymin=0,   ymax=Inf,  key="drier-accelerated"),
    list(xmin=0,   xmax=Inf,  ymin=0,   ymax=Inf,  key="wetter-accelerated"),
    list(xmin=-Inf, xmax=0,   ymin=-Inf, ymax=0,   key="drier-decelerated"),
    list(xmin=0,   xmax=Inf,  ymin=-Inf, ymax=0,   key="wetter-decelerated")
  )
  
  pad <- 0.04
  
  quad_corners <- data.frame(
    x      = c(-x_abs + pad * x_abs,  x_abs - pad * x_abs,
               -x_abs + pad * x_abs,  x_abs - pad * x_abs),
    y      = c( y_abs - pad * y_abs,  y_abs - pad * y_abs,
                -y_abs + pad * y_abs, -y_abs + pad * y_abs),
    hjust  = c(0,   1,   0,   1),   # left-align left labels, right-align right
    vjust  = c(1,   1,   0,   0),   # top-align top labels, bottom-align bottom
    label  = c("Dry & Accelerated", "Wet & Accelerated",
               "Dry & Decelerated", "Wet & Decelerated"),
    key    = c("drier-accelerated", "wetter-accelerated",
               "drier-decelerated", "wetter-decelerated"),
    stringsAsFactors = FALSE
  )
  quad_corners$colour <- STORY_COLS[quad_corners$key]
  
  p <- ggplot()
  
  # ── quadrant fills ────────────────────────────────────────────────────────
  for (q in quad_fills) {
    p <- p + annotate("rect",
                      xmin = q$xmin, xmax = q$xmax,
                      ymin = q$ymin, ymax = q$ymax,
                      fill  = STORY_COLS[q$key],
                      alpha = 0.11
    )
  }
  
  # ── reference lines ───────────────────────────────────────────────────────
  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed",
               linewidth = 0.4, colour = "grey35") +
    geom_vline(xintercept = 0, linetype = "dashed",
               linewidth = 0.4, colour = "grey35")
  
  # ── ensemble cloud ────────────────────────────────────────────────────────
  p <- p +
    geom_point(
      data   = ens_dt,
      aes(x  = avail_abs_change, y = flux_abs_change),
      colour = "grey80", alpha = 0.28, size = 2.2
    ) +
    geom_point(
      data   = ens_dt[sig_story_either == TRUE],
      aes(x  = avail_abs_change, y = flux_abs_change),
      colour = "grey50", alpha = 0.40, size = 2.6
    ) +
    geom_point(
      data   = ens_dt[sig_story_both == TRUE],
      aes(x  = avail_abs_change, y = flux_abs_change),
      shape  = 0, size = 3.6, stroke = 0.6, colour = "grey30"
    )
  
  # ── observational datasets ────────────────────────────────────────────────
  p <- p +
    geom_point(
      data = ds_dt,
      aes(x = avail_abs_change, y = flux_abs_change, colour = dataset),
      size = 5.5, shape = 16
    ) +
    geom_point(
      data = ds_dt,
      aes(x = avail_abs_change, y = flux_abs_change),
      size = 5.5, shape = 1
    ) +
    geom_text_repel(
      data = ds_dt,
      aes(x      = avail_abs_change,
          y      = flux_abs_change,
          label  = dataset),
      size           = 5.2,
      show.legend    = FALSE,
      box.padding    = 0.50,
      point.padding  = 0.30,
      force          = 1.5,
      max.overlaps   = Inf,
      segment.colour = NA       
    ) +
    
    # ── ensemble median ───────────────────────────────────────────────────────
    geom_point(
      data   = med_dt,
      aes(x  = avail_med, y = flux_med),
      shape  = 13,          # circle with X
      size   = 7.0,
      stroke = 0.7
    ) 
  
  # ── corner annotations ────────────────────────────────────────────────────
  for (i in seq_len(nrow(quad_corners))) {
    p <- p + annotate(
      "label",
      x          = quad_corners$x[i],
      y          = quad_corners$y[i],
      label      = quad_corners$label[i],
      hjust      = quad_corners$hjust[i],
      vjust      = quad_corners$vjust[i],
      size       = 4.8,
      fontface   = "bold",
      colour     = quad_corners$colour[i],
      fill       = scales::alpha(quad_corners$colour[i], 0.15),
      label.size = 0.4,
      label.r    = unit(0.15, "lines")
    )
  }
  
  # ── scales ────────────────────────────────────────────────────────────────
  p <- p +
    scale_colour_manual(
      values = c(
        STORY_COLS,
        setNames(
          RColorBrewer::brewer.pal(
            max(3, length(unique(ds_dt$dataset))), "Set2"
          )[seq_along(unique(ds_dt$dataset))],
          sort(unique(ds_dt$dataset))
        )
      ),
      breaks = sort(unique(ds_dt$dataset)),
      name   = "Observational\ndataset"
    ) +
    scale_x_continuous(
      name   = expression(Delta(P - E) ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma,
      limits = c(-x_abs, x_abs),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name   = expression(Delta(P + E) / 2 ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma,
      limits = c(-y_abs, y_abs),
      expand = c(0, 0)
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey93"),
      legend.position   = "none",          # ← removes legend entirely
      plot.title        = element_text(face = "bold", size = 13),
      plot.caption      = element_text(size = 8, colour = "grey45",
                                       hjust = 0, margin = margin(t = 6)),
      axis.title        = element_text(size = 11)
    )
  
  p
}

p_main <- build_avail_flux_scatter(
  ens_dt = ens_plot,
  ds_dt  = ds_plot,
  med_dt = med_plot
)

print(p_main)

# Bar plot: storyline distribution per scenario ===============================
lighten_col <- function(hex, amount = 0.55) {
  m <- col2rgb(hex) / 255
  l <- m + (1 - m) * amount
  rgb(l[1, ], l[2, ], l[3, ])
}

STORY_COLS_LIGHT <- setNames(
  vapply(STORY_COLS, lighten_col, character(1)),
  names(STORY_COLS)
)

DONUT_FILL_VALS <- c(
  setNames(STORY_COLS,       paste0(names(STORY_COLS), "_sig")),
  setNames(STORY_COLS_LIGHT, paste0(names(STORY_COLS), "_nonsig"))
)

DONUT_FILL_LEVELS <- c(
  rbind(
    paste0(STORY_LEVELS, "_sig"),
    paste0(STORY_LEVELS, "_nonsig")
  )
)

prepare_donut_dt <- function(dt, scenario_filter = NULL) {
  dt <- copy(as.data.table(dt))
  
  if (!is.null(scenario_filter)) {
    dt <- dt[scenario %in% scenario_filter]
  }
  
  dt <- dt[!is.na(storyline)]
  
  dt[, fill_key := paste0(
    as.character(storyline),
    ifelse(sig_story_both, "_sig", "_nonsig")
  )]
  
  out <- dt[, .N, by = fill_key]
  out[, frac := N / sum(N)]
  
  out <- merge(
    data.table(fill_key = DONUT_FILL_LEVELS),
    out, by = "fill_key", all.x = TRUE
  )
  out[is.na(N), `:=`(N = 0L, frac = 0)]
  out[, fill_key := factor(fill_key, levels = DONUT_FILL_LEVELS)]
  
  out
}

# Build a single long table with "all" + 5 scenarios ==========================

donut_all_dt <- prepare_donut_dt(ens_plot)
donut_all_dt[, scenario_label := "All scenarios"]

donut_scen_dt <- rbindlist(lapply(SCENARIO_LEVELS, function(s) {
  dt <- prepare_donut_dt(ens_plot, scenario_filter = s)
  dt[, scenario_label := SCENARIO_LABELS[s]]
  dt
}))

donut_long <- rbindlist(list(donut_all_dt, donut_scen_dt))

PANEL_LEVELS <- c("All scenarios", unname(SCENARIO_LABELS[SCENARIO_LEVELS]))
donut_long[, scenario_label := factor(scenario_label, levels = PANEL_LEVELS)]

# Single faceted donut ========================================================

p_donuts <- ggplot(donut_long, aes(x = 2, y = frac, fill = fill_key)) +
  geom_col(width = 0.85, colour = "white", linewidth = 0.35) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  facet_wrap(~scenario_label, nrow = 3) +
  scale_fill_manual(
    values = DONUT_FILL_VALS,
    breaks = paste0(STORY_LEVELS, "_nonsig"),   # ← only non-sig in legend
    labels = c(
      "Wet & Accelerated",
      "Dry & Accelerated",
      "Wet & Decelerated",
      "Dry & Decelerated"
    ),
    drop = FALSE,
    name = NULL
  ) + 
  guides(
    fill = guide_legend(
      nrow        = 2,
      byrow       = TRUE,
      keywidth    = unit(0.70, "cm"),
      keyheight   = unit(0.55, "cm"),
      label.theme = element_text(size = 9),
      override.aes = list(colour = NA)
    )
  ) +
  theme_void(base_size = 13) +
  theme(
    strip.text     = element_text(face = "bold", size = 11,
                                  margin = margin(b = 3)),
    legend.position   = "bottom",
    legend.margin     = margin(t = 4),
    legend.spacing.x  = unit(0.20, "cm"),
    plot.title        = element_text(face = "bold", size = 15,
                                     margin = margin(b = 1)),
    plot.margin       = margin(6, 6, 6, 6)
  )

p_combined <- p_main + p_donuts +
  plot_layout(widths = c(2, 1))

print(p_combined)

# Save ========================================================================

ggsave(
  filename = file.path(PATH_FIGURES, "global_avail_flux_all_scenarios.png"),
  plot     = p_combined,
  width    = 8.5, height = 6.5, dpi = 300
)

