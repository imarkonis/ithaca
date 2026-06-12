# ============================================================================
# Inputs
# ============================================================================

library(data.table)
library(ggplot2)
library(scales)

top100_sim_summary <- readRDS(
  file.path(PATH_OUTPUT_DATA, "top100_grace_consistent_global_change.Rds")
)

member_region_all <- readRDS(
  file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds")
)

mc_region_grace <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_diagnostic_2002_2021.Rds")
)

if (!exists("PATH_OUTPUT_FIGURES")) {
  PATH_OUTPUT_FIGURES <- file.path(dirname(PATH_OUTPUT_DATA), "figures")
}

dir.create(PATH_OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# Constants & Variables
# ============================================================================

AVAIL_THRESHOLD <- 10
FLUX_THRESHOLD <- 5

POINT_SIZE <- 3.8
POINT_STROKE <- 1.4

SHAPE_VALUES <- c(
  "Availability Δ(P-E)" = 22,
  "Exchange Δ(P+E)" = 21
)

GRACE_COLORS <- c(
  "GRACE agreement"    = "black",
  "GRACE disagreement" = "#C51B35",
  "No GRACE signal"    = "grey70"
)
REGION_NAMES <- c(
  ARP = "Arabian Peninsula",
  CAF = "Central Africa",
  CAR = "Caribbean",
  CAU = "Central Australia",
  CNA = "Central N. America",
  EAU = "E. Australia",
  ECA = "E. Central Asia",
  EEU = "E. Europe",
  ENA = "E. N. America",
  NEAF = "NE Africa",
  NES = "NE S. America",
  NAU = "N. Australia",
  RFE = "Russian Far East",
  SAH = "Sahara",
  SAS = "S. Asia",
  SAU = "S. Australia",
  SCA = "S. Central America",
  SEA = "SE Asia",
  SEAF = "SE Africa",
  SES = "SE S. America",
  SWS = "SW S. America",
  TIB = "Tibetan Plateau",
  WCE = "W. & Central Europe",
  WSAF = "W. Southern Africa"
)

# ============================================================================
# Functions
# ============================================================================

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

nice_region_label <- function(region_code) {
  region_name <- unname(REGION_NAMES[region_code])
  region_name[is.na(region_name)] <- region_code[is.na(region_name)]
  paste0(region_name, " (", region_code, ")")
}

add_simulation_keys <- function(dt) {
  setDT(dt)

  if (!"scenario_code" %in% names(dt) && "scenario" %in% names(dt)) {
    dt[, scenario_code := scenario]
  }

  if (!"sim_id_global" %in% names(dt) && "sim_id" %in% names(dt)) {
    dt[, sim_id_global := sim_id]
  }

  if (!"simulation_id_local" %in% names(dt)) {
    dt[, simulation_id_local := paste0(scenario_code, "_", sim_id_global)]
  }

  invisible(dt)
}

check_required_columns <- function(dt, required_cols, object_name) {
  missing_cols <- setdiff(required_cols, names(dt))

  if (length(missing_cols) > 0) {
    stop(
      object_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  invisible(TRUE)
}

# ============================================================================
# Analysis
# ============================================================================

setDT(top100_sim_summary)
setDT(member_region_all)
setDT(mc_region_grace)

add_simulation_keys(top100_sim_summary)
add_simulation_keys(member_region_all)

check_required_columns(
  top100_sim_summary,
  c(
    "scenario_code",
    "sim_id_global",
    "simulation_id_local",
    "grace_agreement_rank",
    "grace_agreement_fraction",
    "grace_disagreement_fraction",
    "grace_non_significant_fraction"
  ),
  "top100_sim_summary"
)

check_required_columns(
  member_region_all,
  c(
    "scenario_code",
    "sim_id_global",
    "region",
    "avail_rel_change",
    "flux_rel_change"
  ),
  "member_region_all"
)

check_required_columns(
  mc_region_grace,
  c(
    "region",
    "grace_sig_95",
    "avail_agreement_fraction"
  ),
  "mc_region_grace"
)

top100_keys <- top100_sim_summary[
  ,
  .(
    scenario_code,
    sim_id_global,
    simulation_id_local,
    grace_agreement_rank,
    grace_agreement_fraction,
    grace_disagreement_fraction,
    grace_non_significant_fraction
  )
]

top100_keys <- unique(top100_keys, by = c("scenario_code", "sim_id_global"))

member_region_top100 <- merge(
  member_region_all,
  top100_keys,
  by = c("scenario_code", "sim_id_global"),
  all = FALSE,
  allow.cartesian = TRUE
)

member_region_top100 <- member_region_top100[
  region != "GLOBAL"
]

region_hotspot <- member_region_top100[
  ,
  .(
    n_sim = .N,

    avail_rel_change = safe_mean(avail_rel_change),
    flux_rel_change = safe_mean(flux_rel_change),

    avail_hotspot_fraction = mean(abs(avail_rel_change) > AVAIL_THRESHOLD, na.rm = TRUE),
    flux_hotspot_fraction = mean(abs(flux_rel_change) > FLUX_THRESHOLD, na.rm = TRUE),

    mean_grace_agreement_rank = safe_mean(grace_agreement_rank),
    mean_grace_agreement_fraction = safe_mean(grace_agreement_fraction),
    mean_grace_disagreement_fraction = safe_mean(grace_disagreement_fraction),
    mean_grace_non_significant_fraction = safe_mean(grace_non_significant_fraction)
  ),
  by = region
]

grace_status_dt <- mc_region_grace[
  ,
  .(
    region,
    grace_sig_95,
    regional_grace_agreement_fraction = avail_agreement_fraction
  )
]

grace_status_dt[
  ,
  grace_status := fifelse(
    grace_sig_95 == TRUE & regional_grace_agreement_fraction > 0.5,
    "GRACE agreement",
    fifelse(
      grace_sig_95 == TRUE & regional_grace_agreement_fraction <= 0.5,
      "GRACE disagreement",
      "No GRACE signal"
    )
  )
]

region_hotspot <- merge(
  region_hotspot,
  grace_status_dt[
    ,
    .(
      region,
      grace_sig_95,
      regional_grace_agreement_fraction,
      grace_status
    )
  ],
  by = "region",
  all.x = TRUE
)

region_hotspot[
  is.na(grace_status),
  grace_status := "No GRACE signal"
]

region_hotspot[
  ,
  avail_hotspot := abs(avail_rel_change) > AVAIL_THRESHOLD
]

region_hotspot[
  ,
  flux_hotspot := abs(flux_rel_change) > FLUX_THRESHOLD
]

plot_dt <- region_hotspot[
  avail_hotspot == TRUE | flux_hotspot == TRUE
]

plot_dt[
  ,
  order_value := pmax(
    abs(avail_rel_change) / AVAIL_THRESHOLD,
    abs(flux_rel_change) / FLUX_THRESHOLD,
    na.rm = TRUE
  )
]

plot_dt <- plot_dt[
  order(-order_value)
]

plot_dt[
  ,
  y := .I
]

plot_dt[
  ,
  region_label := nice_region_label(region)
]

plot_dt[
  ,
  avail_rank := frank(-abs(avail_rel_change), ties.method = "dense")
]

plot_dt[
  ,
  flux_rank := frank(-abs(flux_rel_change), ties.method = "dense")
]

segment_dt <- plot_dt[
  ,
  .(
    region,
    region_label,
    y,
    avail_rel_change,
    flux_rel_change,
    grace_status
  )
]

point_dt <- rbindlist(
  list(
    plot_dt[
      ,
      .(
        region,
        region_label,
        y,
        metric = "Availability Δ(P-E)",
        x = avail_rel_change,
        rank_value = avail_rank,
        grace_status
      )
    ],
    plot_dt[
      ,
      .(
        region,
        region_label,
        y,
        metric = "Exchange Δ(P+E)",
        x = flux_rel_change,
        rank_value = flux_rank,
        grace_status
      )
    ]
  ),
  use.names = TRUE
)

point_dt[
  ,
  metric := factor(
    metric,
    levels = c("Availability Δ(P-E)", "Exchange Δ(P+E)")
  )
]

point_dt[
  ,
  grace_status := factor(
    grace_status,
    levels = c(
      "GRACE agreement",
      "GRACE disagreement",
      "No GRACE signal"
    )
  )
]

x_min <- floor(
  min(c(segment_dt$avail_rel_change, segment_dt$flux_rel_change), na.rm = TRUE) / 5
) * 5

x_max <- ceiling(
  max(c(segment_dt$avail_rel_change, segment_dt$flux_rel_change), na.rm = TRUE) / 5
) * 5

x_pad <- 0.08 * (x_max - x_min)

# ============================================================================
# Outputs
# ============================================================================

p <- ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "grey55"
  ) +
  geom_segment(
    data = segment_dt,
    aes(
      x = avail_rel_change,
      xend = flux_rel_change,
      y = y,
      yend = y
    ),
    color = "grey72",
    linewidth = 0.5
  ) +
  geom_point(
    data = point_dt,
    aes(
      x = x,
      y = y,
      shape = metric,
      fill = rank_value,
      color = grace_status
    ),
    size = POINT_SIZE,
    stroke = POINT_STROKE
  ) +
  scale_shape_manual(
    values = SHAPE_VALUES,
    name = "Metric"
  ) +
  scale_color_manual(
    values = GRACE_COLORS,
    name = "GRACE status",
    drop = FALSE
  ) +
  scale_fill_viridis_c(
    option = "D",
    direction = -1,
    breaks = pretty(c(1, max(point_dt$rank_value, na.rm = TRUE)), n = 5),
    name = "Metric rank"
  ) +
  scale_y_reverse(
    breaks = plot_dt$y,
    labels = plot_dt$region_label,
    expand = expansion(add = 0.7)
  ) +
  scale_x_continuous(
    limits = c(x_min - x_pad, x_max + x_pad),
    breaks = pretty(c(x_min, x_max), n = 6),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    x = "Mean relative change across top 100 GRACE matching simulations, 1982-2021 (%)",
    y = NULL
  ) +
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(
      order = 2,
      override.aes = list(fill = "white", size = 3.8)
    ),
    fill = guide_colorbar(order = 3, reverse = FALSE)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.35),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

ggsave(
  file.path(PATH_OUTPUT_FIGURES, "top100_grace_hotspot_lollipop_regions.png"),
  p,
  width = 10.8,
  height = 6.8,
  dpi = 300
)

ggsave(
  file.path(PATH_OUTPUT_FIGURES, "top100_grace_hotspot_lollipop_regions.pdf"),
  p,
  width = 10.8,
  height = 6.8
)

saveRDS(
  plot_dt,
  file.path(PATH_OUTPUT_DATA, "top100_grace_hotspot_region_data.Rds")
)

saveRDS(
  point_dt,
  file.path(PATH_OUTPUT_DATA, "top100_grace_hotspot_point_data.Rds")
)

saveRDS(
  member_region_top100,
  file.path(PATH_OUTPUT_DATA, "member_region_top100_grace_matching_sims.Rds")
)

# ============================================================================
# Validation
# ============================================================================

print(p)

print(
  plot_dt[
    ,
    .(
      region,
      avail_rel_change,
      flux_rel_change,
      avail_hotspot,
      flux_hotspot,
      avail_rank,
      flux_rank,
      grace_status,
      regional_grace_agreement_fraction,
      n_sim
    )
  ]
)