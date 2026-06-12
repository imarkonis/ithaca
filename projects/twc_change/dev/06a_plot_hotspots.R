source("source/twc_change.R")

# ============================================================================
# Inputs
# ============================================================================

mc_region_grace <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_diagnostic_2002_2021.Rds")
)

region_summary <- readRDS(
  file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds")
)


# ============================================================================
# Constants & Variables
# ============================================================================

AVAIL_THRESHOLD <- 10
FLUX_THRESHOLD  <- 5

SHAPE_VALUES <- c(
  "Availability Δ(P-E)" = 22,
  "Exchange Δ(P+E)"     = 21
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

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

nice_region_label <- function(region_code) {
  region_name <- unname(REGION_NAMES[region_code])
  region_name[is.na(region_name)] <- region_code[is.na(region_name)]
  paste0(region_name, " (", region_code, ")")
}

# ============================================================================
# Analysis
# ============================================================================

setDT(mc_region_grace)
setDT(region_summary)

region_change <- region_summary[
  ,
  .(
    avail_rel_change = safe_median(avail_rel_change),
    flux_rel_change  = safe_median(flux_rel_change)
  ),
  by = region
]

grace_info <- mc_region_grace[
  ,
  .(
    region,
    grace_sig_95,
    avail_agreement_fraction
  )
]

plot_dt <- merge(region_change, grace_info, by = "region", all.x = TRUE)

plot_dt[
  ,
  grace_status := fifelse(
    grace_sig_95 == TRUE & avail_agreement_fraction > 0.5,
    "GRACE agreement",
    fifelse(
      grace_sig_95 == TRUE & avail_agreement_fraction <= 0.5,
      "GRACE disagreement",
      "No GRACE signal"
    )
  )
]

plot_dt[
  ,
  avail_hotspot := abs(avail_rel_change) > AVAIL_THRESHOLD
]

plot_dt[
  ,
  flux_hotspot := abs(flux_rel_change) > FLUX_THRESHOLD
]

plot_dt <- plot_dt[
  avail_hotspot == TRUE | flux_hotspot == TRUE
]

plot_dt[
  ,
  order_value := pmax(
    fifelse(avail_hotspot, abs(avail_rel_change), NA_real_),
    fifelse(flux_hotspot,  abs(flux_rel_change),  NA_real_),
    na.rm = TRUE
  )
]

plot_dt <- plot_dt[order(-order_value)]
plot_dt[, y := .I]
plot_dt[, region_label := nice_region_label(region)]

plot_dt[
  avail_hotspot == TRUE,
  avail_rank := frank(-abs(avail_rel_change), ties.method = "dense")
]

plot_dt[
  flux_hotspot == TRUE,
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

point_dt <- rbindlist(list(
  plot_dt[
    avail_hotspot == TRUE,
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
    flux_hotspot == TRUE,
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
), use.names = TRUE)

point_dt[
  ,
  metric := factor(metric, levels = c("Availability Δ(P-E)", "Exchange Δ(P+E)"))
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

rank_max <- max(point_dt$rank_value, na.rm = TRUE)

x_min <- floor(min(c(segment_dt$avail_rel_change, segment_dt$flux_rel_change), na.rm = TRUE) / 5) * 5
x_max <- ceiling(max(c(segment_dt$avail_rel_change, segment_dt$flux_rel_change), na.rm = TRUE) / 5) * 5

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
    color = "grey70",
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
    size = 3.5,
    stroke = 1.2
  ) +
  scale_shape_manual(
    values = SHAPE_VALUES,
    name = "Metric"
  ) +
  scale_color_manual(
    values = GRACE_COLORS,
    name = "GRACE status"
  ) +
  scale_fill_viridis_b(
    option = "D",
    direction = -1,
    breaks = sort(unique(point_dt$rank_value)),
    name = "Rank"
  ) +
  scale_y_continuous(
    breaks = plot_dt$y,
    labels = plot_dt$region_label,
    trans = "reverse",
    expand = expansion(add = 0.6)
  ) +
  scale_x_continuous(
    limits = c(x_min, x_max),
    breaks = pretty(c(x_min, x_max), n = 6),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    x = "Relative change over 1982-2021 (%)",
    y = NULL
  ) +
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(
      order = 2,
      override.aes = list(fill = "white", size = 3.5)
    ),
    fill = guide_colorbar(order = 3, reverse = FALSE)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.35),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

# ============================================================================
# Outputs
# ============================================================================

ggsave(
  file.path(PATH_OUTPUT_FIGURES, "hotspot_lollipop_avail_flux_ranked.png"),
  p,
  width = 10.5,
  height = 6.4,
  dpi = 300
)

ggsave(
  file.path(PATH_OUTPUT_FIGURES, "hotspot_lollipop_avail_flux_ranked.pdf"),
  p,
  width = 10.5,
  height = 6.4
)

saveRDS(
  plot_dt,
  file.path(PATH_OUTPUT_DATA, "hotspot_lollipop_region_data.Rds")
)

saveRDS(
  point_dt,
  file.path(PATH_OUTPUT_DATA, "hotspot_lollipop_point_data.Rds")
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
      grace_status
    )
  ]
)