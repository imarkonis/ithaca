# Constants & Variables =======================================================

STORY_LEVELS <- c(
  "wetter-accelerated",
  "drier-accelerated",
  "wetter-decelerated",
  "drier-decelerated"
)

STORY_LABELS <- c(
  "wetter-accelerated" = "Wet & Accelerated",
  "drier-accelerated" = "Dry & Accelerated",
  "wetter-decelerated" = "Wet & Decelerated",
  "drier-decelerated" = "Dry & Decelerated"
)

STORY_COLS <- c(
  "wetter-accelerated" = PALETTES$water_cycle_change[1],
  "drier-accelerated" = PALETTES$water_cycle_change[3],
  "wetter-decelerated" = PALETTES$water_cycle_change[2],
  "drier-decelerated" = PALETTES$water_cycle_change[4]
)

QUAD_ALPHA <- 0.11

# Analysis ====================================================================

## Reclassify regions using same storyline colour logic ------------------------

mc_region_grace_class[
  ,
  grace_flux_class := fcase(
    grace_slope > 0 & flux_slope_mean > 0, "wetter-accelerated",
    grace_slope < 0 & flux_slope_mean > 0, "drier-accelerated",
    grace_slope > 0 & flux_slope_mean < 0, "wetter-decelerated",
    grace_slope < 0 & flux_slope_mean < 0, "drier-decelerated",
    default = NA_character_
  )
]

mc_region_grace_class[
  ,
  grace_flux_class := factor(
    grace_flux_class,
    levels = STORY_LEVELS
  )
]

## Split labels by availability-GRACE agreement --------------------------------

label_normal <- mc_region_grace_class[
  avail_grace_agreement >= 0.5 | is.na(avail_grace_agreement)
]

label_low_agreement <- mc_region_grace_class[
  avail_grace_agreement < 0.5
]

## Axis limits -----------------------------------------------------------------

x_abs <- max(abs(mc_region_grace_class$avail_slope_mean), na.rm = TRUE) * 1.15
y_abs <- max(abs(mc_region_grace_class$flux_slope_mean), na.rm = TRUE) * 1.15

# Validation ==================================================================

## Scatter: availability versus acceleration with GRACE class ------------------

p_scatter_avail_flux_grace_class <- ggplot() +
  
  # Quadrant shading, same colour logic as global availability-flux plot
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = STORY_COLS["drier-accelerated"],
    alpha = QUAD_ALPHA
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = 0, ymax = Inf,
    fill = STORY_COLS["wetter-accelerated"],
    alpha = QUAD_ALPHA
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = -Inf, ymax = 0,
    fill = STORY_COLS["drier-decelerated"],
    alpha = QUAD_ALPHA
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = STORY_COLS["wetter-decelerated"],
    alpha = QUAD_ALPHA
  ) +
  
  # Reference axes
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    linetype = "dashed",
    colour = "grey35"
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = "dashed",
    colour = "grey35"
  ) +
  
  # Points
  geom_point(
    data = mc_region_grace_class,
    aes(
      x = avail_slope_mean,
      y = flux_slope_mean,
      fill = grace_flux_class,
      size = avail_grace_agreement
    ),
    shape = 21,
    colour = "grey25",
    stroke = 0.45,
    alpha = 0.92
  ) +
  
  # Normal labels
  ggrepel::geom_text_repel(
    data = label_normal,
    aes(
      x = avail_slope_mean,
      y = flux_slope_mean,
      label = region
    ),
    size = 2.6,
    fontface = "plain",
    max.overlaps = Inf,
    segment.colour = "grey65",
    segment.size = 0.25
  ) +
  
  # Bold labels where agreement with GRACE is lower than 0.5
  ggrepel::geom_text_repel(
    data = label_low_agreement,
    aes(
      x = avail_slope_mean,
      y = flux_slope_mean,
      label = region
    ),
    size = 2.9,
    fontface = "bold",
    max.overlaps = Inf,
    segment.colour = "grey35",
    segment.size = 0.35
  ) +
  
  # Optional quadrant labels
  annotate(
    "label",
    x = -0.96 * x_abs,
    y = 0.96 * y_abs,
    label = "Dry & Accelerated",
    hjust = 0,
    vjust = 1,
    size = 3.5,
    fontface = "bold",
    colour = STORY_COLS["drier-accelerated"],
    fill = scales::alpha(STORY_COLS["drier-accelerated"], 0.15),
    label.size = 0.35,
    label.r = unit(0.12, "lines")
  ) +
  annotate(
    "label",
    x = 0.96 * x_abs,
    y = 0.96 * y_abs,
    label = "Wet & Accelerated",
    hjust = 1,
    vjust = 1,
    size = 3.5,
    fontface = "bold",
    colour = STORY_COLS["wetter-accelerated"],
    fill = scales::alpha(STORY_COLS["wetter-accelerated"], 0.15),
    label.size = 0.35,
    label.r = unit(0.12, "lines")
  ) +
  annotate(
    "label",
    x = -0.96 * x_abs,
    y = -0.96 * y_abs,
    label = "Dry & Decelerated",
    hjust = 0,
    vjust = 0,
    size = 3.5,
    fontface = "bold",
    colour = STORY_COLS["drier-decelerated"],
    fill = scales::alpha(STORY_COLS["drier-decelerated"], 0.15),
    label.size = 0.35,
    label.r = unit(0.12, "lines")
  ) +
  annotate(
    "label",
    x = 0.96 * x_abs,
    y = -0.96 * y_abs,
    label = "Wet & Decelerated",
    hjust = 1,
    vjust = 0,
    size = 3.5,
    fontface = "bold",
    colour = STORY_COLS["wetter-decelerated"],
    fill = scales::alpha(STORY_COLS["wetter-decelerated"], 0.15),
    label.size = 0.35,
    label.r = unit(0.12, "lines")
  ) +
  
  scale_fill_manual(
    values = STORY_COLS,
    breaks = STORY_LEVELS,
    labels = STORY_LABELS[STORY_LEVELS],
    drop = FALSE,
    name = "GRACE-storage and\nflux class"
  ) +
  scale_size_continuous(
    range = c(2.2, 6.2),
    limits = c(0, 1),
    name = "P − E agreement\nwith GRACE"
  ) +
  scale_x_continuous(
    limits = c(-x_abs, x_abs),
    expand = c(0, 0),
    name = expression("Mean availability slope, " * P - E)
  ) +
  scale_y_continuous(
    limits = c(-y_abs, y_abs),
    expand = c(0, 0),
    name = expression("Mean acceleration slope, " * (P + E) / 2)
  ) +
  labs(
    title = "Availability and acceleration trends by GRACE behaviour",
    caption = "Bold labels indicate regions where P − E sign agreement with GRACE is lower than 0.5."
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey93"),
    plot.title = element_text(face = "bold", size = 13),
    plot.caption = element_text(size = 8, colour = "grey45", hjust = 0),
    legend.position = "bottom",
    legend.box = "vertical"
  )

p_scatter_avail_flux_grace_class