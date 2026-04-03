# ============================================================================
# Storyline likelihood heatmap — circle-based, green-to-red evidence
#
# Circle size:   signal strength = abs(fill_score)
# Circle colour: evidence level  = fill_score mapped green (present) → red (absent)
# Text:          Likelihood cell: "p (consensus)" | Flux cells: median rel change
# ============================================================================

library(scales)
source("source/twc_change.R")

# Helpers =====================================================================

fmt_rel <- function(x) {
  ifelse(
    is.na(x), NA_character_,
    ifelse(x >= 0,
           paste0("+", formatC(x, digits = 1, format = "f"), "%"),
           paste0(formatC(x, digits = 1, format = "f"), "%"))
  )
}

fmt_likelihood <- function(p, consensus) {
  ifelse(
    is.na(p), NA_character_,
    paste0(formatC(p,         digits = 2, format = "f"), "\n(",
           formatC(consensus, digits = 2, format = "f"), ")")
  )
}

# Input =======================================================================

summary_dt <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "storyline_summary.Rds"))
)

# Storyline metadata ==========================================================

STORYLINE_META <- data.table(
  storyline = c(
    "1_arctic_boreal_amplification",
    "2_poleward_stormtrack_wetting",
    "3_monsoon_amplification",
    "4_humid_tropical_intensification",
    "5_subtropical_circulation_drying",
    "6_land_atm_coupling_amplification",
    "7_deforestation_induced_deceleration",
    "8_dryland_soilmoisture_collapse"
  ),
  storyline_name = c(
    "Arctic/boreal amplification",
    "Poleward storm-track wetting",
    "Monsoon amplification",
    "Humid tropical intensification",
    "Subtropical circulation drying",
    "Land-atmosphere coupling",
    "Deforestation-induced deceleration",
    "Dryland soil-moisture collapse"
  ),
  direction = c(
    "wetting", "wetting", "wetting", "wetting",
    "drying",  "drying",  "drying",  "drying"
  )
)

# Reshape to long =============================================================

var_map <- data.table(
  p_col    = c("p_prec_agree",    "p_evap_agree",    "p_flux_agree",    "p_avail_agree"),
  rel_col  = c("median_prec_rel", "median_evap_rel",  "median_flux_rel", "median_avail_rel"),
  variable = c("ΔP",              "ΔE",               "Δ(P+E)/2",        "Δ(P−E)")
)

flux_long <- rbindlist(
  lapply(seq_len(nrow(var_map)), function(i) {
    data.table(
      storyline = summary_dt$storyline,
      variable  = var_map$variable[i],
      p_agree   = summary_dt[[var_map$p_col[i]]],
      label     = fmt_rel(summary_dt[[var_map$rel_col[i]]])
    )
  })
)

likelihood_long <- data.table(
  storyline = summary_dt$storyline,
  variable  = "Likelihood",
  p_agree   = summary_dt$median_p_story,
  label     = fmt_likelihood(summary_dt$median_p_story, summary_dt$p_consensus)
)

long_dt <- rbindlist(list(likelihood_long, flux_long))
long_dt  <- merge(long_dt, STORYLINE_META, by = "storyline")

long_dt[, fill_score := fifelse(is.na(p_agree), NA_real_, p_agree - 0.5)]

# Circle aesthetics ===========================================================
# size  = abs(fill_score) — how strong the signal is
# colour = fill_score     — positive (evidence present) → green
#                           negative (evidence absent)  → red
# NA cells get a small hollow point

P_NULL_COMPOUND <- 0.25

long_dt[, fill_score := fcase(
  variable == "Likelihood", p_agree / P_NULL_COMPOUND - 1,  # 0 at null, +1 at 2×null, -1 at 0
  !is.na(p_agree),          p_agree - 0.5,                  # single-variable columns
  default                   = NA_real_
)]

long_dt[variable == "Likelihood", fill_score := fcase(
  p_agree >= P_NULL_COMPOUND,
  (p_agree - P_NULL_COMPOUND) / (1 - P_NULL_COMPOUND) * 0.5,  # [null, 1] → [0, +0.5]
  p_agree <  P_NULL_COMPOUND,
  (p_agree - P_NULL_COMPOUND) / P_NULL_COMPOUND        * 0.5,  # [0, null] → [-0.5, 0]
  default = NA_real_
)]
long_dt[, circle_size   := fifelse(is.na(fill_score), 0.05, abs(fill_score))]
long_dt[, circle_colour := fill_score]

# Factor ordering =============================================================

long_dt[, storyline_label := factor(
  storyline_name,
  levels = rev(STORYLINE_META$storyline_name)
)]

long_dt[, variable := factor(
  variable,
  levels = c("Likelihood", "ΔP", "ΔE", "Δ(P+E)/2", "Δ(P−E)")
)]

long_dt[, text_colour := fcase(
  is.na(fill_score),         "grey50",
  abs(fill_score) >= 0.30,   "white",
  default                    = "grey25"
)]

LABEL_COLOURS <- setNames(
  c(rep("#8c510a", 4), rep("#01665e", 4)),
  rev(STORYLINE_META$storyline_name)   # rev() matches y-axis bottom-to-top order
)

# Plot ========================================================================

p <- ggplot(long_dt, aes(x = variable, y = storyline_label)) +
  
  # background grid tiles
  geom_tile(
    fill      = "grey97",
    colour    = "grey85",
    linewidth = 0.4
  ) +
  
  # group A/B separator tile overlay (subtle band)
  geom_hline(
    yintercept = 4.5,
    colour     = "grey40",
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  
  geom_vline(
    xintercept = 1.5,
    colour     = "grey40",
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  
  # circles
  geom_point(
    aes(size = circle_size, fill = circle_colour),
    shape  = 21,
    colour = "white",
    stroke = 0.3,
    na.rm  = TRUE
  ) +
  
  # cell text below circles
  geom_text(
    aes(label = label, colour = text_colour),
    size       = 2.6,
    fontface   = "plain",
    lineheight = 0.85,
    vjust      = 0.5,
    na.rm      = TRUE
  ) +
  scale_colour_identity() +

  
  # circle fill: green = evidence present, red = evidence absent
  scale_fill_gradientn(
    colours = c(
      "#b2182b", "#ef8a62", "#fddbc7", "#f7f7f7", "#d1e5f0", "#67a9cf", "#2166ac"
               ), 
    values   = rescale(c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5)),
    limits   = c(-0.5, 0.5),
    breaks   = c(-0.45, -0.25, 0, 0.25, 0.45),
    labels   = c("Confident\nabsent", "Likely\nabsent",
                 "No clear\nsignal",
                 "Likely\npresent", "Confident\npresent"),
    na.value = "grey88",
    name     = "Evidence",
    guide    = guide_colorbar(
      barwidth       = unit(12, "cm"),
      barheight      = unit(0.45, "cm"),
      title.position = "top",
      title.hjust    = 0.5,
      label.position = "bottom",
      ticks.linewidth = 1.2
    )
  ) +
  
  # circle size: scaled to signal strength, NA gets tiny dot
  scale_size_continuous(
    range  = c(1, 11),
    limits = c(0, 0.5),
    guide  = "none"
  ) +
  
  scale_x_discrete(position = "top") +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title    = "Mechanistic storyline likelihood",
    subtitle = paste0(
      "Circle size: signal strength  ·  ",
      "Text: median relative change (%)   ·  ",
      "Parenthesis: Region consensus (fraction)"
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 8.5, colour = "grey40", margin = margin(b = 12)),
    axis.text.x      = element_text(face = "bold", size = 11),
    axis.text.y = element_text(
      size   = 10,
      hjust  = 1,
      face   = "bold",
      colour = LABEL_COLOURS[levels(long_dt$storyline_label)]
    ), 
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    panel.grid       = element_blank(),
    plot.margin      = margin(10, 10, 20, 30)
  )

# Save ========================================================================

ggsave(
  file.path(PATH_OUTPUT_PLOTS, "storyline_heatmap.pdf"),
  plot   = p,
  width  = 10,
  height = 6.5,
  device = cairo_pdf
)

ggsave(
  file.path(PATH_OUTPUT_PLOTS, "storyline_heatmap.png"),
  plot   = p,
  width  = 10,
  height = 6.5,
  dpi    = 300
)

