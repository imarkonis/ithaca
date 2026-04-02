# ============================================================================
# Storyline likelihood heatmap — directional fill
#
# Fill score:
#   Wetting storylines (1-4): fill_score = p_agree - 0.5
#     positive (+) → blue  = wetting signal present
#     negative (-) → red   = wetting signal absent
#   Drying storylines (5-8):  fill_score = 0.5 - p_agree
#     negative (-) → red   = drying signal present
#     positive (+) → blue  = drying signal absent
#   Zero = no clear signal (white)
#   NA cells (≈0 or mixed) = grey
#
# Uses:
#   1) storyline_summary.Rds
#
# Produces:
#   storyline_heatmap.pdf / .png
# ============================================================================

library(data.table)
library(ggplot2)
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

# Storyline display names & direction =========================================

STORYLINE_META <- data.table(
  storyline      = c(
    "storyline_1_arctic_boreal_amplification",
    "storyline_2_poleward_stormtrack_wetting",
    "storyline_3_monsoon_amplification",
    "storyline_4_humid_tropical_intensification",
    "storyline_5_subtropical_circulation_drying",
    "storyline_6_land_atm_coupling_amplification",
    "storyline_7_deforestation_induced_deceleration",
    "storyline_8_dryland_soilmoisture_collapse"
  ),
  storyline_name = c(
    "1. Arctic/boreal amplification",
    "2. Poleward storm-track wetting",
    "3. Monsoon amplification",
    "4. Humid tropical intensification",
    "5. Subtropical circulation drying",
    "6. Land-atmosphere coupling",
    "7. Deforestation-induced deceleration",
    "8. Dryland soil-moisture collapse"
  ),
  direction = c(
    "wetting", "wetting", "wetting", "wetting",
    "drying",  "drying",  "drying",  "drying"
  )
)

# Reshape to long format ======================================================

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

# Merge direction & label
long_dt <- merge(long_dt, STORYLINE_META, by = "storyline")

# Directional fill score ======================================================
# Wetting: fill_score = p_agree - 0.5  → positive = happening → blue
# Drying:  fill_score = 0.5 - p_agree  → negative = happening → red
# NA p_agree (≈0 / mixed) stays NA     → rendered as grey

long_dt[, fill_score := fcase(
  direction == "wetting", p_agree - 0.5,
  direction == "drying",  0.5 - p_agree,
  default = NA_real_
)]

# Factor ordering =============================================================

long_dt[, storyline_label := factor(
  storyline_name,
  levels = rev(STORYLINE_META$storyline_name)
)]

# Color scale — diverging, classify_support thresholds as breaks ==============
# Thresholds in fill_score space (p_agree - 0.5):
#   ±0.10 = p 0.40/0.60 boundary (no clear signal edge)
#   ±0.30 = p 0.20/0.80 boundary (likely/most_likely)
#   ±0.45 = p 0.05/0.95 boundary (confident)

FILL_COLORS <- c(
  "#b2182b",  # -0.50  confident drying / wetting absent
           "#d6604d",  # -0.35
           "#f4a582",  # -0.15
           "#f7f7f2",  #  0.00  no clear signal
           "#92c5de",  # +0.15
           "#4393c3",  # +0.35
           "#2166ac"   # +0.50  confident wetting / drying absent
)

FILL_VALUES <- rescale(c(-0.50, -0.35, -0.15, 0, 0.15, 0.35, 0.50))

FILL_BREAKS <- c(-0.45, -0.30, -0.10, 0.10, 0.30, 0.45)

FILL_LABELS <- c(
  "Confident\ndrying",
  "Likely\ndrying",
  "No clear\nsignal",
  "No clear\nsignal",
  "Likely\nwetting",
  "Confident\nwetting"
)

# Text colour — white on dark fills, dark on light fills
long_dt[, text_colour := fcase(
  !is.na(fill_score) & abs(fill_score) >= 0.30, "white",
  default = "grey20"
)]

# Plot ========================================================================

p <- ggplot(long_dt, aes(x = variable, y = storyline_label)) +
  
  geom_tile(
    aes(fill = fill_score),
    colour    = "white",
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = 1.5,
    colour     = "grey30",
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  
  geom_hline(
    yintercept = 4.5,
    colour     = "grey30",
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  
  geom_text(
    aes(label = label, colour = text_colour),
    size       = 3.0,
    fontface   = "bold",
    lineheight = 0.9,
    na.rm      = TRUE
  ) +
  
  annotate("text", x = 0.35, y = 6.5, label = "A",
           fontface = "bold", size = 4.5, colour = "grey30") +
  annotate("text", x = 0.35, y = 2.5, label = "B",
           fontface = "bold", size = 4.5, colour = "grey30") +
  
  scale_fill_gradientn(
    colours  = FILL_COLORS,
    values   = FILL_VALUES,
    limits   = c(-0.5, 0.5),
    breaks   = FILL_BREAKS,
    labels   = FILL_LABELS,
    na.value = "grey88",
    name     = "Signal direction & support",
    guide    = guide_colorbar(
      barwidth      = unit(14, "cm"),
      barheight     = unit(0.5, "cm"),
      title.position = "top",
      title.hjust    = 0.5,
      label.position = "bottom",
      ticks.linewidth = 1.2
    )
  ) +
  
  scale_colour_identity() +
  
  scale_x_discrete(position = "top") +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title    = "Mechanistic storyline likelihood",
    subtitle = paste0(
      "Likelihood: median p-story (consensus fraction)  ·  ",
      "Flux columns: median relative change (%)  ·  ",
      "Blue = wetting direction  ·  Red = drying direction  ·  ",
      "Intensity = signal confidence"
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 8.5, colour = "grey40", margin = margin(b = 12)),
    axis.text.x      = element_text(face = "bold", size = 11),
    axis.text.y      = element_text(size = 10, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    panel.grid       = element_blank(),
    plot.margin      = margin(10, 10, 10, 30)
  )

# Save ========================================================================

ggsave(
  file.path(PATH_OUTPUT_PLOTS, "storyline_heatmap.pdf"),
  plot   = p,
  width  = 10,
  height = 6,
  device = cairo_pdf
)

ggsave(
  file.path(PATH_OUTPUT_PLOTS, "storyline_heatmap.png"),
  plot   = p,
  width  = 10,
  height = 6,
  dpi    = 300
)