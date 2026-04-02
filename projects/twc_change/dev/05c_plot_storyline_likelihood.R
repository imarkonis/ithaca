# ============================================================================
# Storyline likelihood heatmap
#
# Rows:    8 mechanistic storylines
# Columns: Likelihood | ΔP | ΔE | Δ(P+E)/2 | Δ(P−E)
# Fill:    classify_support() applied to per-variable sign-agreement probability
#          (Likelihood column uses median_p_story)
# Text:    Likelihood: "0.XX (0.XX)" | Variables: median relative change (%)
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

classify_support <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.20 ~ "Confident absent",
    p < 0.40 ~ "Likely absent",
    p < 0.60 ~ "No clear signal",
    p < 0.80 ~ "Likely present",
    p < 1 ~ "Confident present"
  )
}

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
    paste0(formatC(p, digits = 2, format = "f"),
           "\n(", formatC(consensus, digits = 2, format = "f"), ")")
  )
}

# Input =======================================================================

summary_dt <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "storyline_summary.Rds"))
)

# Storyline display names =====================================================

STORYLINE_LABELS <- c(
  "storyline_1_arctic_boreal_amplification"       = "1. Arctic/boreal amplification",
  "storyline_2_poleward_stormtrack_wetting"        = "2. Poleward storm-track wetting",
  "storyline_3_monsoon_amplification"              = "3. Monsoon amplification",
  "storyline_4_humid_tropical_intensification"     = "4. Humid tropical intensification",
  "storyline_5_subtropical_circulation_drying"     = "5. Subtropical circulation drying",
  "storyline_6_land_atm_coupling_amplification"    = "6. Land-atmosphere coupling",
  "storyline_7_deforestation_induced_deceleration" = "7. Deforestation-induced deceleration",
  "storyline_8_dryland_soilmoisture_collapse"      = "8. Dryland soil-moisture collapse"
)

# Reshape to long format ======================================================

var_map <- data.table(
  p_col   = c("p_prec_agree",   "p_evap_agree",   "p_flux_agree",   "p_avail_agree"),
  rel_col = c("median_prec_rel", "median_evap_rel", "median_flux_rel", "median_avail_rel"),
  variable = c("ΔP", "ΔE", "Δ(P+E)/2", "Δ(P−E)")
)

flux_long <- rbindlist(
  lapply(seq_len(nrow(var_map)), function(i) {
    data.table(
      storyline  = summary_dt$storyline,
      variable   = var_map$variable[i],
      p_agree    = summary_dt[[var_map$p_col[i]]],
      label      = fmt_rel(summary_dt[[var_map$rel_col[i]]])
    )
  })
)

# Likelihood column — median_p_story colored, label = "p (consensus)"
likelihood_long <- data.table(
  storyline = summary_dt$storyline,
  variable  = "Likelihood",
  p_agree   = summary_dt$median_p_story,
  label     = fmt_likelihood(summary_dt$median_p_story, summary_dt$p_consensus)
)

long_dt <- rbindlist(list(likelihood_long, flux_long))

long_dt[, support := classify_support(p_agree)]

# Factor ordering =============================================================

SUPPORT_LEVELS <- c(
  "Confident absent",
  "Likely absent",
  "No clear signal",
  "Likely present",
  "Confident present"
)

SUPPORT_COLORS <- c(
  "Confident absent"  = "#b2182b",
  "Likely absent"     = "#ef8a62",
  "No clear signal"   = "#f5f5f0",
  "Likely present"    = "#67a9cf",
  "Confident present" = "#2166ac"
)

long_dt[, support := factor(support, levels = SUPPORT_LEVELS)]

long_dt[, storyline_label := factor(
  STORYLINE_LABELS[storyline],
  levels = rev(STORYLINE_LABELS)
)]

long_dt[, variable := factor(
  variable,
  levels = c("Likelihood", "ΔP", "ΔE", "Δ(P+E)/2", "Δ(P−E)")
)]

# Plot ========================================================================

p <- ggplot(long_dt, aes(x = variable, y = storyline_label)) +
  
  # tiles
  geom_tile(aes(fill = support), colour = "white", linewidth = 0.8) +
  
  # vertical separator after Likelihood column
  geom_vline(
    xintercept = 1.5,
    colour     = "grey30",
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  
  # cell text
  geom_text(
    aes(label = label),
    size     = 3.0,
    fontface = "bold",
    colour   = "grey20",
    na.rm    = TRUE,
    lineheight = 0.9
  ) +
  
  # group A/B separator
  geom_hline(
    yintercept = 4.5,
    colour     = "grey30",
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  
  annotate("text", x = 0.35, y = 6.5, label = "A",
           fontface = "bold", size = 4.5, colour = "grey30") +
  annotate("text", x = 0.35, y = 2.5, label = "B",
           fontface = "bold", size = 4.5, colour = "grey30") +
  
  scale_fill_manual(
    values   = SUPPORT_COLORS,
    na.value = "grey88",
    name     = "Signal support",
    drop     = FALSE
  ) +
  
  scale_x_discrete(position = "top") +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title    = "Mechanistic storyline likelihood",
    subtitle = "Likelihood: median p-story (consensus fraction) · Flux columns: median relative change (%) · Fill: signal support",
    x        = NULL,
    y        = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 9, colour = "grey40", margin = margin(b = 12)),
    axis.text.x     = element_text(face = "bold", size = 11),
    axis.text.y     = element_text(size = 10, hjust = 1),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = 10),
    legend.key.width  = unit(1.4, "cm"),
    legend.key.height = unit(0.45, "cm"),
    panel.grid      = element_blank(),
    plot.margin     = margin(10, 10, 10, 30)
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