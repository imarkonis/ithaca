# ============================================================================
# Diagnose regional Monte Carlo TWC trends against GRACE
#
# This script:
# 1. Computes regional categorical agreement diagnostics between GRACE and
#    water availability trends, P - E
# 2. Classifies each Monte Carlo simulation into:
#    a) agreement
#    b) disagreement
#    c) either_significant
#    d) non_significant
# 3. Estimates the fraction of Monte Carlo simulations in each category
# 4. Saves the regional GRACE-TWC diagnostic summary
# 5. Produces a validation figure with:
#    a) dominant water availability agreement category
#    b) scatter plot of GRACE slope versus mean acceleration slope
# ============================================================================

# Libraries ===================================================================

library(data.table)
library(ggplot2)
library(ggrepel)
library(patchwork)

source("source/twc_change.R")

# Inputs ======================================================================

mc_region_slopes_2002_2021 <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_2002_2021.Rds")
)

grace_region_slopes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "grace_region_slopes.Rds")
)

# Constants & Variables =======================================================

P_THRES <- 0.05

grace_category_levels <- c(
  "agreement",
  "disagreement",
  "non_significant"
)

grace_category_labels <- c(
  agreement = "Agreement",
  disagreement = "Disagreement",
  non_significant = "Non-significant"
)

grace_category_colours <- c(
  agreement = "#1A9850",
  disagreement = "#D7301F",
  non_significant = "grey85"
)

category_fill_scale <- scale_fill_manual(
  values = grace_category_colours,
  breaks = grace_category_levels,
  labels = grace_category_labels,
  name = NULL,
  drop = FALSE
)

category_colour_scale <- scale_colour_manual(
  values = grace_category_colours,
  breaks = grace_category_levels,
  labels = grace_category_labels,
  name = "P − E vs GRACE",
  drop = FALSE
)

# Functions ===================================================================

safe_sign <- function(x) {
  fifelse(x > 0, 1L, fifelse(x < 0, -1L, 0L))
}

dominant_category <- function(x) {
  
  counts <- table(
    factor(
      as.character(x),
      levels = grace_category_levels
    )
  )
  
  grace_category_levels[which.max(as.integer(counts))]
}

# Analysis ====================================================================

## Prepare GRACE comparison table =============================================

grace_compare <- grace_region_slopes[
  ,
  .(
    region,
    grace_slope = slope,
    grace_mk_p = mk_p_value,
    grace_sig_95 = mk_p_value <= P_THRES,
    grace_sign = safe_sign(slope)
  )
]

## Join Monte Carlo slopes with GRACE ==========================================

mc_grace <- merge(
  mc_region_slopes_2002_2021[
    ,
    .(
      scenario,
      sim_id,
      region,
      avail_slope,
      avail_mk_p,
      flux_slope,
      flux_mk_p
    )
  ],
  grace_compare,
  by = "region"
)

mc_grace[
  ,
  `:=`(
    avail_sign = safe_sign(avail_slope),
    flux_sign = safe_sign(flux_slope),
    avail_sig_95 = avail_mk_p <= P_THRES,
    flux_sig_95 = flux_mk_p <= P_THRES
  )
]
## Classify availability agreement with GRACE ==================================

mc_grace[
  ,
  avail_grace_category := fcase(
    !grace_sig_95 & !avail_sig_95,
    "non_significant",
    
    (grace_sig_95 | avail_sig_95) & avail_sign == grace_sign,
    "agreement",
    
    (grace_sig_95 | avail_sig_95) & avail_sign != grace_sign,
    "disagreement"
  )
]

mc_grace[
  ,
  avail_grace_category := factor(
    avail_grace_category,
    levels = grace_category_levels
  )
]

## Region-level diagnostic table ==============================================

mc_region_grace_diagnostic <- mc_grace[
  ,
  .(
    grace_slope = first(grace_slope),
    grace_mk_p = first(grace_mk_p),
    grace_sig_95 = first(grace_sig_95),
    grace_sign = first(grace_sign),
    
    avail_slope_mean = mean(avail_slope, na.rm = TRUE),
    flux_slope_mean = mean(flux_slope, na.rm = TRUE),
    
    avail_agreement_fraction = mean(
      avail_grace_category == "agreement",
      na.rm = TRUE
    ),
    avail_disagreement_fraction = mean(
      avail_grace_category == "disagreement",
      na.rm = TRUE
    ),
    avail_non_significant_fraction = mean(
      avail_grace_category == "non_significant",
      na.rm = TRUE
    ),
    
    avail_dominant_category = dominant_category(avail_grace_category),
    
    accel_fraction = mean(flux_slope > 0, na.rm = TRUE),
    accel_sig_fraction = mean(flux_slope > 0 & flux_sig_95, na.rm = TRUE),
    
    accel_fraction_if_grace_wetting = fifelse(
      first(grace_slope) > 0,
      mean(flux_slope > 0, na.rm = TRUE),
      NA_real_
    ),
    
    accel_sig_fraction_if_grace_sig_wetting = fifelse(
      first(grace_slope) > 0 & first(grace_mk_p) <= P_THRES,
      mean(flux_slope > 0 & flux_sig_95, na.rm = TRUE),
      NA_real_
    )
  ),
  by = region
]

mc_region_grace_diagnostic[
  ,
  avail_dominant_category := factor(
    avail_dominant_category,
    levels = grace_category_levels
  )
]

## Simulation-level GRACE agreement summary ===================================

mc_grace[
  ,
  avail_grace_category := fcase(
    !grace_sig_95 & !avail_sig_95,
    "non_significant",
    
    (grace_sig_95 | avail_sig_95) & avail_sign == grace_sign,
    "agreement",
    
    (grace_sig_95 | avail_sig_95) & avail_sign != grace_sign,
    "disagreement",
    
    default = NA_character_
  )
]

mc_grace[
  ,
  avail_grace_category := factor(
    avail_grace_category,
    levels = grace_category_levels
  )
]

sim_grace_summary <- mc_grace[
  ,
  .(
    agreement_fraction = mean(
      avail_grace_category == "agreement",
      na.rm = TRUE
    ),
    disagreement_fraction = mean(
      avail_grace_category == "disagreement",
      na.rm = TRUE
    ),
    non_significant_fraction = mean(
      avail_grace_category == "non_significant",
      na.rm = TRUE
    )
  ),
  by = .(scenario, sim_id)
]

sim_grace_summary[
  ,
  simulation_id := paste(scenario, sim_id, sep = "_")
]

sim_grace_summary <- sim_grace_summary[
  order(
    -agreement_fraction,
    disagreement_fraction,
    non_significant_fraction
  )
]

sim_grace_summary[
  ,
  simulation_rank := .I
]

sim_grace_long <- melt(
  sim_grace_summary,
  id.vars = c("scenario", "sim_id", "simulation_id", "simulation_rank"),
  measure.vars = c(
    "agreement_fraction",
    "disagreement_fraction",
    "non_significant_fraction"
  ),
  variable.name = "category",
  value.name = "fraction"
)

sim_grace_long[
  ,
  category := fcase(
    category == "agreement_fraction", "agreement",
    category == "disagreement_fraction", "disagreement",
    category == "non_significant_fraction", "non_significant"
  )
]

sim_grace_long[
  ,
  category := factor(
    category,
    levels = grace_category_levels
  )
]

# Outputs =====================================================================

saveRDS(
  mc_region_grace_diagnostic,
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_diagnostic_2002_2021.Rds")
)

saveRDS(
  sim_grace_long,
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_simulation_rank.Rds")
)

# Validation ==================================================================

## Panel A: water availability agreement category ==============================

p_avail_map <- build_region_hex_map(
  mc_region_grace_diagnostic[
    ,
    .(
      region,
      value = avail_dominant_category
    )
  ],
  color_scale = category_fill_scale,
  title = "A. P − E agreement with GRACE"
)

## Panel B: scatter ============================================================

p_scatter <- ggplot(mc_region_grace_diagnostic) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_point(
    aes(
      x = grace_slope,
      y = flux_slope_mean,
      colour = avail_dominant_category,
      shape = grace_sig_95
    ),
    size = 3,
    alpha = 0.9
  ) +
  ggrepel::geom_text_repel(
    aes(
      x = grace_slope,
      y = flux_slope_mean,
      label = region
    ),
    size = 2.5,
    max.overlaps = Inf
  ) +
  category_colour_scale +
  scale_shape_manual(
    values = c(`FALSE` = 1, `TRUE` = 16),
    name = "GRACE p ≤ 0.05"
  ) +
  labs(
    x = "GRACE TWS slope",
    y = "Mean acceleration slope, (P + E) / 2",
    title = "B. Acceleration versus GRACE behaviour, 2002-2021"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

## Combined figure ============================================================

p_grace_twc_diagnostic <- p_avail_map / p_scatter

p_grace_twc_diagnostic

# Validation ==================================================================

p_sim_grace_ranked <- ggplot(sim_grace_long) +
  geom_col(
    aes(
      x = simulation_rank,
      y = fraction,
      fill = category
    ),
    width = 1
  ) +
  category_fill_scale +
  labs(
    x = "Monte Carlo simulations, ordered by P − E agreement with GRACE",
    y = "Fraction of IPCC regions",
    title = "P − E agreement with GRACE across Monte Carlo simulations",
    subtitle = paste0(
      "Agreement requires same slope sign and at least one significant trend; ",
      "non-significant means both trends are non-significant"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p_sim_grace_ranked
