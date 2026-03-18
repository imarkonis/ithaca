# ============================================================================
# Plot IPCC hexagon maps for regional TWC storyline likelihood classes
#
# This script:
# 1. Reads precomputed 8-class regional likelihood results
# 2. Reclassifies likelihood into four levels:
#    no_change, likely, most_likely, confident
# 3. Builds dominant regional classes for:
#    a) acceleration   : accelerating / decelerating
#    b) availability   : wetter / drier
#    c) compound       : wetter/drier x accelerated/decelerated
# 4. Joins results to the IPCC hexagon reference file
# 5. Produces:
#    a) main figure with 3 base-scenario panels side by side
#    b) 3 supplementary figures with 4 non-base scenarios each
#
# Likelihood bins used here:
#   - no_change   : p < 0.05
#   - likely      : 0.05 <= p < 0.20
#   - most_likely : 0.20 <= p < 0.50
#   - confident   : p >= 0.50
#
# If needed, adjust the thresholds in classify_likelihood_4().
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

library(grid)
library(patchwork)

# Inputs ======================================================================

region_likelihood_8 <- readRDS(
  file.path(PATH_OUTPUT_DATA, "region_storyline_likelihood_8classes.Rds")
)

ipcc_hexagon <- data.table(read.csv(
  "/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv"))

# Constants ===================================================================

SCENARIO_MAIN <- "Base"

SCENARIO_SUPP <- c(
  "Climate dominated",
  "Evaporation dominated",
  "Precipitation dominated",
  "Trend dominated"
)

SCENARIO_LEVELS_RAW <- c(
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

ACCEL_CLASSES <- c("accelerating", "decelerating")

AVAIL_CLASSES <- c("wetter", "drier")

COMPOUND_CLASSES <- c(
  "wetter-accelerated",
  "wetter-decelerated",
  "drier-accelerated",
  "drier-decelerated"
)

LIKELIHOOD_LEVELS <- c(
  "no_change",
  "likely",
  "most_likely",
  "confident"
)

# Palettes ====================================================================

COL_NO_CHANGE <- "#D9D9D9"

# Availability
COL_WET_1 <- "#D2ECE7"
COL_WET_2 <- "#9FD8CF"
COL_WET_3 <- "#5AB4AC"

COL_DRY_1 <- "#F0DEB7"
COL_DRY_2 <- "#DFC27D"
COL_DRY_3 <- "#D8B365"

# Acceleration
COL_ACC_1 <- "#C8D9EA"
COL_ACC_2 <- "#8FB2D4"
COL_ACC_3 <- "#4C78A8"

COL_DEC_1 <- "#F8D9B5"
COL_DEC_2 <- "#F4B16B"
COL_DEC_3 <- "#E67E22"

# Compound
COL_WA_1 <- "#CDEBE5"
COL_WA_2 <- "#92D3C8"
COL_WA_3 <- "#4EAFA6"

COL_WD_1 <- "#E4F3F0"
COL_WD_2 <- "#BEE4DE"
COL_WD_3 <- "#88CEC3"

COL_DA_1 <- "#F1D9A6"
COL_DA_2 <- "#E7B65A"
COL_DA_3 <- "#DE8F1D"

COL_DD_1 <- "#F5E6C6"
COL_DD_2 <- "#E7C98D"
COL_DD_3 <- "#D8B365"

accel_cols <- c(
  "no_change"                   = COL_NO_CHANGE,
  "accelerating_likely"         = COL_ACC_1,
  "accelerating_most_likely"    = COL_ACC_2,
  "accelerating_confident"      = COL_ACC_3,
  "decelerating_likely"         = COL_DEC_1,
  "decelerating_most_likely"    = COL_DEC_2,
  "decelerating_confident"      = COL_DEC_3
)

avail_cols <- c(
  "no_change"                   = COL_NO_CHANGE,
  "wetter_likely"               = COL_WET_1,
  "wetter_most_likely"          = COL_WET_2,
  "wetter_confident"            = COL_WET_3,
  "drier_likely"                = COL_DRY_1,
  "drier_most_likely"           = COL_DRY_2,
  "drier_confident"             = COL_DRY_3
)

compound_cols <- c(
  "no_change"                         = COL_NO_CHANGE,
  "wetter-accelerated_likely"         = COL_WA_1,
  "wetter-accelerated_most_likely"    = COL_WA_2,
  "wetter-accelerated_confident"      = COL_WA_3,
  "wetter-decelerated_likely"         = COL_WD_1,
  "wetter-decelerated_most_likely"    = COL_WD_2,
  "wetter-decelerated_confident"      = COL_WD_3,
  "drier-accelerated_likely"          = COL_DA_1,
  "drier-accelerated_most_likely"     = COL_DA_2,
  "drier-accelerated_confident"       = COL_DA_3,
  "drier-decelerated_likely"          = COL_DD_1,
  "drier-decelerated_most_likely"     = COL_DD_2,
  "drier-decelerated_confident"       = COL_DD_3
)

accel_fill_levels <- c(
  "no_change",
  "accelerating_likely",
  "accelerating_most_likely",
  "accelerating_confident",
  "decelerating_likely",
  "decelerating_most_likely",
  "decelerating_confident"
)

avail_fill_levels <- c(
  "no_change",
  "wetter_likely",
  "wetter_most_likely",
  "wetter_confident",
  "drier_likely",
  "drier_most_likely",
  "drier_confident"
)

compound_fill_levels <- c(
  "no_change",
  "wetter-accelerated_likely",
  "wetter-accelerated_most_likely",
  "wetter-accelerated_confident",
  "wetter-decelerated_likely",
  "wetter-decelerated_most_likely",
  "wetter-decelerated_confident",
  "drier-accelerated_likely",
  "drier-accelerated_most_likely",
  "drier-accelerated_confident",
  "drier-decelerated_likely",
  "drier-decelerated_most_likely",
  "drier-decelerated_confident"
)

accel_labels <- c(
  "no_change"                   = "No clear change",
  "accelerating_likely"         = "Accel. (•)",
  "accelerating_most_likely"    = "Accel. (••)",
  "accelerating_confident"      = "Accel. (•••)",
  "decelerating_likely"         = "Decel. (•)",
  "decelerating_most_likely"    = "Decel. (••)",
  "decelerating_confident"      = "Decel. (•••)"
)

avail_labels <- c(
  "no_change"                   = "No clear change",
  "wetter_likely"               = "Wetter (•)",
  "wetter_most_likely"          = "Wetter (••)",
  "wetter_confident"            = "Wetter (•••)",
  "drier_likely"                = "Drier (•)",
  "drier_most_likely"           = "Drier (••)",
  "drier_confident"             = "Drier (•••)"
)

compound_labels <- c(
  "no_change"                         = "No clear change",
  "wetter-accelerated_likely"         = "Wet & Accel. (•)",
  "wetter-accelerated_most_likely"    = "Wet & Accel. (••)",
  "wetter-accelerated_confident"      = "Wet & Accel. (•••)",
  "wetter-decelerated_likely"         = "Wet & Decel. (•)",
  "wetter-decelerated_most_likely"    = "Wet & Decel. (••)",
  "wetter-decelerated_confident"      = "Wet & Decel. (•••)",
  "drier-accelerated_likely"          = "Dry & Accel. (•)",
  "drier-accelerated_most_likely"     = "Dry & Accel. (••)",
  "drier-accelerated_confident"       = "Dry & Accel. (•••)",
  "drier-decelerated_likely"          = "Dry & Decel. (•)",
  "drier-decelerated_most_likely"     = "Dry & Decel. (••)",
  "drier-decelerated_confident"       = "Dry & Decel. (•••)"
)

# Helpers =====================================================================

classify_likelihood_4 <- function(prop_sig) {
  fcase(
    !is.finite(prop_sig), NA_character_,
    prop_sig < 0.05, "no_change",
    prop_sig < 0.20, "likely",
    prop_sig < 0.50, "most_likely",
    default = "confident"
  )
}

reclassify_likelihood_table <- function(dt) {
  dt <- copy(as.data.table(dt))

  dt[, likelihood := classify_likelihood_4(prop_sig)]
  dt[, likelihood := factor(
    likelihood,
    levels = LIKELIHOOD_LEVELS,
    ordered = TRUE
  )]

  dt
}

shift_ipcc_hexagons <- function(dt) {
  dt <- copy(as.data.table(dt))

  rows_aus <- which(dt$Acronym %in% c("NAU", "CAU", "EAU", "SAU"))
  rows_nz <- which(dt$Acronym == "NZ")
  rows_mdg <- which(dt$Acronym == "MDG")
  rows_gic <- which(dt$Acronym == "GIC")

  dt$long[rows_gic] <- dt$long[rows_gic] - 7
  dt$lat[rows_gic] <- dt$lat[rows_gic] - 4
  dt$V1[rows_gic] <- dt$V1[rows_gic] - 7
  dt$V2[rows_gic] <- dt$V2[rows_gic] - 4

  dt$long[rows_mdg] <- dt$long[rows_mdg] - 7
  dt$lat[rows_mdg] <- dt$lat[rows_mdg] - 3
  dt$V1[rows_mdg] <- dt$V1[rows_mdg] - 7
  dt$V2[rows_mdg] <- dt$V2[rows_mdg] - 3

  dt$long[rows_aus] <- dt$long[rows_aus] + 5
  dt$lat[rows_aus] <- dt$lat[rows_aus] + 12
  dt$V1[rows_aus] <- dt$V1[rows_aus] + 5
  dt$V2[rows_aus] <- dt$V2[rows_aus] + 12

  dt$long[rows_nz] <- dt$long[rows_nz] + 10
  dt$lat[rows_nz] <- dt$lat[rows_nz] + 9
  dt$V1[rows_nz] <- dt$V1[rows_nz] + 10
  dt$V2[rows_nz] <- dt$V2[rows_nz] + 9

  dt
}

pick_dominant_subset <- function(dt, class_subset) {
  dt <- copy(as.data.table(dt))

  out <- dt[class %in% class_subset]

  out[, likelihood_rank := fcase(
    likelihood == "no_change", 1L,
    likelihood == "likely", 2L,
    likelihood == "most_likely", 3L,
    likelihood == "confident", 4L,
    default = NA_integer_
  )]

  out <- out[
    order(
      scenario,
      region,
      -likelihood_rank,
      -prop_sig,
      -n_sig,
      class
    )
  ][
    ,
    .SD[1],
    by = .(scenario, region)
  ]

  all_regions <- unique(dt[, .(scenario, region, n_total)])
  out <- merge(
    all_regions,
    out,
    by = c("scenario", "region", "n_total"),
    all.x = TRUE
  )

  out[
    is.na(class) | is.na(likelihood) | likelihood == "no_change",
    `:=`(
      class = NA_character_,
      n_sig = 0L,
      prop_sig = 0,
      likelihood = factor(
        "no_change",
        levels = LIKELIHOOD_LEVELS,
        ordered = TRUE
      )
    )
  ]

  out
}

make_fill_key <- function(dt) {
  dt <- copy(as.data.table(dt))

  dt[
    likelihood == "no_change" | is.na(class),
    fill_key := "no_change"
  ]

  dt[
    likelihood != "no_change" & !is.na(class),
    fill_key := paste0(class, "_", as.character(likelihood))
  ]

  dt
}

prepare_hex_map_data <- function(map_dt, fill_levels) {
  hex_dt <- copy(as.data.table(ipcc_hexagon))
  setDT(hex_dt)

  map_dt <- copy(as.data.table(map_dt))
  map_dt[, scenario := factor(
    scenario,
    levels = names(SCENARIO_LABELS),
    labels = SCENARIO_LABELS
  )]
  map_dt[, fill_key := factor(fill_key, levels = fill_levels)]

  out <- hex_dt[
    map_dt[, .(scenario, Acronym = region, fill_key)],
    on = "Acronym",
    allow.cartesian = TRUE
  ]

  shift_ipcc_hexagons(out)
}

plot_hex_map_single <- function(
  dt,
  fill_cols,
  fill_breaks,
  fill_labels,
  title_text = NULL,
  show_legend = TRUE,
  legend_nrow = 2
) {
  label_black <- dt[fill_key == "no_change"]

  ggplot(dt) +
    geom_polygon(
      aes(x = long, y = lat, group = group, fill = fill_key),
      colour = "grey35",
      linewidth = 0.35
    ) +
    geom_text(
      aes(x = V1, y = V2, label = Acronym),
      size = 2.5,
      colour = "white",
      fontface = "bold"
    ) +
    geom_text(
      data = label_black,
      aes(x = V1, y = V2, label = Acronym),
      size = 2.5,
      colour = "black",
      fontface = "bold"
    ) +
    coord_equal() +
    scale_fill_manual(
      values = fill_cols,
      breaks = fill_breaks,
      labels = fill_labels,
      drop = FALSE
    ) +
    guides(
      fill = guide_legend(
        nrow = legend_nrow,
        byrow = TRUE,
        title = NULL,
        override.aes = list(colour = "grey35")
      )
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
      legend.position = if (show_legend) "bottom" else "none",
      legend.box = "horizontal",
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
      legend.key.width = unit(0.55, "cm"),
      legend.key.height = unit(0.40, "cm"),
      legend.spacing.x = unit(0.20, "cm"),
      legend.text = element_text(size = 8)
    )
}

plot_hex_map_faceted <- function(
  dt,
  fill_cols,
  fill_breaks,
  fill_labels,
  title_text,
  legend_nrow = 2
) {
  label_black <- dt[fill_key == "no_change"]

  ggplot(dt) +
    geom_polygon(
      aes(x = long, y = lat, group = group, fill = fill_key),
      colour = "grey35",
      linewidth = 0.30
    ) +
    geom_text(
      aes(x = V1, y = V2, label = Acronym),
      size = 2.5,
      colour = "white",
      fontface = "bold"
    ) +
    geom_text(
      data = label_black,
      aes(x = V1, y = V2, label = Acronym),
      size = 2.5,
      colour = "black",
      fontface = "bold"
    ) +
    coord_equal() +
    facet_wrap(~ scenario, ncol = 2) +
    scale_fill_manual(
      values = fill_cols,
      breaks = fill_breaks,
      labels = fill_labels,
      drop = FALSE
    ) +
    guides(
      fill = guide_legend(
        nrow = legend_nrow,
        byrow = TRUE,
        title = NULL,
        override.aes = list(colour = "grey35")
      )
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_void() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
      legend.key.width = unit(0.55, "cm"),
      legend.key.height = unit(0.40, "cm"),
      legend.spacing.x = unit(0.20, "cm"),
      legend.text = element_text(size = 8)
    )
}

# Reclassify likelihoods ======================================================

region_likelihood_8 <- reclassify_likelihood_table(region_likelihood_8)

# Build dominant products =====================================================

region_dom_accel <- pick_dominant_subset(
  dt = region_likelihood_8,
  class_subset = ACCEL_CLASSES
)

region_dom_avail <- pick_dominant_subset(
  dt = region_likelihood_8,
  class_subset = AVAIL_CLASSES
)

region_dom_compound <- pick_dominant_subset(
  dt = region_likelihood_8,
  class_subset = COMPOUND_CLASSES
)

region_dom_accel <- make_fill_key(region_dom_accel)
region_dom_avail <- make_fill_key(region_dom_avail)
region_dom_compound <- make_fill_key(region_dom_compound)

# Save intermediates ==========================================================

saveRDS(
  region_dom_accel,
  file.path(PATH_OUTPUT_DATA, "region_storyline_mode_acceleration.Rds")
)

saveRDS(
  region_dom_avail,
  file.path(PATH_OUTPUT_DATA, "region_storyline_mode_availability.Rds")
)

saveRDS(
  region_dom_compound,
  file.path(PATH_OUTPUT_DATA, "region_storyline_mode_compound.Rds")
)

# Join to hexagons ============================================================

map_accel_hex <- prepare_hex_map_data(
  map_dt = region_dom_accel,
  fill_levels = accel_fill_levels
)

map_avail_hex <- prepare_hex_map_data(
  map_dt = region_dom_avail,
  fill_levels = avail_fill_levels
)

map_compound_hex <- prepare_hex_map_data(
  map_dt = region_dom_compound,
  fill_levels = compound_fill_levels
)

# Main figure: base only ======================================================

p_base_accel <- plot_hex_map_single(
  dt = map_accel_hex[scenario == SCENARIO_MAIN],
  fill_cols = accel_cols,
  fill_breaks = accel_fill_levels,
  fill_labels = accel_labels,
  title_text = "Acceleration",
  show_legend = TRUE,
  legend_nrow = 2
)

p_base_avail <- plot_hex_map_single(
  dt = map_avail_hex[scenario == SCENARIO_MAIN],
  fill_cols = avail_cols,
  fill_breaks = avail_fill_levels,
  fill_labels = avail_labels,
  title_text = "Availability",
  show_legend = TRUE,
  legend_nrow = 2
)

p_base_compound <- plot_hex_map_single(
  dt = map_compound_hex[scenario == SCENARIO_MAIN],
  fill_cols = compound_cols,
  fill_breaks = compound_fill_levels,
  fill_labels = compound_labels,
  title_text = "Compound storyline",
  show_legend = TRUE,
  legend_nrow = 4
)

p_main <- (p_base_accel | p_base_avail | p_base_compound) +
  plot_layout(guides = "collect", widths = c(1, 1, 1.1)) +
  plot_annotation(
    title = "Regional terrestrial water-cycle storyline likelihoods"
  ) &
  theme(
    legend.position = "bottom"
  )

print(p_main)

# Supplementary figures =======================================================

p_supp_accel <- plot_hex_map_faceted(
  dt = map_accel_hex[scenario %in% SCENARIO_SUPP],
  fill_cols = accel_cols,
  fill_breaks = accel_fill_levels,
  fill_labels = accel_labels,
  title_text = "Regional storyline likelihoods: acceleration",
  legend_nrow = 2
)

p_supp_avail <- plot_hex_map_faceted(
  dt = map_avail_hex[scenario %in% SCENARIO_SUPP],
  fill_cols = avail_cols,
  fill_breaks = avail_fill_levels,
  fill_labels = avail_labels,
  title_text = "Regional storyline likelihoods: availability",
  legend_nrow = 2
)

p_supp_compound <- plot_hex_map_faceted(
  dt = map_compound_hex[scenario %in% SCENARIO_SUPP],
  fill_cols = compound_cols,
  fill_breaks = compound_fill_levels,
  fill_labels = compound_labels,
  title_text = "Regional storyline likelihoods: compound storyline",
  legend_nrow = 4
)

print(p_supp_accel)
print(p_supp_avail)
print(p_supp_compound)

# Save ========================================================================

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "map_ipcc_hexagon_twc_storylines_base_three_panels.png"
  ),
  plot = p_main,
  width = 16.5,
  height = 6.4,
  units = "in",
  dpi = 600
)

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "map_ipcc_hexagon_twc_storylines_supp_acceleration.png"
  ),
  plot = p_supp_accel,
  width = 11,
  height = 8.2,
  units = "in",
  dpi = 600
)

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "map_ipcc_hexagon_twc_storylines_supp_availability.png"
  ),
  plot = p_supp_avail,
  width = 11,
  height = 8.2,
  units = "in",
  dpi = 600
)

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "map_ipcc_hexagon_twc_storylines_supp_compound.png"
  ),
  plot = p_supp_compound,
  width = 12.5,
  height = 9.2,
  units = "in",
  dpi = 600
)

