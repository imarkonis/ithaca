# ============================================================================
# Build regional TWC storyline likelihood classes for IPCC hexagon mapping
#
# This script:
# 1. Counts significant ensemble members per region and scenario for:
#    a) four composite storyline classes
#    b) four single-sign classes
# 2. Converts counts to likelihood bins:
#      - no_change  : p < 0.05 of ensemble members
#      - likely     : 0.05 <= p < 0.20
#      - confident  : p >= 0.20
# 3. Selects one dominant class per region and scenario
# 4. Saves both the full class table and the map-ready dominant table
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

ensemble_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds")
)

# Constants ===================================================================

THRES_SIGNIFICANCE <- 0.05

SCENARIO_LEVELS <- c(
  "base",
  "clim_dominant",
  "evap_dominant",
  "prec_dominant",
  "trend_dominant"
)

CLASS_LEVELS_8 <- c(
  "wetter-accelerated",
  "wetter-decelerated",
  "drier-accelerated",
  "drier-decelerated",
  "wetter",
  "drier",
  "accelerating",
  "decelerating"
)

LIKELIHOOD_LEVELS <- c("no_change", "likely", "most likely", "confident")

# Helpers =====================================================================

classify_likelihood <- function(prop) {
  fcase(
    !is.finite(prop), NA_character_,
    prop < 0.05, "no_change",
    prop < 0.10, "likely",
    prop < 0.20, "most likely",
    default = "confident"
  )
}

prepare_region_members <- function(dt, thres_significance) {
  dt <- copy(as.data.table(dt))
  
  dt <- dt[
    source_type == "mc" &
      region != "GLOBAL" &
      !is.na(storyline)
  ]
  
  dt[, sig_avail := avail_mk_p < thres_significance]
  dt[, sig_flux  := flux_mk_p  < thres_significance]
  dt[, sig_both  := sig_avail & sig_flux]
  
  # availability sign
  dt[, avail_sign := fifelse(avail_abs_change > 0, "wetter",
                             fifelse(avail_abs_change < 0, "drier", NA_character_))]
  
  # flux sign
  dt[, flux_sign := fifelse(flux_abs_change > 0, "accelerating",
                            fifelse(flux_abs_change < 0, "decelerating", NA_character_))]
  
  dt[, scenario := factor(scenario, levels = SCENARIO_LEVELS)]
  
  dt
}

build_class_counts_8 <- function(dt) {
  # Four composite classes:
  # require both availability and flux to be significant
  composite_dt <- rbindlist(list(
    dt[sig_both == TRUE & storyline == "wetter-accelerated",
       .(scenario, region, class = "wetter-accelerated")],
    dt[sig_both == TRUE & storyline == "wetter-decelerated",
       .(scenario, region, class = "wetter-decelerated")],
    dt[sig_both == TRUE & storyline == "drier-accelerated",
       .(scenario, region, class = "drier-accelerated")],
    dt[sig_both == TRUE & storyline == "drier-decelerated",
       .(scenario, region, class = "drier-decelerated")]
  ), use.names = TRUE)
  
  # Four single classes:
  # wetter/drier based on significant availability only
  # accelerating/decelerating based on significant flux only
  single_dt <- rbindlist(list(
    dt[sig_avail == TRUE & avail_sign == "wetter",
       .(scenario, region, class = "wetter")],
    dt[sig_avail == TRUE & avail_sign == "drier",
       .(scenario, region, class = "drier")],
    dt[sig_flux == TRUE & flux_sign == "accelerating",
       .(scenario, region, class = "accelerating")],
    dt[sig_flux == TRUE & flux_sign == "decelerating",
       .(scenario, region, class = "decelerating")]
  ), use.names = TRUE)
  
  counts <- rbind(composite_dt, single_dt, use.names = TRUE)
  
  counts <- counts[
    ,
    .(n_sig = .N),
    by = .(scenario, region, class)
  ]
  
  counts
}

complete_class_table <- function(counts, dt_members) {
  n_members <- dt_members[
    ,
    .(n_total = uniqueN(sim_id)),
    by = .(scenario, region)
  ]
  
  out <- CJ(
    scenario = factor(SCENARIO_LEVELS, levels = SCENARIO_LEVELS),
    region = sort(unique(dt_members$region)),
    class = factor(CLASS_LEVELS_8, levels = CLASS_LEVELS_8)
  )
  
  out <- merge(
    out,
    counts,
    by = c("scenario", "region", "class"),
    all.x = TRUE
  )
  
  out[is.na(n_sig), n_sig := 0L]
  
  out <- merge(
    out,
    n_members,
    by = c("scenario", "region"),
    all.x = TRUE
  )
  
  out[, prop_sig := fifelse(n_total > 0, n_sig / n_total, NA_real_)]
  out[, likelihood := classify_likelihood(prop_sig)]
  out[, likelihood := factor(
    likelihood,
    levels = LIKELIHOOD_LEVELS,
    ordered = TRUE
  )]
  
  out[]
}
choose_dominant_class <- function(dt_full) {
  dt <- copy(dt_full)
  
  # rank likelihood strength
  dt[, likelihood_rank := fcase(
    likelihood == "no_change", 1L,
    likelihood == "likely", 2L,
    likelihood == "most likely", 3L,
    likelihood == "confident", 4L,
    default = NA_integer_
  )]
  
  # If you want composite classes to win ties over single classes,
  # assign higher priority here.
  dt[, class_priority := fcase(
    class %in% c(
      "wetter-accelerated",
      "wetter-decelerated",
      "drier-accelerated",
      "drier-decelerated"
    ), 2L,
    default = 1L
  )]
  
  dominant <- dt[
    order(
      scenario,
      region,
      -likelihood_rank,
      -prop_sig,
      -n_sig,
      -class_priority,
      class
    )
  ][
    ,
    .SD[1],
    by = .(scenario, region)
  ]
  
  # Optional global fallback label if nothing reaches likely/confident
  dominant[
    likelihood == "no_change",
    class_plot := "no_change"
  ]
  dominant[
    likelihood != "no_change",
    class_plot := as.character(class)
  ]
  
  dominant
}

# Build outputs ===============================================================

region_members <- prepare_region_members(
  dt = ensemble_region,
  thres_significance = THRES_SIGNIFICANCE
)

class_counts_8 <- build_class_counts_8(region_members)

region_likelihood_8 <- complete_class_table(
  counts = class_counts_8,
  dt_members = region_members
)

region_dominant_8 <- choose_dominant_class(region_likelihood_8)

# Optional simplified label combining class and likelihood =====================

region_dominant_8[
  ,
  class_likelihood := fifelse(
    class_plot == "no_change",
    "no_change",
    paste0(class_plot, "_", likelihood)
  )
]

# Save ========================================================================

saveRDS(
  region_likelihood_8,
  file.path(PATH_OUTPUT_DATA, "region_storyline_likelihood_8classes.Rds")
)

saveRDS(
  region_dominant_8,
  file.path(PATH_OUTPUT_DATA, "region_storyline_mode_8classes.Rds")
)

