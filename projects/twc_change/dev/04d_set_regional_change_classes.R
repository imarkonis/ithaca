# ============================================================================
# Build regional TWC storyline likelihood classes for IPCC hexagon mapping
#
# This script:
# 1. Counts significant ensemble members per region and scenario for:
#    a) four direct compound classes
#    b) four single-sign classes
# 2. Adds an aggregated "all" scenario that pools ensemble members from all
#    scenarios
# 3. Converts counts to likelihood bins:
#      no_change   : p < 0.05
#      likely      : 0.05 <= p < 0.10
#      most_likely : 0.10 <= p < 0.20
#      confident   : p >= 0.20
# 4. Selects one dominant class per region and scenario for:
#    a) acceleration
#    b) availability
#    c) direct compound
# 5. Builds one additional compound class from marginal dominant classes using:
#      n_sig = min(n_sig_availability, n_sig_acceleration)
# 6. Saves both the full class table and map ready dominant tables
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

library(data.table)

# Inputs ======================================================================

ensemble_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds")
)

# Constants ===================================================================

THRES_SIGNIFICANCE <- 0.05

SCENARIO_LEVELS_RAW <- c(
  "base",
  "clim_dominant",
  "evap_dominant",
  "prec_dominant",
  "trend_dominant"
)

SCENARIO_LEVELS <- c("all", SCENARIO_LEVELS_RAW)

COMPOUND_CLASSES <- c(
  "wetter-accelerated",
  "wetter-decelerated",
  "drier-accelerated",
  "drier-decelerated"
)

AVAIL_CLASSES <- c("wetter", "drier")

ACCEL_CLASSES <- c("accelerating", "decelerating")

CLASS_LEVELS_8 <- c(
  COMPOUND_CLASSES,
  AVAIL_CLASSES,
  ACCEL_CLASSES
)

LIKELIHOOD_LEVELS <- c(
  "no_change",
  "likely",
  "most_likely",
  "confident"
)

# Helpers =====================================================================

classify_likelihood <- function(prop_sig) {
  fcase(
    !is.finite(prop_sig), NA_character_,
    prop_sig < 0.05, "no_change",
    prop_sig < 0.10, "likely",
    prop_sig < 0.20, "most_likely",
    default = "confident"
  )
}

prepare_region_members <- function(dt, thres_significance) {
  dt <- copy(as.data.table(dt))
  
  dt <- dt[
    source_type == "mc" &
      region != "GLOBAL" &
      !is.na(storyline) &
      is.finite(avail_abs_change) &
      is.finite(flux_abs_change)
  ]
  
  dt[, sig_avail := avail_mk_p < thres_significance]
  dt[, sig_flux := flux_mk_p < thres_significance]
  dt[, sig_both := sig_avail & sig_flux]
  
  dt[, avail_sign := fcase(
    avail_abs_change > 0, "wetter",
    avail_abs_change < 0, "drier",
    default = NA_character_
  )]
  
  dt[, flux_sign := fcase(
    flux_abs_change > 0, "accelerating",
    flux_abs_change < 0, "decelerating",
    default = NA_character_
  )]
  
  dt[, scenario := as.character(scenario)]
  dt[, member_id := paste(scenario, sim_id, sep = "__")]
  
  dt[]
}

add_all_scenario <- function(dt) {
  dt <- copy(as.data.table(dt))
  
  dt_all <- copy(dt)
  dt_all[, scenario := "all"]
  
  out <- rbindlist(list(dt, dt_all), use.names = TRUE)
  out[, scenario := factor(scenario, levels = SCENARIO_LEVELS)]
  
  out[]
}

build_class_counts_8 <- function(dt) {
  compound_dt <- dt[
    sig_both == TRUE &
      storyline %in% COMPOUND_CLASSES,
    .(
      scenario,
      region,
      class = storyline
    )
  ]
  
  avail_dt <- dt[
    sig_avail == TRUE &
      avail_sign %in% AVAIL_CLASSES,
    .(
      scenario,
      region,
      class = avail_sign
    )
  ]
  
  accel_dt <- dt[
    sig_flux == TRUE &
      flux_sign %in% ACCEL_CLASSES,
    .(
      scenario,
      region,
      class = flux_sign
    )
  ]
  
  counts <- rbindlist(
    list(compound_dt, avail_dt, accel_dt),
    use.names = TRUE
  )[
    ,
    .(n_sig = .N),
    by = .(scenario, region, class)
  ]
  
  counts[, class := factor(class, levels = CLASS_LEVELS_8)]
  
  counts[]
}

complete_class_table <- function(counts, dt_members) {
  n_members <- dt_members[
    ,
    .(n_total = uniqueN(member_id)),
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

choose_dominant_subset <- function(dt_full, class_subset) {
  dt <- copy(as.data.table(dt_full))[class %in% class_subset]
  
  dt[, likelihood_rank := fcase(
    likelihood == "no_change", 1L,
    likelihood == "likely", 2L,
    likelihood == "most_likely", 3L,
    likelihood == "confident", 4L,
    default = NA_integer_
  )]
  
  dominant <- dt[
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
  
  all_regions <- unique(dt_full[, .(scenario, region, n_total)])
  
  dominant <- merge(
    all_regions,
    dominant,
    by = c("scenario", "region", "n_total"),
    all.x = TRUE
  )
  
  dominant[
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
  
  dominant[
    ,
    class_plot := fifelse(
      likelihood == "no_change" | is.na(class),
      "no_change",
      as.character(class)
    )
  ]
  
  dominant[
    ,
    class_likelihood := fifelse(
      class_plot == "no_change",
      "no_change",
      paste0(class_plot, "_", as.character(likelihood))
    )
  ]
  
  dominant[]
}

build_compound_from_marginals <- function(dt_avail, dt_accel) {
  out <- merge(
    dt_avail[
      ,
      .(
        scenario,
        region,
        n_total,
        avail_class = class,
        avail_n_sig = n_sig,
        avail_prop_sig = prop_sig,
        avail_likelihood = likelihood
      )
    ],
    dt_accel[
      ,
      .(
        scenario,
        region,
        n_total,
        accel_class = class,
        accel_n_sig = n_sig,
        accel_prop_sig = prop_sig,
        accel_likelihood = likelihood
      )
    ],
    by = c("scenario", "region", "n_total"),
    all = TRUE
  )
  
  out[
    ,
    compound_class := fifelse(
      is.na(avail_class) | is.na(accel_class),
      NA_character_,
      paste0(avail_class, "-", accel_class)
    )
  ]
  
  out[
    ,
    n_sig := fifelse(
      is.na(avail_class) | is.na(accel_class),
      0L,
      pmin(avail_n_sig, accel_n_sig)
    )
  ]
  
  out[, prop_sig := fifelse(n_total > 0, n_sig / n_total, NA_real_)]
  out[, likelihood := classify_likelihood(prop_sig)]
  out[, likelihood := factor(
    likelihood,
    levels = LIKELIHOOD_LEVELS,
    ordered = TRUE
  )]
  
  out[
    likelihood == "no_change" | is.na(compound_class),
    compound_class := NA_character_
  ]
  
  out[
    ,
    class_plot := fifelse(
      is.na(compound_class) | likelihood == "no_change",
      "no_change",
      compound_class
    )
  ]
  
  out[
    ,
    class_likelihood := fifelse(
      class_plot == "no_change",
      "no_change",
      paste0(class_plot, "_", as.character(likelihood))
    )
  ]
  
  setcolorder(
    out,
    c(
      "scenario", "region", "n_total",
      "avail_class", "avail_n_sig", "avail_prop_sig", "avail_likelihood",
      "accel_class", "accel_n_sig", "accel_prop_sig", "accel_likelihood",
      "compound_class", "n_sig", "prop_sig", "likelihood",
      "class_plot", "class_likelihood"
    )
  )
  
  out[]
}

# Build outputs ================================================================

region_members <- prepare_region_members(
  dt = ensemble_region,
  thres_significance = THRES_SIGNIFICANCE
)

region_members <- add_all_scenario(region_members)

class_counts_8 <- build_class_counts_8(region_members)

region_likelihood_8 <- complete_class_table(
  counts = class_counts_8,
  dt_members = region_members
)

region_dominant_accel <- choose_dominant_subset(
  dt_full = region_likelihood_8,
  class_subset = ACCEL_CLASSES
)

region_dominant_avail <- choose_dominant_subset(
  dt_full = region_likelihood_8,
  class_subset = AVAIL_CLASSES
)

region_dominant_compound <- choose_dominant_subset(
  dt_full = region_likelihood_8,
  class_subset = COMPOUND_CLASSES
)

region_dominant_compound_from_marginals <- build_compound_from_marginals(
  dt_avail = region_dominant_avail,
  dt_accel = region_dominant_accel
)

region_dominant_8 <- Reduce(
  function(x, y) merge(x, y, by = c("scenario", "region", "n_total"), all = TRUE),
  list(
    region_dominant_accel[
      ,
      .(
        scenario,
        region,
        n_total,
        accel_class = class_plot,
        accel_n_sig = n_sig,
        accel_prop_sig = prop_sig,
        accel_likelihood = likelihood,
        accel_class_likelihood = class_likelihood
      )
    ],
    region_dominant_avail[
      ,
      .(
        scenario,
        region,
        n_total,
        avail_class = class_plot,
        avail_n_sig = n_sig,
        avail_prop_sig = prop_sig,
        avail_likelihood = likelihood,
        avail_class_likelihood = class_likelihood
      )
    ],
    region_dominant_compound[
      ,
      .(
        scenario,
        region,
        n_total,
        compound_class = class_plot,
        compound_n_sig = n_sig,
        compound_prop_sig = prop_sig,
        compound_likelihood = likelihood,
        compound_class_likelihood = class_likelihood
      )
    ],
    region_dominant_compound_from_marginals[
      ,
      .(
        scenario,
        region,
        n_total,
        compound_from_marginals = class_plot,
        compound_from_marginals_n_sig = n_sig,
        compound_from_marginals_prop_sig = prop_sig,
        compound_from_marginals_likelihood = likelihood,
        compound_from_marginals_class_likelihood = class_likelihood
      )
    ]
  )
)

# Save =========================================================================

saveRDS(
  region_likelihood_8,
  file.path(PATH_OUTPUT_DATA, "region_storyline_likelihood_8classes.Rds")
)

saveRDS(
  region_dominant_8,
  file.path(PATH_OUTPUT_DATA, "region_storyline_mode_8classes.Rds")
)