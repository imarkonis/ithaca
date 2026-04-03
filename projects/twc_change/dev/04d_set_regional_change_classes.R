# ============================================================================
# Build regional TWC storyline likelihood classes for IPCC hexagon mapping
# ============================================================================

source("source/twc_change.R")

# Constants ===================================================================

THRES_SIGNIFICANCE <- 0.05

SCENARIO_LEVELS <- c("all", "base", "clim_dominant", "evap_dominant",
                     "prec_dominant", "trend_dominant")

COMPOUND_CLASSES <- c("wetter-accelerated", "wetter-decelerated",
                      "drier-accelerated",  "drier-decelerated")
AVAIL_CLASSES    <- c("wetter", "drier")
ACCEL_CLASSES    <- c("accelerating", "decelerating")
CLASS_LEVELS_8   <- c(COMPOUND_CLASSES, AVAIL_CLASSES, ACCEL_CLASSES)

LIKELIHOOD_LEVELS <- c("no_change", "likely", "most_likely", "confident")

# Likelihood functions ========================================================

# Single-variable: null = 0.5
classify_likelihood_single <- function(p) {
  fcase(
    !is.finite(p), NA_character_,
    p < 0.05,      "no_change",
    p < 0.10,      "likely",
    p < 0.20,      "most_likely",
    default        = "confident"
  )
}

# Compound (2 criteria): null = 0.25, thresholds halved accordingly
classify_likelihood_compound <- function(p) {
  fcase(
    !is.finite(p), NA_character_,
    p < 0.025,     "no_change",
    p < 0.05,      "likely",
    p < 0.10,      "most_likely",
    default        = "confident"
  )
}

# Helpers =====================================================================

prepare_members <- function(dt) {
  dt <- copy(as.data.table(dt))[
    source_type == "mc" &
      region      != "GLOBAL" &
      !is.na(storyline) &
      is.finite(avail_abs_change) &
      is.finite(flux_abs_change)
  ]
  
  dt[, sig_avail := avail_mk_p < THRES_SIGNIFICANCE]
  dt[, sig_flux  := flux_mk_p  < THRES_SIGNIFICANCE]
  dt[, sig_both  := sig_avail & sig_flux]
  
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
  
  dt[, scenario  := as.character(scenario)]
  dt[, member_id := paste(scenario, sim_id, sep = "__")]
  
  # duplicate rows into "all" scenario
  rbindlist(list(dt, copy(dt)[, scenario := "all"]), use.names = TRUE)[
    , scenario := factor(scenario, levels = SCENARIO_LEVELS)
  ]
}

count_classes <- function(dt) {
  compound <- dt[sig_both  == TRUE & storyline  %in% COMPOUND_CLASSES,
                 .(scenario, region, class = storyline,   type = "compound")]
  avail    <- dt[sig_avail == TRUE & avail_sign %in% AVAIL_CLASSES,
                 .(scenario, region, class = avail_sign,  type = "single")]
  accel    <- dt[sig_flux  == TRUE & flux_sign  %in% ACCEL_CLASSES,
                 .(scenario, region, class = flux_sign,   type = "single")]
  
  rbindlist(list(compound, avail, accel))[
    , .(n_sig = .N, type = type[1]),
    by = .(scenario, region, class)
  ][, class := factor(class, levels = CLASS_LEVELS_8)]
}

build_full_table <- function(counts, dt) {
  n_total <- dt[, .(n_total = uniqueN(member_id)), by = .(scenario, region)]
  
  out <- CJ(
    scenario = factor(SCENARIO_LEVELS, levels = SCENARIO_LEVELS),
    region   = sort(unique(dt$region)),
    class    = factor(CLASS_LEVELS_8,  levels = CLASS_LEVELS_8)
  )
  
  out <- merge(out, counts,  by = c("scenario", "region", "class"), all.x = TRUE)
  out <- merge(out, n_total, by = c("scenario", "region"),          all.x = TRUE)
  
  out[is.na(n_sig), `:=`(n_sig = 0L, type = "single")]
  out[, prop_sig := fifelse(n_total > 0, n_sig / n_total, NA_real_)]
  
  # apply the appropriate likelihood function per class type
  out[, likelihood := fifelse(
    type == "compound",
    classify_likelihood_compound(prop_sig),
    classify_likelihood_single(prop_sig)
  )]
  out[, likelihood := factor(likelihood, levels = LIKELIHOOD_LEVELS, ordered = TRUE)]
  
  out[]
}

choose_dominant <- function(full_dt, class_subset) {
  dt <- full_dt[class %in% class_subset][
    order(scenario, region, -as.integer(likelihood), -prop_sig, -n_sig, class)
  ][, .SD[1], by = .(scenario, region)]
  
  out <- merge(
    unique(full_dt[, .(scenario, region, n_total)]),
    dt[, .(scenario, region, class, n_sig, prop_sig, likelihood)],  # only raw cols
    by = c("scenario", "region"),
    all.x = TRUE
  )
  
  out[is.na(likelihood) | likelihood == "no_change", `:=`(
    class      = NA_character_,
    n_sig      = 0L,
    prop_sig   = 0,
    likelihood = factor("no_change", levels = LIKELIHOOD_LEVELS, ordered = TRUE)
  )]
  
  # derived cols computed after merge
  out[, class_plot       := fifelse(is.na(class) | likelihood == "no_change",
                                    "no_change", as.character(class))]
  out[, class_likelihood := fifelse(class_plot == "no_change", "no_change",
                                    paste0(class_plot, "_", likelihood))]
  out[]
}

build_compound_from_marginals <- function(dt_avail, dt_accel) {
  out <- merge(
    dt_avail[, .(scenario, region, n_total,
                 avail_class = class, avail_n_sig = n_sig,
                 avail_prop_sig = prop_sig, avail_likelihood = likelihood)],
    dt_accel[, .(scenario, region,
                 accel_class = class, accel_n_sig = n_sig,
                 accel_prop_sig = prop_sig, accel_likelihood = likelihood)],
    by = c("scenario", "region"), all = TRUE
  )
  
  out[, n_sig := fifelse(
    is.na(avail_class) | is.na(accel_class), 0L,
    pmin(avail_n_sig, accel_n_sig)
  )]
  out[, prop_sig   := fifelse(n_total > 0, n_sig / n_total, NA_real_)]
  
  # marginal compound uses compound classifier (conservative)
  out[, likelihood := factor(
    classify_likelihood_compound(prop_sig),
    levels = LIKELIHOOD_LEVELS, ordered = TRUE
  )]
  
  out[, compound_class := fifelse(
    is.na(avail_class) | is.na(accel_class) | likelihood == "no_change",
    NA_character_,
    paste0(avail_class, "-", accel_class)
  )]
  out[, class_plot       := fifelse(is.na(compound_class), "no_change", compound_class)]
  out[, class_likelihood := fifelse(class_plot == "no_change", "no_change",
                                    paste0(class_plot, "_", likelihood))]
  out[]
}

# Run =========================================================================

members     <- prepare_members(readRDS(file.path(PATH_OUTPUT_DATA,
                                                 "member_level_region_summary.Rds")))
counts      <- count_classes(members)
full_table  <- build_full_table(counts, members)

dom_accel    <- choose_dominant(full_table, ACCEL_CLASSES)
dom_avail    <- choose_dominant(full_table, AVAIL_CLASSES)
dom_compound <- choose_dominant(full_table, COMPOUND_CLASSES)
dom_marginal <- build_compound_from_marginals(dom_avail, dom_accel)

# Wide dominant table =========================================================

rename_dom <- function(dt, prefix) {
  cols <- c("class_plot", "n_sig", "prop_sig", "likelihood", "class_likelihood")
  out  <- dt[, .SD, .SDcols = c("scenario", "region", "n_total", cols)]
  setnames(out, cols, paste0(prefix, c("_class", "_n_sig", "_prop_sig",
                                       "_likelihood", "_class_likelihood")))
  out
}

region_dominant_8 <- Reduce(
  function(x, y) merge(x, y, by = c("scenario", "region", "n_total"), all = TRUE),
  list(
    rename_dom(dom_accel,    "accel"),
    rename_dom(dom_avail,    "avail"),
    rename_dom(dom_compound, "compound"),
    dom_marginal[, .(scenario, region, n_total,
                     compound_marginal_class            = class_plot,
                     compound_marginal_n_sig            = n_sig,
                     compound_marginal_prop_sig         = prop_sig,
                     compound_marginal_likelihood       = likelihood,
                     compound_marginal_class_likelihood = class_likelihood)]
  )
)

# Save ========================================================================

saveRDS(full_table,       file.path(PATH_OUTPUT_DATA, "region_storyline_likelihood_8classes.Rds"))
saveRDS(region_dominant_8, file.path(PATH_OUTPUT_DATA, "region_storyline_mode_8classes.Rds"))
