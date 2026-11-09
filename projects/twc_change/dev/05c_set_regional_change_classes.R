# ============================================================================
# Build regional TWC storyline likelihood classes for IPCC hexagon mapping
# ============================================================================

source("source/twc_change.R")

library(data.table)

# ============================================================================
# Input file
# ============================================================================

mc_region_metrics_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_metrics_base.Rds")
)

# ============================================================================
# Constants & variables
# ============================================================================

THRES_SIGNIFICANCE <- 0.05

# Change metric used for storyline direction:
# positive avail = wetter
# negative avail = drier
# positive flux  = accelerated / accelerating
# negative flux  = decelerated / decelerating
CHANGE_COL <- "diff_2002_2021_minus_1982_2001"

# Significance metric:
# "slope_full_p_value" = trend-based significance
# "diff_p_value"      = period-difference significance
PVAL_COL <- "slope_full_p_value"

COMPOUND_CLASSES <- c(
  "wetter-accelerated",
  "wetter-decelerated",
  "drier-accelerated",
  "drier-decelerated"
)

AVAIL_CLASSES <- c(
  "wetter",
  "drier"
)

ACCEL_CLASSES <- c(
  "accelerating",
  "decelerating"
)

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

# ============================================================================
# Functions
# ============================================================================

classify_likelihood_single <- function(p) {
  fcase(
    !is.finite(p), NA_character_,
    p < 0.05,      "no_change",
    p < 0.25,      "likely",
    p < 0.50,      "most_likely",
    default        = "confident"
  )
}

classify_likelihood_compound <- function(p) {
  fcase(
    !is.finite(p), NA_character_,
    p < 0.025,     "no_change",
    p < 0.125,      "likely",
    p < 0.25,      "most_likely",
    default        = "confident"
  )
}

prepare_members <- function(dt,
                            change_col = CHANGE_COL,
                            pval_col   = PVAL_COL) {
  
  dt <- copy(as.data.table(dt))
  
  dt_long <- dt[
    region != "GLOBAL" &
      variable %in% c("avail", "flux") &
      is.finite(get(change_col)) &
      is.finite(get(pval_col)),
    .(
      sim_id     = sim,
      region     = as.character(region),
      variable   = as.character(variable),
      abs_change = get(change_col),
      p_value    = get(pval_col)
    )
  ]
  
  dt_wide <- dcast(
    dt_long,
    sim_id + region ~ variable,
    value.var = c("abs_change", "p_value")
  )
  
  setnames(
    dt_wide,
    old = c(
      "abs_change_avail",
      "abs_change_flux",
      "p_value_avail",
      "p_value_flux"
    ),
    new = c(
      "avail_abs_change",
      "flux_abs_change",
      "avail_p_value",
      "flux_p_value"
    )
  )
  
  dt_wide <- dt_wide[
    is.finite(avail_abs_change) &
      is.finite(flux_abs_change) &
      is.finite(avail_p_value) &
      is.finite(flux_p_value)
  ]
  
  dt_wide[, sig_avail := avail_p_value < THRES_SIGNIFICANCE]
  dt_wide[, sig_flux  := flux_p_value  < THRES_SIGNIFICANCE]
  dt_wide[, sig_both  := sig_avail & sig_flux]
  
  dt_wide[, avail_sign := fcase(
    avail_abs_change > 0, "wetter",
    avail_abs_change < 0, "drier",
    default              = NA_character_
  )]
  
  # Marginal acceleration labels
  dt_wide[, flux_sign := fcase(
    flux_abs_change > 0, "accelerating",
    flux_abs_change < 0, "decelerating",
    default             = NA_character_
  )]
  
  # Compound acceleration labels
  dt_wide[, flux_compound_sign := fcase(
    flux_abs_change > 0, "accelerated",
    flux_abs_change < 0, "decelerated",
    default             = NA_character_
  )]
  
  dt_wide[, compound_class := fifelse(
    !is.na(avail_sign) & !is.na(flux_compound_sign),
    paste0(avail_sign, "-", flux_compound_sign),
    NA_character_
  )]
  
  setcolorder(
    dt_wide,
    c(
      "sim_id",
      "region",
      "avail_abs_change",
      "flux_abs_change",
      "avail_p_value",
      "flux_p_value",
      "sig_avail",
      "sig_flux",
      "sig_both",
      "avail_sign",
      "flux_sign",
      "flux_compound_sign",
      "compound_class"
    )
  )
  
  setkey(dt_wide, region, sim_id)
  
  dt_wide[]
}

count_classes <- function(members) {
  
  compound <- members[
    sig_both == TRUE &
      compound_class %in% COMPOUND_CLASSES,
    .(
      region,
      class = compound_class,
      type  = "compound"
    )
  ]
  
  avail <- members[
    sig_avail == TRUE &
      avail_sign %in% AVAIL_CLASSES,
    .(
      region,
      class = avail_sign,
      type  = "single"
    )
  ]
  
  accel <- members[
    sig_flux == TRUE &
      flux_sign %in% ACCEL_CLASSES,
    .(
      region,
      class = flux_sign,
      type  = "single"
    )
  ]
  
  out <- rbindlist(
    list(compound, avail, accel),
    use.names = TRUE
  )[
    ,
    .(
      n_sig = .N,
      type  = type[1]
    ),
    by = .(region, class)
  ]
  
  out[, class := factor(
    class,
    levels = CLASS_LEVELS_8
  )]
  
  setkey(out, region, class)
  
  out[]
}

build_full_table <- function(counts, members) {
  
  n_total <- members[
    ,
    .(n_total = uniqueN(sim_id)),
    by = region
  ]
  
  out <- CJ(
    region = sort(unique(members$region)),
    class  = CLASS_LEVELS_8
  )
  
  out[, class := factor(
    class,
    levels = CLASS_LEVELS_8
  )]
  
  class_type <- data.table(
    class = factor(
      CLASS_LEVELS_8,
      levels = CLASS_LEVELS_8
    ),
    type = fifelse(
      CLASS_LEVELS_8 %in% COMPOUND_CLASSES,
      "compound",
      "single"
    )
  )
  
  out <- merge(
    out,
    counts[, .(region, class, n_sig)],
    by = c("region", "class"),
    all.x = TRUE
  )
  
  out <- merge(
    out,
    class_type,
    by = "class",
    all.x = TRUE
  )
  
  out <- merge(
    out,
    n_total,
    by = "region",
    all.x = TRUE
  )
  
  out[is.na(n_sig), n_sig := 0L]
  
  out[, prop_sig := fifelse(
    n_total > 0,
    n_sig / n_total,
    NA_real_
  )]
  
  out[type == "compound",
      likelihood := classify_likelihood_compound(prop_sig)]
  
  out[type == "single",
      likelihood := classify_likelihood_single(prop_sig)]
  
  out[, likelihood := factor(
    likelihood,
    levels = LIKELIHOOD_LEVELS,
    ordered = TRUE
  )]
  
  setcolorder(
    out,
    c(
      "region",
      "class",
      "type",
      "n_total",
      "n_sig",
      "prop_sig",
      "likelihood"
    )
  )
  
  setkey(out, region, class)
  
  out[]
}

choose_dominant <- function(full_dt, class_subset) {
  
  dt <- full_dt[
    as.character(class) %in% class_subset
  ][
    order(
      region,
      -as.integer(likelihood),
      -prop_sig,
      -n_sig,
      class
    )
  ][
    ,
    .SD[1],
    by = region
  ]
  
  out <- merge(
    unique(full_dt[, .(region, n_total)]),
    dt[
      ,
      .(
        region,
        class,
        n_sig,
        prop_sig,
        likelihood
      )
    ],
    by = "region",
    all.x = TRUE
  )
  
  out[
    is.na(likelihood) | likelihood == "no_change",
    `:=`(
      class      = NA,
      n_sig      = 0L,
      prop_sig   = 0,
      likelihood = factor(
        "no_change",
        levels = LIKELIHOOD_LEVELS,
        ordered = TRUE
      )
    )
  ]
  
  out[, class_plot := fifelse(
    is.na(class) | likelihood == "no_change",
    "no_change",
    as.character(class)
  )]
  
  out[, class_likelihood := fifelse(
    class_plot == "no_change",
    "no_change",
    paste0(class_plot, "_", likelihood)
  )]
  
  setcolorder(
    out,
    c(
      "region",
      "n_total",
      "class",
      "n_sig",
      "prop_sig",
      "likelihood",
      "class_plot",
      "class_likelihood"
    )
  )
  
  setkey(out, region)
  
  out[]
}

build_compound_from_marginals <- function(members,
                                          dom_avail,
                                          dom_accel) {
  
  choices <- merge(
    dom_avail[
      ,
      .(
        region,
        n_total,
        avail_class = fifelse(
          class_plot == "no_change",
          NA_character_,
          class_plot
        ),
        avail_n_sig      = n_sig,
        avail_prop_sig   = prop_sig,
        avail_likelihood = likelihood
      )
    ],
    dom_accel[
      ,
      .(
        region,
        accel_class = fifelse(
          class_plot == "no_change",
          NA_character_,
          class_plot
        ),
        accel_n_sig      = n_sig,
        accel_prop_sig   = prop_sig,
        accel_likelihood = likelihood
      )
    ],
    by = "region",
    all = TRUE
  )
  
  member_hits <- merge(
    members[
      ,
      .(
        region,
        sim_id,
        avail_sign,
        flux_sign,
        sig_avail,
        sig_flux
      )
    ],
    choices[
      ,
      .(
        region,
        avail_class,
        accel_class
      )
    ],
    by = "region",
    all.x = FALSE,
    allow.cartesian = TRUE
  )
  
  member_hits[, hit := (
    !is.na(avail_class) &
      !is.na(accel_class) &
      sig_avail == TRUE &
      sig_flux == TRUE &
      avail_sign == avail_class &
      flux_sign == accel_class
  )]
  
  hits <- member_hits[
    ,
    .(n_sig = sum(hit, na.rm = TRUE)),
    by = region
  ]
  
  out <- merge(
    choices,
    hits,
    by = "region",
    all.x = TRUE
  )
  
  out[is.na(n_sig), n_sig := 0L]
  
  out[, prop_sig := fifelse(
    n_total > 0,
    n_sig / n_total,
    NA_real_
  )]
  
  out[, likelihood := classify_likelihood_compound(prop_sig)]
  
  out[, likelihood := factor(
    likelihood,
    levels = LIKELIHOOD_LEVELS,
    ordered = TRUE
  )]
  
  out[, accel_compound_class := fcase(
    accel_class == "accelerating", "accelerated",
    accel_class == "decelerating", "decelerated",
    default                     = NA_character_
  )]
  
  out[, compound_class := fifelse(
    is.na(avail_class) |
      is.na(accel_compound_class) |
      likelihood == "no_change",
    NA_character_,
    paste0(avail_class, "-", accel_compound_class)
  )]
  
  out[, class_plot := fifelse(
    is.na(compound_class),
    "no_change",
    compound_class
  )]
  
  out[, class_likelihood := fifelse(
    class_plot == "no_change",
    "no_change",
    paste0(class_plot, "_", likelihood)
  )]
  
  setcolorder(
    out,
    c(
      "region",
      "n_total",
      "avail_class",
      "accel_class",
      "compound_class",
      "avail_n_sig",
      "accel_n_sig",
      "n_sig",
      "avail_prop_sig",
      "accel_prop_sig",
      "prop_sig",
      "avail_likelihood",
      "accel_likelihood",
      "likelihood",
      "class_plot",
      "class_likelihood"
    )
  )
  
  setkey(out, region)
  
  out[]
}

rename_dom <- function(dt, prefix) {
  
  cols <- c(
    "class_plot",
    "n_sig",
    "prop_sig",
    "likelihood",
    "class_likelihood"
  )
  
  out <- dt[
    ,
    .SD,
    .SDcols = c(
      "region",
      "n_total",
      cols
    )
  ]
  
  setnames(
    out,
    old = cols,
    new = paste0(
      prefix,
      c(
        "_class",
        "_n_sig",
        "_prop_sig",
        "_likelihood",
        "_class_likelihood"
      )
    )
  )
  
  setkey(out, region)
  
  out[]
}

# ============================================================================
# Analysis
# ============================================================================

members <- prepare_members(
  dt         = mc_region_metrics_base,
  change_col = CHANGE_COL,
  pval_col   = PVAL_COL
)

counts <- count_classes(
  members = members
)

full_table <- build_full_table(
  counts  = counts,
  members = members
)

dom_accel <- choose_dominant(
  full_dt      = full_table,
  class_subset = ACCEL_CLASSES
)

dom_avail <- choose_dominant(
  full_dt      = full_table,
  class_subset = AVAIL_CLASSES
)

dom_compound <- choose_dominant(
  full_dt      = full_table,
  class_subset = COMPOUND_CLASSES
)

dom_marginal <- build_compound_from_marginals(
  members   = members,
  dom_avail = dom_avail,
  dom_accel = dom_accel
)

region_dominant_8 <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = c("region", "n_total"),
      all = TRUE
    )
  },
  list(
    rename_dom(dom_accel,    "accel"),
    rename_dom(dom_avail,    "avail"),
    rename_dom(dom_compound, "compound"),
    dom_marginal[
      ,
      .(
        region,
        n_total,
        compound_marginal_class            = class_plot,
        compound_marginal_n_sig            = n_sig,
        compound_marginal_prop_sig         = prop_sig,
        compound_marginal_likelihood       = likelihood,
        compound_marginal_class_likelihood = class_likelihood
      )
    ]
  )
)

setkey(region_dominant_8, region)

region_storyline_clean <- region_dominant_8[
  ,
  .(
    region,
    
    accel_class,
    accel_prop_sig,
    accel_likelihood,
    
    avail_class,
    avail_prop_sig,
    avail_likelihood,
    
    compound_class,
    compound_prop_sig,
    compound_likelihood,
    
    compound_marginal_class,
    compound_marginal_prop_sig,
    compound_marginal_likelihood
  )
]

setkey(region_storyline_clean, region)

# ============================================================================
# Output
# ============================================================================

saveRDS(
  full_table,
  file.path(PATH_OUTPUT_DATA, "region_storyline_likelihood_8classes.Rds")
)

saveRDS(
  region_dominant_8,
  file.path(PATH_OUTPUT_DATA, "region_storyline_mode_8classes.Rds")
)

saveRDS(
  region_storyline_clean,
  file.path(PATH_OUTPUT_DATA, "region_storyline_clean.Rds")
)

# ============================================================================
# Validation
# ============================================================================

cat("\nInput file:\n")
print(file.path(PATH_OUTPUT_DATA, "mc_region_metrics_base.Rds"))

cat("\nOutput files:\n")
print(file.path(PATH_OUTPUT_DATA, "region_storyline_likelihood_8classes.Rds"))
print(file.path(PATH_OUTPUT_DATA, "region_storyline_mode_8classes.Rds"))
print(file.path(PATH_OUTPUT_DATA, "region_storyline_clean.Rds"))

cat("\nInput dimensions:\n")
print(dim(mc_region_metrics_base))

cat("\nMembers dimensions:\n")
print(dim(members))

cat("\nFull likelihood table dimensions:\n")
print(dim(full_table))

cat("\nDominant storyline table dimensions:\n")
print(dim(region_dominant_8))

cat("\nClean downstream table dimensions:\n")
print(dim(region_storyline_clean))

cat("\nNumber of ensemble members per region:\n")
print(
  members[
    ,
    .(n_total = uniqueN(sim_id)),
    by = region
  ][
    ,
    .(
      n_regions = .N,
      min_n     = min(n_total),
      median_n  = median(n_total),
      max_n     = max(n_total)
    )
  ]
)

cat("\nRaw compound class counts from members:\n")
print(
  members[
    sig_both == TRUE,
    .N,
    by = compound_class
  ][
    order(compound_class)
  ]
)

cat("\nFull-table compound support:\n")
print(
  full_table[
    as.character(class) %in% COMPOUND_CLASSES,
    .(
      n_regions        = .N,
      total_n_sig      = sum(n_sig),
      max_prop_sig     = max(prop_sig, na.rm = TRUE),
      n_nonzero        = sum(n_sig > 0),
      n_likely_or_more = sum(likelihood != "no_change", na.rm = TRUE)
    )
  ]
)

cat("\nAvailability dominant classes:\n")
print(
  region_dominant_8[
    ,
    .N,
    by = .(
      avail_class,
      avail_likelihood
    )
  ][
    order(avail_class, avail_likelihood)
  ]
)

cat("\nAcceleration dominant classes:\n")
print(
  region_dominant_8[
    ,
    .N,
    by = .(
      accel_class,
      accel_likelihood
    )
  ][
    order(accel_class, accel_likelihood)
  ]
)

cat("\nExact compound dominant classes:\n")
print(
  region_dominant_8[
    ,
    .N,
    by = .(
      compound_class,
      compound_likelihood
    )
  ][
    order(compound_class, compound_likelihood)
  ]
)

cat("\nMarginal compound dominant classes:\n")
print(
  region_dominant_8[
    ,
    .N,
    by = .(
      compound_marginal_class,
      compound_marginal_likelihood
    )
  ][
    order(
      compound_marginal_class,
      compound_marginal_likelihood
    )
  ]
)

cat("\nFull-table class completeness:\n")
print(
  full_table[
    ,
    .N,
    by = class
  ][
    order(class)
  ]
)

cat("\nPreview: region_storyline_clean\n")
print(region_storyline_clean)

cat("\nPreview: region_dominant_8\n")
print(region_dominant_8)