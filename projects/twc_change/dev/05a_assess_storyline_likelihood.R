# ============================================================================
# Assess likelihood of all 8 mechanistic storylines
#
# Uses:
#   1) member_level_region_summary
#   2) twc_grid_classes  (for region metadata join at the end)
#
# Produces:
#   all_storylines_probability.Rds
# ============================================================================

source("source/twc_change.R")
source("source/storyline_specs.R")

# Helpers =====================================================================

classify_support <- function(p) {
  fcase(
    p < 0.05, "confident_not_happening",
    p < 0.15, "likely_not_happening",
    p < 0.375, "no_clear_signal",
    p < 0.75, "likely_happening",
    default  = "confident_happening"
  )
}

score_binary <- function(dt, spec) {
  dt[, criteria_flag := eval(spec$criteria)]
  sub_variants <- if (is.null(spec$sub_variants)) list() else spec$sub_variants        # NULL-safe
  for (nm in names(sub_variants)) {
    dt[, (nm) := eval(sub_variants[[nm]])]
  }
  dt
}

score_ordinal <- function(dt, spec) {
  dt         <- score_binary(dt, spec)
  sub_cols <- names(if (is.null(spec$sub_variants)) list() else spec$sub_variants)    # may be character(0)
  if (length(sub_cols) > 0) {
    dt[, ordinal_score := as.integer(criteria_flag) +
         rowSums(.SD == TRUE, na.rm = TRUE),
       .SDcols = sub_cols]
  } else {
    dt[, ordinal_score := as.integer(criteria_flag)]
  }
  dt
}

score_magnitude <- function(dt, spec) {
  dt <- score_binary(dt, spec)
  dt[, magnitude_weight := fifelse(
    criteria_flag,
    (abs(prec_abs_change) + abs(avail_abs_change)) / 2,
    NA_real_
  )]
  dt
}

add_score <- function(dt, spec, method) {
  switch(method,
         binary    = score_binary(dt, spec),
         ordinal   = score_ordinal(dt, spec),
         magnitude = score_magnitude(dt, spec),
         stop("Unknown score_method: ", method)
  )
}

summarise_by_region <- function(dt, spec, method) {
  
  sub_cols <- names(if (is.null(spec$sub_variants)) list() else spec$sub_variants)      # may be character(0)
  
  sub_means <- if (length(sub_cols) > 0) {
    setNames(
      lapply(sub_cols, function(v) {
        substitute(mean(V, na.rm = TRUE), list(V = as.name(v)))
      }),
      paste0("p_", sub_cols)
    )
  } else {
    list()
  }
  
  extra <- switch(method,
                  ordinal   = list(
                    mean_ordinal = quote(mean(ordinal_score, na.rm = TRUE))
                  ),
                  magnitude = list(
                    mean_magnitude = quote(mean(magnitude_weight, na.rm = TRUE)),
                    p_mk_sig       = quote(mean(criteria_flag & prec_mk_sig, na.rm = TRUE))
                  ),
                  list()
  )
  
  all_exprs <- c(
    list(
      n_member = quote(as.integer(.N)),
      n_story  = quote(sum(criteria_flag, na.rm = TRUE)),
      p_story  = quote(mean(criteria_flag, na.rm = TRUE))
    ),
    sub_means,
    extra
  )
  
  dt[, eval(as.call(c(list(quote(list)), all_exprs))), by = region]
}

add_support_cols <- function(out) {
  p_cols <- grep("^p_", names(out), value = TRUE)
  if (length(p_cols) > 0) {
    out[, paste0("support_", p_cols) := lapply(.SD, classify_support),
        .SDcols = p_cols]
  }
  out
}

assess_storyline <- function(member_features, spec, score_method = "binary") {
  
  missing_regions <- setdiff(spec$regions, unique(member_features$region))
  if (length(missing_regions) > 0) {
    warning(spec$label, ": regions not found in data — ",
            paste(missing_regions, collapse = ", "))
  }
  
  dt  <- copy(member_features[region %in% spec$regions])
  dt  <- add_score(dt, spec, method = score_method)
  out <- summarise_by_region(dt, spec, method = score_method)
  out <- add_support_cols(out)
  out[, storyline := spec$label]
  out
}

# Inputs ======================================================================

member_features <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds"))
)

region_meta <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds"))
)[,
  .(
    region_full  = region_full[!is.na(region_full)][1],
    circulation  = circulation[!is.na(circulation)][1],
    lat_zone     = lat_zone[!is.na(lat_zone)][1],
    climate_main = climate_main[!is.na(climate_main)][1],
    hydrobelt    = hydrobelt[!is.na(hydrobelt)][1]
  ),
  by = region
]

# Score method: "binary" | "ordinal" | "magnitude"
SCORE_METHOD <- "binary"

# Run all storylines ==========================================================

results <- lapply(
  STORYLINE_SPECS,
  assess_storyline,
  member_features = member_features,
  score_method    = SCORE_METHOD
)

# Join region metadata ========================================================

results <- lapply(results, function(dt) {
  merge(dt, region_meta, by = "region", all.x = TRUE)
})

# Save combined output ========================================================

all_storylines <- rbindlist(results, fill = TRUE)

saveRDS(
  all_storylines,
  file.path(PATH_OUTPUT_DATA, "all_storylines_probability.Rds")
)
