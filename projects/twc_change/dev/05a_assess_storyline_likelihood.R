# ============================================================================
# Assess likelihood of all 8 mechanistic storylines
#
# Uses:
#   1) member_level_region_summary
#   2) twc_grid_classes  (for region metadata join at the end)
#
# Produces:
#   storyline_[1-8]_probability.Rds
#   all_storylines_probability.Rds
# ============================================================================

library(data.table)
source("source/twc_change.R")
source("source/storyline_specs.R")

# Helpers =====================================================================

classify_support <- function(p) {
  fcase(
    p < 0.05, "confident_not_happening",
    p < 0.20, "most_likely_not_happening",
    p < 0.40, "no_clear_signal",
    p < 0.60, "likely_happening",
    p < 0.80, "most_likely_happening",
    default  = "confident_happening"
  )
}

score_binary <- function(dt, spec) {
  dt[, primary_flag := eval(spec$primary)]
  for (nm in names(spec$sub_variants)) {
    dt[, (nm) := eval(spec$sub_variants[[nm]])]
  }
  dt
}

score_ordinal <- function(dt, spec) {
  dt <- score_binary(dt, spec)
  sub_cols <- names(spec$sub_variants)
  dt[, ordinal_score := as.integer(primary_flag) +
       rowSums(.SD == TRUE, na.rm = TRUE),
     .SDcols = sub_cols]
  dt
}

score_magnitude <- function(dt, spec) {
  dt <- score_binary(dt, spec)
  dt[, magnitude_weight := fifelse(
    primary_flag,
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
  
  sub_cols <- names(spec$sub_variants)
  
  sub_means <- setNames(
    lapply(sub_cols, function(v) {
      substitute(mean(V, na.rm = TRUE), list(V = as.name(v)))
    }),
    paste0("p_", sub_cols)
  )
  
  extra <- switch(method,
                  ordinal   = list(
                    mean_ordinal = quote(mean(ordinal_score, na.rm = TRUE))
                  ),
                  magnitude = list(
                    mean_magnitude = quote(mean(magnitude_weight, na.rm = TRUE)),
                    p_mk_sig       = quote(mean(primary_flag & prec_mk_sig, na.rm = TRUE))
                  ),
                  list()
  )
  
  all_exprs <- c(
    list(
      n_member = quote(as.integer(.N)),
      n_story  = quote(sum(primary_flag, na.rm = TRUE)),
      p_story  = quote(mean(primary_flag, na.rm = TRUE))
    ),
    sub_means,
    extra
  )
  
  dt[, eval(as.call(c(list(quote(list)), all_exprs))), by = region]
}

add_support_cols <- function(out) {
  p_cols <- grep("^p_", names(out), value = TRUE)
  out[, paste0("support_", p_cols) := lapply(.SD, classify_support),
      .SDcols = p_cols]
  out
}

assess_storyline <- function(member_features, spec, score_method = "binary") {
  
  missing_regions <- setdiff(spec$regions, unique(member_features$region))
  if (length(missing_regions) > 0) {
    warning(spec$label, ": regions not found in data — ",
            paste(missing_regions, collapse = ", "))
  }
  
  dt <- copy(member_features[region %in% spec$regions])
  dt <- add_score(dt, spec, method = score_method)
  
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
