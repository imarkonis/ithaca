# ============================================================================
# Storyline 3 probability
# Subtropical drying / aridification
#
# Uses:
#   1) member_level_region_summary
#   2) twc_grid_classes
#
# Produces:
#   storyline_3_probability.Rds
# ============================================================================

# Libraries ===================================================================

library(data.table)

source("source/twc_change.R")

# Inputs ======================================================================

member_level_region_summary <- readRDS(
  file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Helpers =====================================================================

classify_support <- function(p) {
  fcase(
    p < 0.05, "confident_not_happening",
    p < 0.20, "most_likely_not_happening",
    p < 0.40, "no_clear_signal",
    p < 0.60, "likely_happening",
    p < 0.80, "most_likely_happening",
    default = "confident_happening"
  )
}

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

# Region metadata =============================================================

region_meta <- as.data.table(twc_grid_classes)[
  ,
  .(
    region_full  = first_non_na(region_full),
    circulation  = first_non_na(circulation),
    lat_zone     = first_non_na(lat_zone),
    climate_main = first_non_na(climate_main),
    hydrobelt    = first_non_na(hydrobelt)
  ),
  by = region
]

# Member features ==============================================================

member_features <- merge(
  as.data.table(member_level_region_summary),
  region_meta,
  by = "region",
  all.x = TRUE
)

member_features[
  ,
  `:=`(
    dP = prec_abs_change,
    dE = evap_abs_change,
    dAvail = avail_abs_change,
    is_dry_subsidence = circulation == "dry_subsidence"
  )
]

# Probability summary ==========================================================

out <- member_features[
  ,
  .(
    n_member = .N,
    n_story = sum(dP < 0 & dAvail < 0, na.rm = TRUE),
    p_story = mean(dP < 0 & dAvail < 0, na.rm = TRUE),
    p_hotter_drier = mean(dP < 0 & dAvail < 0 & dE > 0, na.rm = TRUE),
    p_moisture_limited_drying = mean(dP < 0 & dAvail < 0 & dE < 0, na.rm = TRUE),
    p_dry_subsidence_context = mean(is_dry_subsidence, na.rm = TRUE)
  ),
  by = .(region, region_full, circulation, lat_zone, climate_main, hydrobelt)
]

out[
  ,
  `:=`(
    support = classify_support(p_story),
    support_hotter_drier = classify_support(p_hotter_drier),
    support_moisture_limited = classify_support(p_moisture_limited_drying),
    storyline = "storyline_3_subtropical_drying_aridification"
  )
]

# Save ========================================================================

saveRDS(
  out,
  file.path(PATH_OUTPUT_DATA, "storyline_3_probability.Rds")
)