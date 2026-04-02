# ============================================================================
# Summarise storyline likelihoods — one row per storyline
#
# Uses:
#   1) all_storylines_probability.Rds  (region-level p_story)
#   2) member_level_region_summary.Rds (raw member fluxes)
#   3) source/storyline_specs.R        (canonical region lists)
#
# Produces:
#   storyline_summary.Rds
# ============================================================================

library(data.table)
source("source/twc_change.R")
source("source/storyline_specs.R")

# Constants ===================================================================

CONSENSUS_THRESHOLD <- 0.60

# Expected sign matrix — from the storyline table (+1, -1, NA for ≈0/mixed) ==
# Columns: prec, evap, flux (=(P+E)/2), avail (=P-E)

SIGN_MATRIX <- data.table(
  storyline  = c(
    "storyline_1_arctic_boreal_amplification",
    "storyline_2_poleward_stormtrack_wetting",
    "storyline_3_monsoon_amplification",
    "storyline_4_humid_tropical_intensification",
    "storyline_5_subtropical_circulation_drying",
    "storyline_6_land_atm_coupling_amplification",
    "storyline_7_deforestation_induced_deceleration",
    "storyline_8_dryland_soilmoisture_collapse"
  ),
  sign_prec  = c( 1,  1,  1,  1, -1, -1, -1, -1),
  sign_evap  = c( 1,  1,  1,  1, NA, NA, -1, -1),  # ≈0 for 5,6
  sign_flux  = c( 1,  1,  1,  1, NA, NA, -1, -1),  # ≈0 for 5; mixed for 6
  sign_avail = c( 1,  1,  1, NA, -1, -1, NA, -1)   # ≈0 for 4; mixed for 7
)

# Inputs ======================================================================

storyline_probs <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "all_storylines_probability.Rds"))
)

member_features <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds"))
)

# rename to avoid collision with storyline column added later
setnames(member_features, "storyline", "regime_change")

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

# sign-aware agreement probability
# sign =  1 -> P(x > 0)
# sign = -1 -> P(x < 0)
# sign = NA -> NA (not assessed for this storyline)
p_agree <- function(x, sign) {
  if (is.na(sign)) return(NA_real_)
  if (sign ==  1L) return(mean(x > 0, na.rm = TRUE))
  if (sign == -1L) return(mean(x < 0, na.rm = TRUE))
  NA_real_
}

# Part 1: probability summary — from region-level results =====================

prob_summary <- storyline_probs[,
                                .(
                                  n_regions      = .N,
                                  median_p_story = median(p_story, na.rm = TRUE),
                                  p_consensus    = mean(p_story >= CONSENSUS_THRESHOLD, na.rm = TRUE)
                                ),
                                by = storyline
]

prob_summary[, dominant_support := classify_support(median_p_story)]

# Part 2: flux medians + sign-agreement — pooled across member x region =======

# canonical region lookup
region_lookup <- rbindlist(
  lapply(STORYLINE_SPECS, function(spec) {
    data.table(storyline = spec$label, region = spec$regions)
  })
)

# tag member rows with their storyline(s)
member_tagged <- merge(
  member_features,
  region_lookup,
  by       = "region",
  all      = FALSE,
  allow.cartesian = TRUE
)

# merge sign matrix so we can compute directional agreement per row
member_tagged <- merge(member_tagged, SIGN_MATRIX, by = "storyline")

flux_summary <- member_tagged[,
                              .(
                                # absolute change medians
                                median_prec_abs  = median(prec_abs_change,  na.rm = TRUE),
                                median_evap_abs  = median(evap_abs_change,  na.rm = TRUE),
                                median_flux_abs  = median(flux_abs_change,  na.rm = TRUE),
                                median_avail_abs = median(avail_abs_change, na.rm = TRUE),
                                
                                # relative change medians (using valid flags)
                                median_prec_rel  = median(prec_rel_change[prec_rel_valid],  na.rm = TRUE),
                                median_evap_rel  = median(evap_rel_change[evap_rel_valid],  na.rm = TRUE),
                                median_flux_rel  = median(flux_rel_change[flux_rel_valid],  na.rm = TRUE),
                                median_avail_rel = median(avail_rel_change[avail_rel_valid], na.rm = TRUE),
                                
                                # sign-agreement probabilities (direction from SIGN_MATRIX)
                                p_prec_agree  = p_agree(prec_abs_change,  sign_prec[1]),
                                p_evap_agree  = p_agree(evap_abs_change,  sign_evap[1]),
                                p_flux_agree  = p_agree(flux_abs_change,  sign_flux[1]),
                                p_avail_agree = p_agree(avail_abs_change, sign_avail[1])
                              ),
                              by = storyline
]

# Part 3: combine =============================================================

out <- merge(prob_summary, flux_summary, by = "storyline")

setcolorder(out, c(
  "storyline",
  "n_regions",
  "median_p_story",
  "p_consensus",
  "dominant_support",
  "p_prec_agree",  "median_prec_abs",  "median_prec_rel",
  "p_evap_agree",  "median_evap_abs",  "median_evap_rel",
  "p_flux_agree",  "median_flux_abs",  "median_flux_rel",
  "p_avail_agree", "median_avail_abs", "median_avail_rel"
))

setorder(out, storyline)

# Save ========================================================================

saveRDS(
  out,
  file.path(PATH_OUTPUT_DATA, "storyline_summary.Rds")
)
