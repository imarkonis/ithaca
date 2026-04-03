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

source("source/twc_change.R")
source("source/storyline_specs.R")

# Constants ===================================================================

CONSENSUS_THRESHOLD <- 0.375

# Expected sign matrix — from the storyline table (+1, -1, NA for ≈0/mixed) ==
# Columns: prec, evap, flux (=(P+E)/2), avail (=P-E)

SIGN_MATRIX <- data.table(
  storyline  = c(
    "1_arctic_boreal_amplification",
    "2_poleward_stormtrack_wetting",
    "3_monsoon_amplification",
    "4_humid_tropical_intensification",
    "5_subtropical_circulation_drying",
    "6_land_atm_coupling_amplification",
    "7_deforestation_induced_deceleration",
    "8_dryland_soilmoisture_collapse"
  ),
  sign_prec  = c( 1,  1,  1,  1, -1, -1, -1, -1),
  sign_evap  = c( 1,  1,  1,  1, NA, NA, -1, -1),
  sign_flux  = c( 1,  1,  1,  1, NA, NA, -1, -1),
  sign_avail = c( 1,  1,  1, NA, -1, -1, NA, -1)
)

# Inputs ======================================================================

storyline_probs <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "all_storylines_probability.Rds"))
)

member_features <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds"))
)

setnames(member_features, "storyline", "regime_change")

# Helpers =====================================================================

# Compound criteria classifier — thresholds scaled relative to joint null
classify_support_compound <- function(p) {
  fcase(
    p < 0.05, "confident_not_happening",
    p < 0.15, "likely_not_happening",
    p < 0.375, "no_clear_signal",
    p < 0.75, "likely_happening",
    default  = "confident_happening"
  )
}

# Single-variable classifier — symmetric around 0.5 null
classify_support_single <- function(p) {
  fcase(
    p < 0.10, "confident_not_happening",
    p < 0.33, "likely_not_happening",
    p < 0.67, "no_clear_signal",
    p < 0.90, "likely_happening",
    default  = "confident_happening"
  )
}

# Sign-aware agreement probability for a single variable
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

# compound classifier for the joint storyline probability
prob_summary[, dominant_support := classify_support_compound(median_p_story)]

# Part 2: flux medians + sign-agreement — pooled across member x region =======

region_lookup <- rbindlist(
  lapply(STORYLINE_SPECS, function(spec) {
    data.table(storyline = spec$label, region = spec$regions)
  })
)

member_tagged <- merge(
  member_features,
  region_lookup,
  by = "region", all = FALSE, allow.cartesian = TRUE
)

member_tagged <- merge(member_tagged, SIGN_MATRIX, by = "storyline")

flux_summary <- member_tagged[,
                              .(
                                # absolute change medians
                                median_prec_abs  = median(prec_abs_change,  na.rm = TRUE),
                                median_evap_abs  = median(evap_abs_change,  na.rm = TRUE),
                                median_flux_abs  = median(flux_abs_change,  na.rm = TRUE),
                                median_avail_abs = median(avail_abs_change, na.rm = TRUE),
                                
                                # relative change medians
                                median_prec_rel  = median(prec_rel_change[prec_rel_valid],   na.rm = TRUE),
                                median_evap_rel  = median(evap_rel_change[evap_rel_valid],   na.rm = TRUE),
                                median_flux_rel  = median(flux_rel_change[flux_rel_valid],   na.rm = TRUE),
                                median_avail_rel = median(avail_rel_change[avail_rel_valid], na.rm = TRUE),
                                
                                # sign-agreement probabilities (single-variable, use single null)
                                p_prec_agree  = p_agree(prec_abs_change,  sign_prec[1]),
                                p_evap_agree  = p_agree(evap_abs_change,  sign_evap[1]),
                                p_flux_agree  = p_agree(flux_abs_change,  sign_flux[1]),
                                p_avail_agree = p_agree(avail_abs_change, sign_avail[1])
                              ),
                              by = storyline
]

# Single-variable support labels for each flux direction
flux_summary[,
             `:=`(
               support_prec  = classify_support_single(p_prec_agree),
               support_evap  = classify_support_single(p_evap_agree),
               support_flux  = classify_support_single(p_flux_agree),
               support_avail = classify_support_single(p_avail_agree)
             )
]

# Part 3: combine =============================================================

out <- merge(prob_summary, flux_summary, by = "storyline")

setcolorder(out, c(
  "storyline",
  "n_regions",
  "median_p_story",
  "p_consensus",
  "dominant_support",
  "p_prec_agree",  "support_prec",  "median_prec_abs",  "median_prec_rel",
  "p_evap_agree",  "support_evap",  "median_evap_abs",  "median_evap_rel",
  "p_flux_agree",  "support_flux",  "median_flux_abs",  "median_flux_rel",
  "p_avail_agree", "support_avail", "median_avail_abs", "median_avail_rel"
))

setorder(out, storyline)

# Save ========================================================================

saveRDS(
  out,
  file.path(PATH_OUTPUT_DATA, "storyline_summary.Rds")
)
