source('source/twc_change.R')
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))
masks <- pRecipe::pRecipe_masks()

dataset_ranks <- merge(dataset_ranks, masks[land_mask == 'land', .(lon, lat, ipcc_short_region, biome_short_class)], 
                       by = c('lon', 'lat')) 

dt <- copy(dataset_ranks)
setDT(dt)

LAMBDA <- 4
SIG_FAIL <- 0.3

# -----------------------------
# Helper functions
# -----------------------------

# Binary/ternary check score
# TRUE = 1, FALSE = fail_score, NA = NA (not applicable / not evaluated)
score_check <- function(x, fail_score = 0.25) {
  out <- fifelse(is.na(x), NA_real_, fifelse(x, 1, fail_score))
  as.numeric(out)
}

# -----------------------------
# 1) Component scores
# -----------------------------

# Climatology (P, E)
dt[, s_prec_mean := score_bias(prec_mean_bias, lambda = LAMBDA)]
dt[, s_prec_sd   := score_bias(prec_sd_bias, lambda = LAMBDA)]
dt[, s_evap_mean := score_bias(evap_mean_bias, lambda = LAMBDA)]
dt[, s_evap_sd   := score_bias(evap_sd_bias, lambda = LAMBDA)]

# Significance consistency
dt[, s_prec_sigcheck := weighted_row_mean(
  score_check(prec_check_significance, fail_score = SIG_FAIL),
  score_check(prec_check_non_significance, fail_score = SIG_FAIL)
)]

dt[, s_evap_sigcheck := weighted_row_mean(
  score_check(evap_check_significance, fail_score = SIG_FAIL),
  score_check(evap_check_non_significance, fail_score = SIG_FAIL)
)]

# Slope-bias scores (assumes slope bias already masked to meaningful cells upstream)
dt[, s_prec_slopebias := score_bias(prec_bias_slope, lambda = LAMBDA)]
dt[, s_evap_slopebias := score_bias(evap_bias_slope, lambda = LAMBDA)]

# P/E physical plausibility and PET consistency
dt[, s_pe_ratio := score_check(pe_ratio_check, fail_score = 0)]




###################################### START HERE







# -----------------------------
# 2) Domain scores
# -----------------------------
dt[, score_P_clim := weighted_row_mean(s_prec_mean, s_prec_sd, w = c(0.5, 0.5))]
dt[, score_E_clim := weighted_row_mean(s_evap_mean, s_evap_sd, w = c(0.5, 0.5))]

dt[, score_P_trend := weighted_row_mean(s_prec_sigcheck, s_prec_slopebias, w = c(0.6, 0.4))]
dt[, score_E_trend := weighted_row_mean(s_evap_sigcheck, s_evap_slopebias, w = c(0.6, 0.4))]

dt[, score_E_phys := weighted_row_mean(s_pe_ratio, s_pet_consistency, w = c(0.45, 0.55))]

# Variable-level internal scores
dt[, score_P_internal := weighted_row_mean(score_P_clim, score_P_trend, w = c(0.5, 0.5))]
dt[, score_E_internal := weighted_row_mean(score_E_clim, score_E_trend, score_E_phys, w = c(0.35, 0.35, 0.30))]

# Target-specific internal scores
dt[, score_internal_pe_minus := weighted_row_mean(score_P_internal, score_E_internal, w = c(0.45, 0.55))]
dt[, score_internal_pe_plus  := weighted_row_mean(score_P_internal, score_E_internal, w = c(0.50, 0.50))]

# Contrast sharpening
gamma <- 2
dt[, w_internal_pe_minus_raw := score_internal_pe_minus^gamma]
dt[, w_internal_pe_plus_raw  := score_internal_pe_plus^gamma]

# -----------------------------
# 3) Dataset x Region summaries
# -----------------------------
dataset_region_internal <- dt[
  ,
  .(
    n_cells = .N,
    area_sum = sum(area_w, na.rm = TRUE),
    
    cov_P_internal = mean(!is.na(score_P_internal)),
    cov_E_internal = mean(!is.na(score_E_internal)),
    cov_pe_minus   = mean(!is.na(score_internal_pe_minus)),
    cov_pe_plus    = mean(!is.na(score_internal_pe_plus)),
    
    score_P_internal        = wmean_safe(score_P_internal, area_w),
    score_E_internal        = wmean_safe(score_E_internal, area_w),
    score_internal_pe_minus = wmean_safe(score_internal_pe_minus, area_w),
    score_internal_pe_plus  = wmean_safe(score_internal_pe_plus, area_w),
    
    w_internal_pe_minus_raw = wmean_safe(w_internal_pe_minus_raw, area_w),
    w_internal_pe_plus_raw  = wmean_safe(w_internal_pe_plus_raw, area_w)
  ),
  by = .(dataset, ipcc_short_region)
]

# Normalize within each region (optional, useful to see who "wins" regionally)
dataset_region_internal[, w_region_pe_minus := normalize(w_internal_pe_minus_raw), by = ipcc_short_region]
dataset_region_internal[, w_region_pe_plus  := normalize(w_internal_pe_plus_raw),  by = ipcc_short_region]

# -----------------------------
# 4) Region weights for global aggregation
# -----------------------------
# Baseline: area-proportional region weights from your grid
region_weights <- dt[
  ,
  .(region_area = sum(area_w, na.rm = TRUE)),
  by = ipcc_short_region
]
region_weights[, region_w_area := normalize(region_area)]

# ---- Optional target-specific reweighting ----
# Example: emphasize Mediterranean for P-E (customize as you like)
region_weights[, region_w_pe_minus := region_w_area]
region_weights[, region_w_pe_plus  := region_w_area]

# Example manual boost (illustrative)
# region_weights[ipcc_short_region == "MED", region_w_pe_minus := region_w_pe_minus * 2]
# region_weights[ipcc_short_region %in% c("WCE","NEU"), region_w_pe_minus := region_w_pe_minus * 1.2]

# Renormalize after any manual edits
region_weights[, region_w_pe_minus := normalize(region_w_pe_minus)]
region_weights[, region_w_pe_plus  := normalize(region_w_pe_plus)]

# -----------------------------
# 5) Aggregate region-stratified scores to final dataset weights
# -----------------------------
tmp <- merge(
  dataset_region_internal,
  region_weights[, .(ipcc_short_region, region_w_pe_minus, region_w_pe_plus)],
  by = "ipcc_short_region",
  all.x = TRUE
)

dataset_internal_regionaware <- tmp[
  ,
  .(
    n_regions = sum(!is.na(score_internal_pe_minus)),
    # region-aware global scores
    score_internal_pe_minus = wmean_safe(score_internal_pe_minus, region_w_pe_minus),
    score_internal_pe_plus  = wmean_safe(score_internal_pe_plus,  region_w_pe_plus),
    
    # aggregate raw weights from region-level scores
    w_internal_pe_minus_raw = wmean_safe(w_internal_pe_minus_raw, region_w_pe_minus),
    w_internal_pe_plus_raw  = wmean_safe(w_internal_pe_plus_raw,  region_w_pe_plus)
  ),
  by = .(dataset, ipcc_short_region)
]

# final normalized dataset weights
dataset_internal_regionaware[, w_internal_pe_minus := normalize(w_internal_pe_minus_raw)]
dataset_internal_regionaware[, w_internal_pe_plus  := normalize(w_internal_pe_plus_raw)]

setorder(dataset_internal_regionaware, -w_internal_pe_minus)

print(dataset_internal_regionaware)