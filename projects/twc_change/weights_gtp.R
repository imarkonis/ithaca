

dt <- copy(dataset_ranks)
setDT(dt)

# ============================================================
# 0) Required columns
# ============================================================
req_cols <- c(
  "lon", "lat", "dataset",
  "prec_mean_bias", "prec_sd_bias", "evap_mean_bias", "evap_sd_bias",
  "prec_check_significance", "prec_check_non_significance",
  "evap_check_significance", "evap_check_non_significance",
  "prec_bias_slope", "evap_bias_slope",
  "pe_ratio_check", "n_below_pet"
)

missing_cols <- setdiff(req_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing columns in dataset_ranks: ", paste(missing_cols, collapse = ", "))
}

# IPCC region
if (!"ipcc_short_region" %in% names(dt)) {
  dt[, ipcc_short_region := "GLOBAL"]
} else {
  dt[is.na(ipcc_short_region) | ipcc_short_region == "", ipcc_short_region := "UNKNOWN"]
}

# Biome (your workflow requires this)
# Set this to your actual column name if you have it (eg koppen, biome, landcover_biome)
biome_col <- "biome_short_class"

if (is.na(biome_col) || is.null(biome_col)) {
  dt[, biome := "GLOBAL"]
  biome_col <- "biome"
} else {
  setnames(dt, biome_col, "biome")
  biome_col <- "biome"
  dt[is.na(biome) | biome == "", biome := "UNKNOWN"]
}

# Area weights
dt[, area_w := cos(lat * pi / 180)]
dt[!is.finite(area_w), area_w := NA_real_]

# ============================================================
# 1) Helpers
# ============================================================
normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & !is.na(x) & (x > 0)
  if (!any(ok)) return(out)
  s <- sum(x[ok])
  if (!is.finite(s) || s <= 0) {
    out[ok] <- 1 / sum(ok)
  } else {
    out[ok] <- x[ok] / s
  }
  out
}

loss_to_prob <- function(loss, eps = 1e-6) {
  q <- rep(NA_real_, length(loss))
  ok <- is.finite(loss) & !is.na(loss) & (loss >= 0)
  q[ok] <- 1 / (eps + loss[ok])
  normalize_prob(q)
}

weighted_gmean <- function(..., w = NULL, eps = 1e-12) {
  m <- as.matrix(cbind(...))
  if (is.null(w)) w <- rep(1, ncol(m))
  stopifnot(length(w) == ncol(m))
  
  out <- rep(NA_real_, nrow(m))
  for (i in seq_len(nrow(m))) {
    xi <- m[i, ]
    ok <- is.finite(xi) & !is.na(xi) & xi > 0 & is.finite(w) & !is.na(w)
    if (!any(ok)) next
    wi <- w[ok] / sum(w[ok])
    out[i] <- exp(sum(wi * log(pmax(xi[ok], eps))))
  }
  out
}

weighted_hmean <- function(..., w = NULL, eps = 1e-12) {
  m <- as.matrix(cbind(...))
  if (is.null(w)) w <- rep(1, ncol(m))
  stopifnot(length(w) == ncol(m))
  
  out <- rep(NA_real_, nrow(m))
  for (i in seq_len(nrow(m))) {
    xi <- m[i, ]
    ok <- is.finite(xi) & !is.na(xi) & xi > 0 & is.finite(w) & !is.na(w)
    if (!any(ok)) next
    wi <- w[ok] / sum(w[ok])
    out[i] <- 1 / sum(wi / pmax(xi[ok], eps))
  }
  out
}
weighted_gmean <- function(..., w = NULL, eps = 1e-12) {
  m <- as.matrix(cbind(...))
  if (is.null(w)) w <- rep(1, ncol(m))
  stopifnot(length(w) == ncol(m))
  
  out <- rep(NA_real_, nrow(m))
  for (i in seq_len(nrow(m))) {
    xi <- m[i, ]
    ok <- is.finite(xi) & !is.na(xi) & xi > 0 & is.finite(w) & !is.na(w)
    if (!any(ok)) next
    wi <- w[ok] / sum(w[ok])
    out[i] <- exp(sum(wi * log(pmax(xi[ok], eps))))
  }
  out
}

weighted_hmean <- function(..., w = NULL, eps = 1e-12) {
  m <- as.matrix(cbind(...))
  if (is.null(w)) w <- rep(1, ncol(m))
  stopifnot(length(w) == ncol(m))
  
  out <- rep(NA_real_, nrow(m))
  for (i in seq_len(nrow(m))) {
    xi <- m[i, ]
    ok <- is.finite(xi) & !is.na(xi) & xi > 0 & is.finite(w) & !is.na(w)
    if (!any(ok)) next
    wi <- w[ok] / sum(w[ok])
    out[i] <- 1 / sum(wi / pmax(xi[ok], eps))
  }
  out
}
wmean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w = w[ok], na.rm = TRUE)
}

normalize_final <- function(x) {
  s <- sum(x, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) return(rep(NA_real_, length(x)))
  x / s
}

# ============================================================
# 12) Grid cell inspector for debugging
# ============================================================
inspect_grid_cell <- function(dt, lon0, lat0, digits = 4) {
  stopifnot(is.data.table(dt))
  if (!all(c("lon", "lat", "dataset") %in% names(dt))) stop("dt must contain lon, lat, dataset")
  
  x <- dt[lon == lon0 & lat == lat0]
  if (!nrow(x)) stop("No rows found for this lon lat")
  
  # core columns that usually matter for tracing a surprising choice
  cols_try <- c(
    # raw losses
    "prec_mean_bias", "prec_sd_bias", "evap_mean_bias", "evap_sd_bias",
    "prec_bias_slope", "evap_bias_slope",
    # prob layers
    "p_prec_mean", "p_prec_sd", "p_evap_mean", "p_evap_sd",
    "p_prec_slope", "p_evap_slope", "p_prec_sig", "p_evap_sig",
    "p_pe_ratio", "p_pet",
    # composites
    "p_P_clim", "p_E_clim", "p_P_trend", "p_E_trend", "p_E_phys",
    "p_P_internal", "p_E_internal", "p_internal_joint",
    # gates and totals
    "gate_pe", "gate_pet", "gate_phys",
    "score_joint_gated", "p_PE_external", "score_total", "score_total_raw",
    # final within cell prob
    "w_cell",
    # context
    "ipcc_short_region", "biome", "area_w", "n_below_pet",
    "pe_ratio_check", "prec_check_significance", "evap_check_significance"
  )
  cols <- intersect(cols_try, names(x))
  
  out <- x[, ..cols]
  num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  out[, (num_cols) := lapply(.SD, function(v) round(v, digits)), .SDcols = num_cols]
  
  setorder(out, -w_cell)
  out[, rank_in_cell := seq_len(.N)]
  
  # quick cell level summaries
  sums <- list(
    p_layers = lapply(intersect(c("p_prec_mean","p_prec_sd","p_evap_mean","p_evap_sd",
                                  "p_prec_slope","p_evap_slope","p_prec_sig","p_evap_sig",
                                  "p_pe_ratio","p_pet","w_cell"), names(x)),
                      function(cc) x[, sum(get(cc), na.rm = TRUE)]),
    gate_phys_range = if ("gate_phys" %in% names(x)) range(x$gate_phys, na.rm = TRUE) else NA
  )
  names(sums$p_layers) <- intersect(c("p_prec_mean","p_prec_sd","p_evap_mean","p_evap_sd",
                                      "p_prec_slope","p_evap_slope","p_prec_sig","p_evap_sig",
                                      "p_pe_ratio","p_pet","w_cell"), names(x))
  
  list(
    lon = lon0,
    lat = lat0,
    n_datasets = nrow(x),
    sum_checks = sums,
    table = out
  )
}

# ============================================================
# 2) Optional masking of slope bias
# ============================================================
dt[, prec_bias_slope_use := prec_bias_slope]
dt[, evap_bias_slope_use := evap_bias_slope]

dt[is.na(prec_check_significance), prec_bias_slope_use := NA_real_]
dt[is.na(evap_check_significance), evap_bias_slope_use := NA_real_]

dt[!is.finite(prec_bias_slope_use) | prec_bias_slope_use < 0, prec_bias_slope_use := NA_real_]
dt[!is.finite(evap_bias_slope_use) | evap_bias_slope_use < 0, evap_bias_slope_use := NA_real_]

# ============================================================
# 3) Grid level probabilities per metric (sum to 1 within lon lat)
# ============================================================
dt <- dt[pe_ratio_check == TRUE]
dt <- dt[n_below_pet > 0]

dt[, p_prec_mean := loss_to_prob(prec_mean_bias), by = .(lon, lat)]
dt[, p_prec_sd   := loss_to_prob(prec_sd_bias),   by = .(lon, lat)]
dt[, p_evap_mean := loss_to_prob(evap_mean_bias), by = .(lon, lat)]
dt[, p_evap_sd   := loss_to_prob(evap_sd_bias),   by = .(lon, lat)]
dt[, p_prec_slope := loss_to_prob(prec_bias_slope), by = .(lon, lat)]
dt[, p_evap_slope := loss_to_prob(evap_bias_slope), by = .(lon, lat)]

dt[, p_prec_sig := {
  raw <- fcoalesce(
    fifelse(!is.na(prec_check_significance),
            fifelse(prec_check_significance, 1.0, 0.10), NA_real_),
    fifelse(!is.na(prec_check_non_significance),
            fifelse(prec_check_non_significance, 1.0, 0.10), NA_real_)
  )
  normalize_prob(raw)
}, by = .(lon, lat)]

dt[, p_evap_sig := {
  raw <- fcoalesce(
    fifelse(!is.na(evap_check_significance),
            fifelse(evap_check_significance, 1.0, 0.10), NA_real_),
    fifelse(!is.na(evap_check_non_significance),
            fifelse(evap_check_non_significance, 1.0, 0.10), NA_real_)
  )
  normalize_prob(raw)
}, by = .(lon, lat)]


# Physics checks at metric level

dt[, p_pet := {
  raw <- fifelse(is.na(n_below_pet), NA_real_, pmax(n_below_pet, 0))
  normalize_prob(raw)
}, by = .(lon, lat)]

# ============================================================
# 4) Domain scores, variable scores
# ============================================================
dt[, p_P_clim := weighted_row_mean(p_prec_mean, p_prec_sd, w = c(0.5, 0.5))]
dt[, p_E_clim := weighted_row_mean(p_evap_mean, p_evap_sd, w = c(0.5, 0.5))]

dt[, p_P_trend := weighted_row_mean(p_prec_sig, p_prec_slope, w = c(0.7, 0.3))]
dt[, p_E_trend := weighted_row_mean(p_evap_sig, p_evap_slope, w = c(0.7, 0.3))]

dt[, p_E_phys := p_pet]

dt[, P_internal_score := weighted_gmean(p_P_clim, p_P_trend, w = c(0.3, 0.7))]
dt[, E_internal_score := weighted_gmean(p_E_clim, p_E_trend, w = c(0.3, 0.7))]
dt[, internal_joint_score := weighted_hmean(
  P_internal_score, E_internal_score, p_E_phys,
  w = c(0.35, 0.45, 0.30)
)]

dt[, p_P_internal_prob := normalize_prob(pmax(P_internal_score, 0)), by = .(lon, lat)]
dt[, p_E_internal_prob := normalize_prob(pmax(E_internal_score, 0)), by = .(lon, lat)]
dt[, p_internal_joint := normalize_prob(p_internal_joint_score), by = .(lon, lat)]

dt[lon == 9.875 & lat == 35.125] #still not realistic - weight tuning decision affects a lot the outcome
dt[lon == 69.875 & lat == 35.125] #bias too big in evap_bias_slope, still high likelihood in joint_score 



###### START HERE



















# ============================================================
# 5) Physics sanity checks as gates or strong penalties
# ============================================================
# You can choose strict gating or soft penalties.
# Here is a strong penalty approach:
#   gate = 1 if passes, penalty if fails, NA means exclude from penalty (neutral)
#
# AET <= PET check already in pe_ratio_check, but we apply it as a multiplier gate
# PET consistency count n_below_pet also used as a multiplier
#
# Tuning knobs
pen_fail_ratio <- 0.05     # very strong penalty if pe_ratio_check fails
pen_fail_pet   <- 0.20     # penalty when n_below_pet is low relative to typical
pet_scale_eps  <- 1e-6

dt[, gate_pe := fifelse(is.na(pe_ratio_check), 1.0,
                        fifelse(pe_ratio_check, 1.0, pen_fail_ratio))]

# convert n_below_pet to a 0 to 1 factor within cell, then scale to penalty range
dt[, gate_pet := {
  x <- n_below_pet
  ok <- is.finite(x) & !is.na(x)
  if (!any(ok)) {
    rep(1.0, .N)
  } else {
    # within cell min max to 0..1
    xx <- x
    mn <- min(xx[ok])
    mx <- max(xx[ok])
    g <- rep(1.0, .N)
    if (is.finite(mn) && is.finite(mx) && mx > mn) {
      z <- (xx - mn) / (mx - mn + pet_scale_eps)
      z[!ok] <- NA_real_
      # map z to [pen_fail_pet, 1]
      g <- fifelse(is.na(z), 1.0, pen_fail_pet + (1 - pen_fail_pet) * z)
    }
    g
  }
}, by = .(lon, lat)]

dt[, gate_phys := gate_pe * gate_pet]

# apply gate to internal joint score before any sharpening
dt[, score_joint_gated := p_internal_joint * gate_phys]

# ============================================================
# 6) Optional external support hooks (P minus E consistency)
# ============================================================
# If you have external columns, they get folded in here.
# Supported patterns:
#   pe_check_significance or pe_check_non_significance (logical)
#   pe_bias_slope (nonnegative)
#   pe_mean_bias, pe_sd_bias (nonnegative)
#
# If none exist, external term is neutral.

has_any_external <- any(c(
  "pe_check_significance", "pe_check_non_significance",
  "pe_bias_slope", "pe_mean_bias", "pe_sd_bias"
) %in% names(dt))

if (has_any_external) {
  
  if (!"pe_bias_slope" %in% names(dt)) dt[, pe_bias_slope := NA_real_]
  if (!"pe_mean_bias" %in% names(dt))  dt[, pe_mean_bias := NA_real_]
  if (!"pe_sd_bias" %in% names(dt))    dt[, pe_sd_bias := NA_real_]
  if (!"pe_check_significance" %in% names(dt))      dt[, pe_check_significance := NA]
  if (!"pe_check_non_significance" %in% names(dt))  dt[, pe_check_non_significance := NA]
  
  dt[, pe_bias_slope_use := pe_bias_slope]
  dt[!is.finite(pe_bias_slope_use) | pe_bias_slope_use < 0, pe_bias_slope_use := NA_real_]
  
  dt[, p_pe_mean := loss_to_prob(pe_mean_bias), by = .(lon, lat)]
  dt[, p_pe_sd   := loss_to_prob(pe_sd_bias),   by = .(lon, lat)]
  dt[, p_pe_slope := loss_to_prob(pe_bias_slope_use), by = .(lon, lat)]
  
  dt[, p_pe_sig := {
    raw <- fcoalesce(
      fifelse(!is.na(pe_check_significance),
              fifelse(pe_check_significance, 1.0, 0.30), NA_real_),
      fifelse(!is.na(pe_check_non_significance),
              fifelse(pe_check_non_significance, 1.0, 0.30), NA_real_)
    )
    normalize_prob(raw)
  }, by = .(lon, lat)]
  
  dt[, p_PE_external := weighted_row_mean(
    weighted_row_mean(p_pe_mean, p_pe_sd, w = c(0.5, 0.5)),
    weighted_row_mean(p_pe_sig, p_pe_slope, w = c(0.6, 0.4)),
    w = c(0.5, 0.5)
  )]
  
  # mix internal and external (external is about change consistency in P minus E)
  dt[, score_total := weighted_row_mean(score_joint_gated, p_PE_external, w = c(0.7, 0.3))]
  
} else {
  dt[, p_PE_external := NA_real_]
  dt[, score_total := score_joint_gated]
}

# ============================================================
# 7) Final per grid cell dataset probability (this is the key output of step 1)
# ============================================================
gamma <- 1.5
dt[, score_total_raw := score_total^gamma]

dt[, w_cell := {
  raw <- score_total_raw
  normalize_prob(raw)
}, by = .(lon, lat)]

# ============================================================
# 8) Aggregate grid to biome within IPCC region
# ============================================================
# w_{region, biome}(dataset) with sum to 1 per region biome
dataset_region_biome <- dt[
  ,
  .(
    n_cells = .N,
    area_sum = sum(area_w, na.rm = TRUE),
    w_rb_raw = wmean_safe(w_cell, area_w)
  ),
  by = .(ipcc_short_region, biome, dataset)
]

dataset_region_biome[, w_region_biome := normalize_final(w_rb_raw), by = .(ipcc_short_region, biome)]
dataset_region_biome[, w_rb_raw := NULL]

# biome area fractions within each IPCC region
biome_area <- dt[
  ,
  .(biome_area = sum(area_w, na.rm = TRUE)),
  by = .(ipcc_short_region, biome)
]
biome_area[, A_region_biome := normalize_final(biome_area), by = ipcc_short_region]
biome_area[, biome_area := NULL]

# ============================================================
# 9) Monte Carlo sampling of dataset worlds
# ============================================================
# This produces sampled datasets per region biome per simulation and region level mixture weights.
#
# To compute P and E regional values per simulation you will need per grid cell P and E fields.
# This sampling scaffold is still useful now: it generates the dataset choice uncertainty structure.

sample_dataset_worlds <- function(w_rb, A_rb, n_sims = 2000, seed = 1L) {
  set.seed(seed)
  
  # build a quick lookup list: for each region biome, datasets and probs
  key <- w_rb[, .(dataset, prob = w_region_biome), by = .(ipcc_short_region, biome)]
  key <- key[is.finite(prob) & !is.na(prob) & prob > 0]
  
  rb <- unique(key[, .(ipcc_short_region, biome)])
  if (nrow(rb) == 0) stop("No region biome probabilities available for sampling")
  
  # sample one dataset per region biome per sim
  out <- rbindlist(lapply(seq_len(n_sims), function(s) {
    sel <- key[
      ,
      .(dataset = sample(dataset, size = 1, prob = prob)),
      by = .(ipcc_short_region, biome)
    ]
    sel[, sim := s]
    sel
  }))
  
  # attach biome area fractions
  out <- merge(out, A_rb[, .(ipcc_short_region, biome, A_region_biome)],
               by = c("ipcc_short_region", "biome"), all.x = TRUE)
  
  out[]
}

mc_choices <- sample_dataset_worlds(dataset_region_biome, biome_area, n_sims = 2000, seed = 42)

# ============================================================
# 10) Optional region level summary weight (expected mixture)
# ============================================================
# This is NOT the Monte Carlo, it is the deterministic expected weight:
# sum over biomes of A_region_biome * w_region_biome(dataset)
expected_region_weights <- merge(
  dataset_region_biome,
  biome_area,
  by = c("ipcc_short_region", "biome"),
  all.x = TRUE
)

expected_region_weights[, w_region_expected := w_region_biome * A_region_biome]
expected_region_weights <- expected_region_weights[
  ,
  .(w_region_expected = sum(w_region_expected, na.rm = TRUE)),
  by = .(ipcc_short_region, dataset)
]
expected_region_weights[, w_region_expected := normalize_final(w_region_expected), by = ipcc_short_region]

# global expected weight (area across regions)
region_area <- dt[, .(region_area = sum(area_w, na.rm = TRUE)), by = ipcc_short_region]
region_area[, region_w := normalize_final(region_area)]
expected_global <- merge(expected_region_weights, region_area[, .(ipcc_short_region, region_w)],
                         by = "ipcc_short_region", all.x = TRUE)
expected_global <- expected_global[
  ,
  .(w_global_expected = sum(w_region_expected * region_w, na.rm = TRUE)),
  by = dataset
]
expected_global[, w_global_expected := normalize_final(w_global_expected)]
setorder(expected_global, -w_global_expected)

# ============================================================
# Outputs
# ============================================================
cat("\nExpected global mixture weights\n")
print(expected_global)

cat("\nExample Monte Carlo draws (first 10 rows)\n")
print(mc_choices[1:10])

cat("\nExpected weights within selected regions (example)\n")
print(expected_region_weights[ipcc_short_region %in% c("MED", "WCE", "NEU")][order(ipcc_short_region, -w_region_expected)])


# input: dataset_region_biome with cols:
# ipcc_short_region, biome, dataset, w_region_biome_pe

make_prob_wide <- function(dataset_region_biome,
                           prob_col = "w_region_biome_pe_minus",
                           region = "MED") {
  x <- dataset_region_biome[ipcc_short_region == region,
                            .(biome, dataset, prob = get(prob_col))]
  dcast(x, biome ~ dataset, value.var = "prob", fill = 0)
}
make_prob_wide(dataset_region_biome, "w_region_biome", "MED")

make_prob_tidy <- function(dataset_region_biome,
                           prob_col = "w_region_biome_pe_minus") {
  dataset_region_biome[, .(
    ipcc_short_region,
    biome,
    dataset,
    prob = get(prob_col)
  )][is.finite(prob) & !is.na(prob)]
}

make_prob_tidy(dataset_region_biome, "w_region_biome")


make_area_wide <- function(biome_area, region = "MED") {
  x <- biome_area[ipcc_short_region == region, .(biome, A_region_biome)]
  dcast(x, . ~ biome, value.var = "A_region_biome", fill = 0)
}

make_area_wide(biome_area, "MED")
biome_area[, .(ipcc_short_region, biome, A_region_biome)]


make_mc_wide <- function(mc_choices, region = "MED", n_show = 10) {
  x <- mc_choices[ipcc_short_region == region & sim <= n_show,
                  .(biome, sim, dataset)]
  x[, sim := paste0("Ens_", sim)]
  dcast(x, biome ~ sim, value.var = "dataset")
}

make_mc_wide(mc_choices, "MED", n_show = 10)
mc_choices[, .(ipcc_short_region, biome, sim, dataset)]
mc_tidy

expected_region_tidy <- function(dataset_region_biome, biome_area,
                                 prob_col = "w_region_biome_pe_minus") {
  x <- dataset_region_biome[, .(ipcc_short_region, biome, dataset, w = get(prob_col))]
  x <- merge(x, biome_area[, .(ipcc_short_region, biome, A_region_biome)],
             by = c("ipcc_short_region", "biome"), all.x = TRUE)
  
  out <- x[, .(w_region_expected = sum(w * A_region_biome, na.rm = TRUE)),
           by = .(ipcc_short_region, dataset)]
  
  out[, w_region_expected := w_region_expected / sum(w_region_expected, na.rm = TRUE),
      by = ipcc_short_region]
  
  out[]
}

expected_region_tidy(dataset_region_biome, biome_area, "w_region_biome")



