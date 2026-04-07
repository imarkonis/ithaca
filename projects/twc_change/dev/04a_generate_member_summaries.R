# ============================================================================
# Summarize Monte Carlo ensemble and scenario-level TWC storylines
#
# This script:
# 1. Builds member-level summaries for Monte Carlo ensemble members
# 2. Aggregates MC members into scenario summaries
# 3. Saves global and regional summary products
#
# Diagnostics include:
#   - period means
#   - absolute and relative change
#   - Sen slope
#   - Mann-Kendall p-value
#   - storyline class, scenario quantiles/probabilities, storyline entropy
# ============================================================================

library(trend)
source("source/twc_change.R")

# Inputs ======================================================================

ensemble_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_yearly_prec_evap.Rds")
)
ensemble_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_yearly_prec_evap.Rds")
)

# Constants ===================================================================

YEAR_SPLIT   <- 2002L
ALPHA_SIG    <- 0.05
EPS_PREC     <- 1
EPS_EVAP     <- 1
EPS_FLUX     <- 1
EPS_AVAIL    <- 5

STORYLINE_LEVELS <- c(
  "wetter-accelerated",
  "drier-accelerated",
  "wetter-decelerated",
  "drier-decelerated"
)

# Helpers =====================================================================

add_metrics_period <- function(dt) {
  dt <- copy(as.data.table(dt))
  dt[, `:=`(
    avail  = prec - evap,
    flux   = (prec + evap) / 2,
    period = fifelse(year < YEAR_SPLIT, "1982_2001", "2002_2021")
  )]
  dt[]
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else as.numeric(quantile(x, probs = p, names = FALSE))
}

safe_prob <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else mean(x)
}

safe_rel_change <- function(v2, v1, eps) {
  if (any(!is.finite(c(v1, v2))) || abs(v1) <= eps) NA_real_
  else 100 * (v2 - v1) / abs(v1)
}

safe_sen <- function(y) {
  y <- as.numeric(y[is.finite(y)])
  if (length(y) < 3) return(NA_real_)
  out <- try(trend::sens.slope(y), silent = TRUE)
  if (inherits(out, "try-error")) NA_real_ else as.numeric(out$estimates)
}

safe_mk_p <- function(y) {
  y <- as.numeric(y[is.finite(y)])
  if (length(y) < 3) return(NA_real_)
  out <- try(trend::mk.test(y), silent = TRUE)
  if (inherits(out, "try-error")) NA_real_ else as.numeric(out$p.value)
}

classify_storyline <- function(avail_change, flux_change) {
  if (!is.finite(avail_change) || !is.finite(flux_change)) return(NA_character_)
  if (avail_change > 0 && flux_change > 0) return("wetter-accelerated")
  if (avail_change < 0 && flux_change > 0) return("drier-accelerated")
  if (avail_change > 0 && flux_change < 0) return("wetter-decelerated")
  if (avail_change < 0 && flux_change < 0) return("drier-decelerated")
  "neutral"
}

# Core summary for a single time series ---------------------------------------

summarize_one_series <- function(dt_one) {
  dt_one <- add_metrics_period(copy(as.data.table(dt_one)))
  setorder(dt_one, year)
  
  p1 <- dt_one[period == "1982_2001"]
  p2 <- dt_one[period == "2002_2021"]
  
  vars <- c("prec", "evap", "avail", "flux")
  eps  <- c(EPS_PREC, EPS_EVAP, EPS_AVAIL, EPS_FLUX)
  
  means1 <- setNames(sapply(vars, \(v) safe_mean(p1[[v]])), paste0(vars, "_1982_2001"))
  means2 <- setNames(sapply(vars, \(v) safe_mean(p2[[v]])), paste0(vars, "_2002_2021"))
  
  abs_chg <- means2 - means1
  rel_chg <- setNames(
    mapply(safe_rel_change, means2, means1, eps),
    paste0(vars, "_rel_change")
  )
  
  sen <- setNames(sapply(vars, \(v) safe_sen(dt_one[[v]])), paste0(vars, "_sen"))
  mk  <- setNames(sapply(vars, \(v) safe_mk_p(dt_one[[v]])), paste0(vars, "_mk_p"))
  
  names(abs_chg) <- paste0(vars, "_abs_change")
  
  out <- as.data.table(c(
    as.list(means1), as.list(means2),
    as.list(abs_chg), as.list(rel_chg),
    as.list(sen), as.list(mk)
  ))
  
  out[, storyline := classify_storyline(avail_abs_change, flux_abs_change)]
  out[]
}

# Build member-level summaries ------------------------------------------------

build_member_summary <- function(dt, id_cols) {
  dt   <- as.data.table(dt)
  keys <- dt[, do.call(paste, c(.SD, sep = "___")), .SDcols = id_cols]
  
  rbindlist(
    lapply(split(seq_len(nrow(dt)), keys), function(ii) {
      dt_one <- dt[ii]
      cbind(unique(dt_one[, ..id_cols]), summarize_one_series(dt_one))
    }),
    fill = TRUE, use.names = TRUE
  )[]
}

# Aggregate MC members into scenario summaries --------------------------------

summarize_scenarios <- function(dt_member, group_cols) {
  dt_member <- as.data.table(dt_member)
  
  dt_member[, {
    p_story <- c(
      safe_prob(storyline == STORYLINE_LEVELS[1]),
      safe_prob(storyline == STORYLINE_LEVELS[2]),
      safe_prob(storyline == STORYLINE_LEVELS[3]),
      safe_prob(storyline == STORYLINE_LEVELS[4])
    )
    
    modal_idx <- if (all(is.na(p_story))) NA_integer_
    else which.max(replace(p_story, is.na(p_story), -Inf))
    
    .(
      n_member = .N,
      
      prec_abs_q05  = safe_quantile(prec_abs_change,  0.05),
      prec_abs_q50  = safe_quantile(prec_abs_change,  0.50),
      prec_abs_q95  = safe_quantile(prec_abs_change,  0.95),
      
      evap_abs_q05  = safe_quantile(evap_abs_change,  0.05),
      evap_abs_q50  = safe_quantile(evap_abs_change,  0.50),
      evap_abs_q95  = safe_quantile(evap_abs_change,  0.95),
      
      avail_abs_q05 = safe_quantile(avail_abs_change, 0.05),
      avail_abs_q50 = safe_quantile(avail_abs_change, 0.50),
      avail_abs_q95 = safe_quantile(avail_abs_change, 0.95),
      
      flux_abs_q05  = safe_quantile(flux_abs_change,  0.05),
      flux_abs_q50  = safe_quantile(flux_abs_change,  0.50),
      flux_abs_q95  = safe_quantile(flux_abs_change,  0.95),
      
      prec_rel_q05  = safe_quantile(prec_rel_change,  0.05),
      prec_rel_q50  = safe_quantile(prec_rel_change,  0.50),
      prec_rel_q95  = safe_quantile(prec_rel_change,  0.95),
      
      evap_rel_q05  = safe_quantile(evap_rel_change,  0.05),
      evap_rel_q50  = safe_quantile(evap_rel_change,  0.50),
      evap_rel_q95  = safe_quantile(evap_rel_change,  0.95),
      
      avail_rel_q05 = safe_quantile(avail_rel_change, 0.05),
      avail_rel_q50 = safe_quantile(avail_rel_change, 0.50),
      avail_rel_q95 = safe_quantile(avail_rel_change, 0.95),
      
      flux_rel_q05  = safe_quantile(flux_rel_change,  0.05),
      flux_rel_q50  = safe_quantile(flux_rel_change,  0.50),
      flux_rel_q95  = safe_quantile(flux_rel_change,  0.95),
      
      pr_prec_abs_gt0  = safe_prob(prec_abs_change  > 0),
      pr_evap_abs_gt0  = safe_prob(evap_abs_change  > 0),
      pr_avail_abs_gt0 = safe_prob(avail_abs_change > 0),
      pr_flux_abs_gt0  = safe_prob(flux_abs_change  > 0),
      
      pr_prec_abs_lt0  = safe_prob(prec_abs_change  < 0),
      pr_evap_abs_lt0  = safe_prob(evap_abs_change  < 0),
      pr_avail_abs_lt0 = safe_prob(avail_abs_change < 0),
      pr_flux_abs_lt0  = safe_prob(flux_abs_change  < 0),
      
      pr_prec_sen_gt0  = safe_prob(prec_sen  > 0),
      pr_evap_sen_gt0  = safe_prob(evap_sen  > 0),
      pr_avail_sen_gt0 = safe_prob(avail_sen > 0),
      pr_flux_sen_gt0  = safe_prob(flux_sen  > 0),
      
      pr_prec_mk_sig   = safe_prob(prec_mk_p  < ALPHA_SIG),
      pr_evap_mk_sig   = safe_prob(evap_mk_p  < ALPHA_SIG),
      pr_avail_mk_sig  = safe_prob(avail_mk_p < ALPHA_SIG),
      pr_flux_mk_sig   = safe_prob(flux_mk_p  < ALPHA_SIG),
      
      pr_prec_sig_pos  = safe_prob(prec_mk_p  < ALPHA_SIG & prec_sen  > 0),
      pr_evap_sig_pos  = safe_prob(evap_mk_p  < ALPHA_SIG & evap_sen  > 0),
      pr_avail_sig_pos = safe_prob(avail_mk_p < ALPHA_SIG & avail_sen > 0),
      pr_flux_sig_pos  = safe_prob(flux_mk_p  < ALPHA_SIG & flux_sen  > 0),
      
      pr_prec_sig_neg  = safe_prob(prec_mk_p  < ALPHA_SIG & prec_sen  < 0),
      pr_evap_sig_neg  = safe_prob(evap_mk_p  < ALPHA_SIG & evap_sen  < 0),
      pr_avail_sig_neg = safe_prob(avail_mk_p < ALPHA_SIG & avail_sen < 0),
      pr_flux_sig_neg  = safe_prob(flux_mk_p  < ALPHA_SIG & flux_sen  < 0),
      
      pr_wetter_accelerated  = p_story[1],
      pr_drier_accelerated   = p_story[2],
      pr_wetter_decelerated  = p_story[3],
      pr_drier_decelerated   = p_story[4],
      
      modal_storyline      = if (is.na(modal_idx)) NA_character_ else STORYLINE_LEVELS[modal_idx],
      modal_storyline_prob = if (is.na(modal_idx)) NA_real_ else p_story[modal_idx]
    )
  }, by = group_cols][]
}

# Analysis ====================================================================

## Member-level summaries

ensemble_global_member <- build_member_summary(
  dt = ensemble_global_yearly,
  id_cols = c("scenario", "sim_id")
)
ensemble_global_member[, region := "GLOBAL"]
ensemble_global_member[, sim_id := seq_len(.N)]

ensemble_region_member <- build_member_summary(
  dt = ensemble_region_yearly,
  id_cols = c("scenario", "sim_id", "region")
)
ensemble_region_member[, sim_id := seq_len(.N), by = .(region)]

## Scenario summaries

scenario_global_summary <- summarize_scenarios(
  dt_member  = ensemble_global_member,
  group_cols = c("scenario", "region")
)

scenario_region_summary <- summarize_scenarios(
  dt_member  = ensemble_region_member,
  group_cols = c("scenario", "region")
)

# Outputs =====================================================================

saveRDS(ensemble_global_member,
        file.path(PATH_OUTPUT_DATA, "member_level_global_summary.Rds"))

saveRDS(ensemble_region_member,
        file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds"))

saveRDS(scenario_global_summary,
        file.path(PATH_OUTPUT_DATA, "scenario_global_summary.Rds"))

saveRDS(scenario_region_summary,
        file.path(PATH_OUTPUT_DATA, "scenario_region_summary.Rds"))
