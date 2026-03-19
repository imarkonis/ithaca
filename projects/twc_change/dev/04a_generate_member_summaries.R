# ============================================================================
# Summarize dataset, ensemble member, scenario, and top1 TWC storylines
#
# This script:
# 1. Builds member level summaries for datasets, Monte Carlo ensemble members,
#    and top1 scenarios
# 2. Aggregates Monte Carlo members into scenario summaries
# 3. Compares scenario summaries against deterministic top1 scenarios
# 4. Saves global and regional summary products
#
# Diagnostics include:
#   - period means
#   - absolute and relative change
#   - JS divergence
#   - Sen slope
#   - Mann Kendall p value and significance
#   - storyline class
#   - scenario quantiles and probabilities
#   - storyline entropy
# ============================================================================

# Libraries ===================================================================

library(trend)

source("source/twc_change.R")

# Inputs ======================================================================

dataset_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_yearly_prec_evap.Rds")
)

dataset_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_yearly_prec_evap.Rds")
)

ensemble_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_yearly_prec_evap.Rds")
)

ensemble_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_yearly_prec_evap.Rds")
)

scenario_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_yearly_prec_evap.Rds")
)

scenario_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_region_yearly_prec_evap.Rds")
)

# Constants ===================================================================

YEAR_SPLIT <- 2002L
ALPHA_SIG <- 0.05

EPS_PREC <- 1
EPS_EVAP <- 1
EPS_FLUX <- 1
EPS_AVAIL <- 5

JS_N_BINS <- 10L
JS_EPS <- 1e-8

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
    avail = prec - evap,
    flux = (prec + evap) / 2,
    period = fifelse(year < YEAR_SPLIT, "1982_2001", "2002_2021")
  )]
  
  dt[]
}

safe_mean <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x) & !is.na(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
}

safe_prob <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_rel_change <- function(period_2, period_1, eps) {
  if (!is.finite(period_2) || !is.finite(period_1) || is.na(period_2) || is.na(period_1)) {
    return(NA_real_)
  }
  
  if (abs(period_1) <= eps) {
    return(NA_real_)
  }
  
  100 * (period_2 - period_1) / abs(period_1)
}

norm_entropy4 <- function(p) {
  p <- p[is.finite(p) & !is.na(p) & p > 0]
  if (!length(p)) return(NA_real_)
  -sum(p * log(p)) / log(4)
}

calc_js_divergence <- function(x1, x2, n_bins = JS_N_BINS, eps = JS_EPS) {
  x1 <- x1[is.finite(x1) & !is.na(x1)]
  x2 <- x2[is.finite(x2) & !is.na(x2)]
  
  if (length(x1) < 2 || length(x2) < 2) {
    return(NA_real_)
  }
  
  rng <- range(c(x1, x2), na.rm = TRUE)
  if (!all(is.finite(rng))) {
    return(NA_real_)
  }
  
  if (rng[1] == rng[2]) {
    return(0)
  }
  
  breaks <- seq(rng[1], rng[2], length.out = n_bins + 1L)
  
  p <- hist(x1, breaks = breaks, plot = FALSE)$counts + eps
  q <- hist(x2, breaks = breaks, plot = FALSE)$counts + eps
  
  p <- p / sum(p)
  q <- q / sum(q)
  m <- 0.5 * (p + q)
  
  0.5 * (
    sum(p * log(p / m)) +
      sum(q * log(q / m))
  )
}

safe_sen <- function(y) {
  y <- as.numeric(y)
  y <- y[is.finite(y) & !is.na(y)]
  
  if (length(y) < 3) {
    return(NA_real_)
  }
  
  out <- try(trend::sens.slope(y), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(NA_real_)
  }
  
  as.numeric(out$estimates)
}

safe_mk_p <- function(y) {
  y <- as.numeric(y)
  y <- y[is.finite(y) & !is.na(y)]
  
  if (length(y) < 3) {
    return(NA_real_)
  }
  
  out <- try(trend::mk.test(y), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(NA_real_)
  }
  
  as.numeric(out$p.value)
}

classify_storyline <- function(avail_change, flux_change) {
  if (!is.finite(avail_change) || !is.finite(flux_change)) {
    return(NA_character_)
  }
  
  if (avail_change > 0 && flux_change > 0) return("wetter-accelerated")
  if (avail_change < 0 && flux_change > 0) return("drier-accelerated")
  if (avail_change > 0 && flux_change < 0) return("wetter-decelerated")
  if (avail_change < 0 && flux_change < 0) return("drier-decelerated")
  
  "neutral"
}

summarize_one_series <- function(dt_one) {
  dt_one <- copy(as.data.table(dt_one))
  setorder(dt_one, year)
  dt_one <- add_metrics_period(dt_one)
  
  period_1 <- dt_one[period == "1982_2001"]
  period_2 <- dt_one[period == "2002_2021"]
  
  prec_1982_2001 <- safe_mean(period_1$prec)
  prec_2002_2021 <- safe_mean(period_2$prec)
  evap_1982_2001 <- safe_mean(period_1$evap)
  evap_2002_2021 <- safe_mean(period_2$evap)
  avail_1982_2001 <- safe_mean(period_1$avail)
  avail_2002_2021 <- safe_mean(period_2$avail)
  flux_1982_2001 <- safe_mean(period_1$flux)
  flux_2002_2021 <- safe_mean(period_2$flux)
  
  prec_abs <- prec_2002_2021 - prec_1982_2001
  evap_abs <- evap_2002_2021 - evap_1982_2001
  avail_abs <- avail_2002_2021 - avail_1982_2001
  flux_abs <- flux_2002_2021 - flux_1982_2001
  
  prec_rel <- safe_rel_change(prec_2002_2021, prec_1982_2001, EPS_PREC)
  evap_rel <- safe_rel_change(evap_2002_2021, evap_1982_2001, EPS_EVAP)
  avail_rel <- safe_rel_change(avail_2002_2021, avail_1982_2001, EPS_AVAIL)
  flux_rel <- safe_rel_change(flux_2002_2021, flux_1982_2001, EPS_FLUX)
  
  js_prec <- calc_js_divergence(period_1$prec, period_2$prec)
  js_evap <- calc_js_divergence(period_1$evap, period_2$evap)
  js_avail <- calc_js_divergence(period_1$avail, period_2$avail)
  js_flux <- calc_js_divergence(period_1$flux, period_2$flux)
  
  prec_sen <- safe_sen(dt_one$prec)
  evap_sen <- safe_sen(dt_one$evap)
  avail_sen <- safe_sen(dt_one$avail)
  flux_sen <- safe_sen(dt_one$flux)
  
  prec_mk_p <- safe_mk_p(dt_one$prec)
  evap_mk_p <- safe_mk_p(dt_one$evap)
  avail_mk_p <- safe_mk_p(dt_one$avail)
  flux_mk_p <- safe_mk_p(dt_one$flux)
  
  data.table(
    prec_1982_2001 = prec_1982_2001,
    prec_2002_2021 = prec_2002_2021,
    evap_1982_2001 = evap_1982_2001,
    evap_2002_2021 = evap_2002_2021,
    avail_1982_2001 = avail_1982_2001,
    avail_2002_2021 = avail_2002_2021,
    flux_1982_2001 = flux_1982_2001,
    flux_2002_2021 = flux_2002_2021,
    
    prec_abs_change = prec_abs,
    evap_abs_change = evap_abs,
    avail_abs_change = avail_abs,
    flux_abs_change = flux_abs,
    
    prec_rel_change = prec_rel,
    evap_rel_change = evap_rel,
    avail_rel_change = avail_rel,
    flux_rel_change = flux_rel,
    
    prec_rel_valid = !is.na(prec_rel),
    evap_rel_valid = !is.na(evap_rel),
    avail_rel_valid = !is.na(avail_rel),
    flux_rel_valid = !is.na(flux_rel),
    
    js_prec = js_prec,
    js_evap = js_evap,
    js_avail = js_avail,
    js_flux = js_flux,
    
    prec_sen = prec_sen,
    evap_sen = evap_sen,
    avail_sen = avail_sen,
    flux_sen = flux_sen,
    
    prec_mk_p = prec_mk_p,
    evap_mk_p = evap_mk_p,
    avail_mk_p = avail_mk_p,
    flux_mk_p = flux_mk_p,
    
    prec_mk_sig = is.finite(prec_mk_p) & !is.na(prec_mk_p) & prec_mk_p < ALPHA_SIG,
    evap_mk_sig = is.finite(evap_mk_p) & !is.na(evap_mk_p) & evap_mk_p < ALPHA_SIG,
    avail_mk_sig = is.finite(avail_mk_p) & !is.na(avail_mk_p) & avail_mk_p < ALPHA_SIG,
    flux_mk_sig = is.finite(flux_mk_p) & !is.na(flux_mk_p) & flux_mk_p < ALPHA_SIG,
    
    storyline = classify_storyline(avail_abs, flux_abs)
  )
}

build_member_summary <- function(dt, id_cols) {
  dt <- as.data.table(dt)
  
  split_idx <- split(
    seq_len(nrow(dt)),
    dt[, do.call(paste, c(.SD, sep = "___")), .SDcols = id_cols]
  )
  
  out <- rbindlist(
    lapply(split_idx, function(ii) {
      dt_one <- dt[ii]
      ids <- unique(dt_one[, ..id_cols])
      cbind(ids, summarize_one_series(dt_one))
    }),
    fill = TRUE,
    use.names = TRUE
  )
  
  out[]
}

summarize_scenarios <- function(dt_member, group_cols) {
  dt_member <- as.data.table(dt_member)
  
  out <- dt_member[
    ,
    {
      p_story <- c(
        safe_prob(storyline == STORYLINE_LEVELS[1]),
        safe_prob(storyline == STORYLINE_LEVELS[2]),
        safe_prob(storyline == STORYLINE_LEVELS[3]),
        safe_prob(storyline == STORYLINE_LEVELS[4])
      )
      
      modal_idx <- if (all(is.na(p_story))) {
        NA_integer_
      } else {
        which.max(replace(p_story, is.na(p_story), -Inf))
      }
      
      .(
        n_member = .N,
        
        prec_abs_q05 = safe_quantile(prec_abs_change, 0.05),
        prec_abs_q50 = safe_quantile(prec_abs_change, 0.50),
        prec_abs_q95 = safe_quantile(prec_abs_change, 0.95),
        
        evap_abs_q05 = safe_quantile(evap_abs_change, 0.05),
        evap_abs_q50 = safe_quantile(evap_abs_change, 0.50),
        evap_abs_q95 = safe_quantile(evap_abs_change, 0.95),
        
        avail_abs_q05 = safe_quantile(avail_abs_change, 0.05),
        avail_abs_q50 = safe_quantile(avail_abs_change, 0.50),
        avail_abs_q95 = safe_quantile(avail_abs_change, 0.95),
        
        flux_abs_q05 = safe_quantile(flux_abs_change, 0.05),
        flux_abs_q50 = safe_quantile(flux_abs_change, 0.50),
        flux_abs_q95 = safe_quantile(flux_abs_change, 0.95),
        
        prec_rel_q05 = safe_quantile(prec_rel_change, 0.05),
        prec_rel_q50 = safe_quantile(prec_rel_change, 0.50),
        prec_rel_q95 = safe_quantile(prec_rel_change, 0.95),
        
        evap_rel_q05 = safe_quantile(evap_rel_change, 0.05),
        evap_rel_q50 = safe_quantile(evap_rel_change, 0.50),
        evap_rel_q95 = safe_quantile(evap_rel_change, 0.95),
        
        avail_rel_q05 = safe_quantile(avail_rel_change, 0.05),
        avail_rel_q50 = safe_quantile(avail_rel_change, 0.50),
        avail_rel_q95 = safe_quantile(avail_rel_change, 0.95),
        
        flux_rel_q05 = safe_quantile(flux_rel_change, 0.05),
        flux_rel_q50 = safe_quantile(flux_rel_change, 0.50),
        flux_rel_q95 = safe_quantile(flux_rel_change, 0.95),
        
        pr_prec_abs_gt0 = safe_prob(prec_abs_change > 0),
        pr_evap_abs_gt0 = safe_prob(evap_abs_change > 0),
        pr_avail_abs_gt0 = safe_prob(avail_abs_change > 0),
        pr_flux_abs_gt0 = safe_prob(flux_abs_change > 0),
        
        pr_prec_abs_lt0 = safe_prob(prec_abs_change < 0),
        pr_evap_abs_lt0 = safe_prob(evap_abs_change < 0),
        pr_avail_abs_lt0 = safe_prob(avail_abs_change < 0),
        pr_flux_abs_lt0 = safe_prob(flux_abs_change < 0),
        
        pr_prec_sen_gt0 = safe_prob(prec_sen > 0),
        pr_evap_sen_gt0 = safe_prob(evap_sen > 0),
        pr_avail_sen_gt0 = safe_prob(avail_sen > 0),
        pr_flux_sen_gt0 = safe_prob(flux_sen > 0),
        
        pr_prec_mk_sig = safe_prob(prec_mk_sig),
        pr_evap_mk_sig = safe_prob(evap_mk_sig),
        pr_avail_mk_sig = safe_prob(avail_mk_sig),
        pr_flux_mk_sig = safe_prob(flux_mk_sig),
        
        pr_prec_sig_pos = safe_prob(prec_mk_sig & prec_sen > 0),
        pr_evap_sig_pos = safe_prob(evap_mk_sig & evap_sen > 0),
        pr_avail_sig_pos = safe_prob(avail_mk_sig & avail_sen > 0),
        pr_flux_sig_pos = safe_prob(flux_mk_sig & flux_sen > 0),
        
        pr_prec_sig_neg = safe_prob(prec_mk_sig & prec_sen < 0),
        pr_evap_sig_neg = safe_prob(evap_mk_sig & evap_sen < 0),
        pr_avail_sig_neg = safe_prob(avail_mk_sig & avail_sen < 0),
        pr_flux_sig_neg = safe_prob(flux_mk_sig & flux_sen < 0),
        
        pr_wetter_accelerated = p_story[1],
        pr_drier_accelerated = p_story[2],
        pr_wetter_decelerated = p_story[3],
        pr_drier_decelerated = p_story[4],
        
        modal_storyline = if (is.na(modal_idx)) NA_character_ else STORYLINE_LEVELS[modal_idx],
        modal_storyline_prob = if (is.na(modal_idx)) NA_real_ else p_story[modal_idx],
        storyline_entropy = norm_entropy4(p_story),
        
        js_prec_q50 = safe_quantile(js_prec, 0.50),
        js_evap_q50 = safe_quantile(js_evap, 0.50),
        js_avail_q50 = safe_quantile(js_avail, 0.50),
        js_flux_q50 = safe_quantile(js_flux, 0.50)
      )
    },
    by = group_cols
  ]
  
  out[]
}

compare_scenario_vs_top1 <- function(scenario_summary, top1_member, by_cols) {
  scenario_keep <- copy(as.data.table(scenario_summary))
  top1_keep <- copy(as.data.table(top1_member))
  
  scenario_cols <- setdiff(names(scenario_keep), by_cols)
  top1_cols <- setdiff(names(top1_keep), by_cols)
  
  setnames(scenario_keep, scenario_cols, paste0("scenario_", scenario_cols))
  setnames(top1_keep, top1_cols, paste0("top1_", top1_cols))
  
  out <- merge(scenario_keep, top1_keep, by = by_cols, all = TRUE)
  
  if (all(c(
    "scenario_avail_abs_q50",
    "scenario_flux_abs_q50",
    "top1_avail_abs_change",
    "top1_flux_abs_change"
  ) %in% names(out))) {
    out[, scenario_modal_quadrant := classify_storyline(
      scenario_avail_abs_q50,
      scenario_flux_abs_q50
    )]
    
    out[, top1_modal_quadrant := classify_storyline(
      top1_avail_abs_change,
      top1_flux_abs_change
    )]
    
    out[, top1_scenario_storyline_agree := (
      scenario_modal_quadrant == top1_modal_quadrant
    )]
  }
  
  if (all(c("scenario_flux_abs_q50", "top1_flux_abs_change") %in% names(out))) {
    out[, flux_top1_minus_scenario_q50 := (
      top1_flux_abs_change - scenario_flux_abs_q50
    )]
  }
  
  if (all(c("scenario_avail_abs_q50", "top1_avail_abs_change") %in% names(out))) {
    out[, avail_top1_minus_scenario_q50 := (
      top1_avail_abs_change - scenario_avail_abs_q50
    )]
  }
  
  if (all(c("scenario_prec_abs_q50", "top1_prec_abs_change") %in% names(out))) {
    out[, prec_top1_minus_scenario_q50 := (
      top1_prec_abs_change - scenario_prec_abs_q50
    )]
  }
  
  if (all(c("scenario_evap_abs_q50", "top1_evap_abs_change") %in% names(out))) {
    out[, evap_top1_minus_scenario_q50 := (
      top1_evap_abs_change - scenario_evap_abs_q50
    )]
  }
  
  out[]
}

# Analysis ====================================================================

## Member level summaries 

ensemble_global_member <- build_member_summary(
  dt = ensemble_global_yearly,
  id_cols = c("scenario", "sim_id")
)

ensemble_global_member[, `:=`(
  source_type = "mc",
  region = "GLOBAL"
)]
setcolorder(
  ensemble_global_member,
  c(
    "source_type", "scenario", "sim_id", "region",
    setdiff(
      names(ensemble_global_member),
      c("source_type", "scenario", "sim_id", "region")
    )
  )
)

ensemble_region_member <- build_member_summary(
  dt = ensemble_region_yearly,
  id_cols = c("scenario", "sim_id", "region")
)

ensemble_region_member[, source_type := "mc"]
setcolorder(
  ensemble_region_member,
  c(
    "source_type", "scenario", "sim_id", "region",
    setdiff(
      names(ensemble_region_member),
      c("source_type", "scenario", "sim_id", "region")
    )
  )
)

dataset_global_member <- build_member_summary(
  dt = dataset_global_yearly,
  id_cols = c("dataset")
)
dataset_global_member[, `:=`(
  source_type = "dataset",
  region = "GLOBAL"
)]
setcolorder(
  dataset_global_member,
  c(
    "source_type", "dataset", "region",
    setdiff(
      names(dataset_global_member),
      c("source_type", "dataset", "region")
    )
  )
)

dataset_region_member <- build_member_summary(
  dt = dataset_region_yearly,
  id_cols = c("dataset", "region")
)
dataset_region_member[, source_type := "dataset"]
setcolorder(
  dataset_region_member,
  c(
    "source_type", "dataset", "region",
    setdiff(
      names(dataset_region_member),
      c("source_type", "dataset", "region")
    )
  )
)

top1_global_member <- build_member_summary(
  dt = scenario_global_yearly[grepl("_top1$", scenario)],
  id_cols = c("scenario")
)
top1_global_member[, `:=`(
  source_type = "top1",
  region = "GLOBAL"
)]
setcolorder(
  top1_global_member,
  c(
    "source_type", "scenario", "region",
    setdiff(
      names(top1_global_member),
      c("source_type", "scenario", "region")
    )
  )
)

top1_region_member <- build_member_summary(
  dt = scenario_region_yearly[grepl("_top1$", scenario)],
  id_cols = c("scenario", "region")
)
top1_region_member[, source_type := "top1"]
setcolorder(
  top1_region_member,
  c(
    "source_type", "scenario", "region",
    setdiff(
      names(top1_region_member),
      c("source_type", "scenario", "region")
    )
  )
)

member_level_global_summary <- rbindlist(
  list(
    ensemble_global_member,
    dataset_global_member,
    top1_global_member
  ),
  fill = TRUE,
  use.names = TRUE
)

member_level_region_summary <- rbindlist(
  list(
    ensemble_region_member,
    dataset_region_member,
    top1_region_member
  ),
  fill = TRUE,
  use.names = TRUE
)

# Scenario summaries ----------------------------------------------------------

scenario_global_summary <- summarize_scenarios(
  dt_member = ensemble_global_member,
  group_cols = c("scenario", "region")
)

scenario_region_summary <- summarize_scenarios(
  dt_member = ensemble_region_member,
  group_cols = c("scenario", "region")
)

# Scenario vs top1 comparisons ------------------------------------------------

top1_global_member_comp <- copy(top1_global_member)
top1_global_member_comp[, scenario := sub("_top1$", "", scenario)]

top1_region_member_comp <- copy(top1_region_member)
top1_region_member_comp[, scenario := sub("_top1$", "", scenario)]

scenario_vs_top1_global_summary <- compare_scenario_vs_top1(
  scenario_summary = scenario_global_summary,
  top1_member = top1_global_member_comp,
  by_cols = c("scenario", "region")
)

scenario_vs_top1_region_summary <- compare_scenario_vs_top1(
  scenario_summary = scenario_region_summary,
  top1_member = top1_region_member_comp,
  by_cols = c("scenario", "region")
)

# Outputs 

saveRDS(
  member_level_global_summary,
  file.path(PATH_OUTPUT_DATA, "member_level_global_summary.Rds")
)

saveRDS(
  member_level_region_summary,
  file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds")
)

saveRDS(
  scenario_global_summary,
  file.path(PATH_OUTPUT_DATA, "scenario_global_summary.Rds")
)

saveRDS(
  scenario_region_summary,
  file.path(PATH_OUTPUT_DATA, "scenario_region_summary.Rds")
)
saveRDS(
  dataset_global_member,
  file.path(PATH_OUTPUT_DATA, "dataset_global_summary.Rds")
)

saveRDS(
  dataset_region_member,
  file.path(PATH_OUTPUT_DATA, "dataset_region_summary.Rds")
)

saveRDS(
  scenario_vs_top1_global_summary,
  file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_global_summary.Rds")
)

saveRDS(
  scenario_vs_top1_region_summary,
  file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_region_summary.Rds")
)
