# ============================================================================
# SUMMARY SCRIPT FOR TWC STORYLINES
#
# Uses outputs from the yearly aggregation script:
#   A. dataset_global_yearly_prec_evap.Rds
#   B. dataset_region_yearly_prec_evap.Rds
#   C. ensemble_global_yearly_prec_evap.Rds
#   D. ensemble_region_yearly_prec_evap.Rds
#   E. scenario_global_yearly_prec_evap.Rds
#   F. scenario_region_yearly_prec_evap.Rds
#
# Produces:
#   1) member_level_global_summary.Rds
#   2) member_level_region_summary.Rds
#   3) soft_scenario_global_summary.Rds
#   4) soft_scenario_region_summary.Rds
#   5) dataset_global_summary.Rds
#   6) dataset_region_summary.Rds
#   7) soft_vs_top1_global_summary.Rds
#   8) soft_vs_top1_region_summary.Rds
#
# Diagnostics included:
#   - period means
#   - absolute change
#   - relative change
#   - JS divergence
#   - Sen slope
#   - Mann-Kendall p value and significance
#   - storyline class
#   - soft-scenario quantiles and probabilities
#   - storyline entropy
# ============================================================================

# Libraries ==================================================================

install.packages('trend')

library(data.table)
library(trend)

source("source/twc_change.R")

# Inputs =====================================================================

dataset_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_yearly_prec_evap.Rds")
)

dataset_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_yearly_prec_evap.Rds")
)

ensemble_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_global_yearly_prec_evap.Rds")
)

ensemble_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_region_yearly_prec_evap.Rds")
)

scenario_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_yearly_prec_evap.Rds")
)

scenario_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_region_yearly_prec_evap.Rds")
)

# Constants & Variables ========================================================

year_split <- 2001L
alpha_sig  <- 0.05

eps_prec  <- 1
eps_evap  <- 1
eps_flux  <- 1
eps_avail <- 5

js_n_bins <- 10L
js_eps    <- 1e-8

# Helpers ======================================================================

add_metrics_period <- function(dt) {
  dt <- copy(as.data.table(dt))
  dt[, avail := prec - evap]
  dt[, flux  := (prec + evap) / 2]
  dt[, period := fifelse(year < year_split, "bef_2001", "aft_2001")]
  dt[]
}

safe_mean <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
}

safe_prob <- function(cond) {
  cond <- cond[!is.na(cond)]
  if (length(cond) == 0) return(NA_real_)
  mean(cond)
}

safe_rel_change <- function(aft, bef, eps) {
  if (!is.finite(aft) || !is.finite(bef) || is.na(aft) || is.na(bef)) return(NA_real_)
  if (abs(bef) <= eps) return(NA_real_)
  100 * (aft - bef) / abs(bef)
}

norm_entropy4 <- function(p) {
  p <- p[is.finite(p) & !is.na(p) & p > 0]
  if (length(p) == 0) return(NA_real_)
  -sum(p * log(p)) / log(4)
}

calc_js_divergence <- function(x1, x2, n_bins = 6L, eps = 1e-8) {
  x1 <- x1[is.finite(x1) & !is.na(x1)]
  x2 <- x2[is.finite(x2) & !is.na(x2)]
  
  if (length(x1) < 2 || length(x2) < 2) return(NA_real_)
  
  rng <- range(c(x1, x2), na.rm = TRUE)
  if (!all(is.finite(rng))) return(NA_real_)
  
  if (rng[1] == rng[2]) return(0)
  
  breaks <- seq(rng[1], rng[2], length.out = n_bins + 1L)
  
  p <- hist(x1, breaks = breaks, plot = FALSE)$counts + eps
  q <- hist(x2, breaks = breaks, plot = FALSE)$counts + eps
  
  p <- p / sum(p)
  q <- q / sum(q)
  m <- 0.5 * (p + q)
  
  kl_pm <- sum(p * log(p / m))
  kl_qm <- sum(q * log(q / m))
  
  0.5 * (kl_pm + kl_qm)
}

safe_sen <- function(y) {
  y <- as.numeric(y)
  ok <- is.finite(y) & !is.na(y)
  y <- y[ok]
  if (length(y) < 3) return(NA_real_)
  out <- try(trend::sens.slope(y), silent = TRUE)
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out$estimates)
}

safe_mk_p <- function(y) {
  y <- as.numeric(y)
  ok <- is.finite(y) & !is.na(y)
  y <- y[ok]
  if (length(y) < 3) return(NA_real_)
  out <- try(trend::mk.test(y), silent = TRUE)
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out$p.value)
}

classify_storyline <- function(avail_change, flux_change) {
  if (!is.finite(avail_change) || !is.finite(flux_change)) return(NA_character_)
  if (avail_change > 0 && flux_change > 0) return("wetter-accelerated")
  if (avail_change < 0 && flux_change > 0) return("drier-accelerated")
  if (avail_change > 0 && flux_change < 0) return("wetter-decelerated")
  if (avail_change < 0 && flux_change < 0) return("drier-decelerated")
  return("neutral")
}

summarize_one_series <- function(dt_one) {
  dt_one <- copy(as.data.table(dt_one))
  setorder(dt_one, year)
  dt_one <- add_metrics_period(dt_one)
  
  bef <- dt_one[period == "bef_2001"]
  aft <- dt_one[period == "aft_2001"]
  
  prec_bef  <- safe_mean(bef$prec)
  prec_aft  <- safe_mean(aft$prec)
  evap_bef  <- safe_mean(bef$evap)
  evap_aft  <- safe_mean(aft$evap)
  avail_bef <- safe_mean(bef$avail)
  avail_aft <- safe_mean(aft$avail)
  flux_bef  <- safe_mean(bef$flux)
  flux_aft  <- safe_mean(aft$flux)
  
  prec_abs  <- prec_aft  - prec_bef
  evap_abs  <- evap_aft  - evap_bef
  avail_abs <- avail_aft - avail_bef
  flux_abs  <- flux_aft  - flux_bef
  
  prec_rel  <- safe_rel_change(prec_aft,  prec_bef,  eps_prec)
  evap_rel  <- safe_rel_change(evap_aft,  evap_bef,  eps_evap)
  avail_rel <- safe_rel_change(avail_aft, avail_bef, eps_avail)
  flux_rel  <- safe_rel_change(flux_aft,  flux_bef,  eps_flux)
  
  js_prec  <- calc_js_divergence(bef$prec,  aft$prec,  n_bins = js_n_bins, eps = js_eps)
  js_evap  <- calc_js_divergence(bef$evap,  aft$evap,  n_bins = js_n_bins, eps = js_eps)
  js_avail <- calc_js_divergence(bef$avail, aft$avail, n_bins = js_n_bins, eps = js_eps)
  js_flux  <- calc_js_divergence(bef$flux,  aft$flux,  n_bins = js_n_bins, eps = js_eps)
  
  prec_sen  <- safe_sen(dt_one$prec)
  evap_sen  <- safe_sen(dt_one$evap)
  avail_sen <- safe_sen(dt_one$avail)
  flux_sen  <- safe_sen(dt_one$flux)
  
  prec_mk_p  <- safe_mk_p(dt_one$prec)
  evap_mk_p  <- safe_mk_p(dt_one$evap)
  avail_mk_p <- safe_mk_p(dt_one$avail)
  flux_mk_p  <- safe_mk_p(dt_one$flux)
  
  storyline <- classify_storyline(avail_abs, flux_abs)
  
  data.table(
    prec_bef = prec_bef,
    prec_aft = prec_aft,
    evap_bef = evap_bef,
    evap_aft = evap_aft,
    avail_bef = avail_bef,
    avail_aft = avail_aft,
    flux_bef = flux_bef,
    flux_aft = flux_aft,
    
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
    
    prec_mk_sig = is.finite(prec_mk_p)  & !is.na(prec_mk_p)  & prec_mk_p  < alpha_sig,
    evap_mk_sig = is.finite(evap_mk_p)  & !is.na(evap_mk_p)  & evap_mk_p  < alpha_sig,
    avail_mk_sig = is.finite(avail_mk_p) & !is.na(avail_mk_p) & avail_mk_p < alpha_sig,
    flux_mk_sig = is.finite(flux_mk_p)  & !is.na(flux_mk_p)  & flux_mk_p  < alpha_sig,
    
    storyline = storyline
  )
}

build_member_summary <- function(dt, id_cols) {
  dt <- as.data.table(dt)
  
  group_key <- dt[, do.call(paste, c(.SD, sep = "___")), .SDcols = id_cols]
  split_idx <- split(seq_len(nrow(dt)), group_key)
  
  out <- rbindlist(lapply(split_idx, function(ii) {
    dt_one <- dt[ii]
    ids <- unique(dt_one[, ..id_cols])
    cbind(ids, summarize_one_series(dt_one))
  }), fill = TRUE)
  
  out[]
}

summarize_soft_members <- function(dt_member, group_cols) {
  dt_member <- as.data.table(dt_member)
  
  out <- dt_member[
    ,
    {
      p_story <- c(
        safe_prob(storyline == "wetter-accelerated"),
        safe_prob(storyline == "drier-accelerated"),
        safe_prob(storyline == "wetter-decelerated"),
        safe_prob(storyline == "drier-decelerated")
      )
      
      story_names <- c(
        "wetter-accelerated",
        "drier-accelerated",
        "wetter-decelerated",
        "drier-decelerated"
      )
      
      modal_idx <- if (all(is.na(p_story))) NA_integer_ else which.max(replace(p_story, is.na(p_story), -Inf))
      modal_story <- if (is.na(modal_idx)) NA_character_ else story_names[modal_idx]
      modal_prob  <- if (is.na(modal_idx)) NA_real_ else p_story[modal_idx]
      
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
        
        modal_storyline = modal_story,
        modal_storyline_prob = modal_prob,
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

compare_soft_vs_top1 <- function(soft_summary, top1_member, by_cols) {
  soft_keep <- copy(as.data.table(soft_summary))
  top1_keep <- copy(as.data.table(top1_member))
  
  soft_cols <- setdiff(names(soft_keep), by_cols)
  top1_cols <- setdiff(names(top1_keep), by_cols)
  
  setnames(soft_keep, soft_cols, paste0("soft_", soft_cols))
  setnames(top1_keep, top1_cols, paste0("top1_", top1_cols))
  
  out <- merge(soft_keep, top1_keep, by = by_cols, all = TRUE)
  
  if (all(c("soft_avail_abs_q50", "soft_flux_abs_q50", "top1_avail_abs_change", "top1_flux_abs_change") %in% names(out))) {
    out[, soft_modal_quadrant := fifelse(
      soft_avail_abs_q50 > 0 & soft_flux_abs_q50 > 0, "wetter-accelerated",
      fifelse(
        soft_avail_abs_q50 < 0 & soft_flux_abs_q50 > 0, "drier-accelerated",
        fifelse(
          soft_avail_abs_q50 > 0 & soft_flux_abs_q50 < 0, "wetter-decelerated",
          fifelse(
            soft_avail_abs_q50 < 0 & soft_flux_abs_q50 < 0, "drier-decelerated",
            "neutral"
          )
        )
      )
    )]
    
    out[, top1_modal_quadrant := fifelse(
      top1_avail_abs_change > 0 & top1_flux_abs_change > 0, "wetter-accelerated",
      fifelse(
        top1_avail_abs_change < 0 & top1_flux_abs_change > 0, "drier-accelerated",
        fifelse(
          top1_avail_abs_change > 0 & top1_flux_abs_change < 0, "wetter-decelerated",
          fifelse(
            top1_avail_abs_change < 0 & top1_flux_abs_change < 0, "drier-decelerated",
            "neutral"
          )
        )
      )
    )]
    
    out[, top1_soft_storyline_agree := soft_modal_quadrant == top1_modal_quadrant]
  }
  
  if (all(c("soft_flux_abs_q50", "top1_flux_abs_change") %in% names(out))) {
    out[, flux_top1_minus_soft_q50 := top1_flux_abs_change - soft_flux_abs_q50]
  }
  if (all(c("soft_avail_abs_q50", "top1_avail_abs_change") %in% names(out))) {
    out[, avail_top1_minus_soft_q50 := top1_avail_abs_change - soft_avail_abs_q50]
  }
  if (all(c("soft_prec_abs_q50", "top1_prec_abs_change") %in% names(out))) {
    out[, prec_top1_minus_soft_q50 := top1_prec_abs_change - soft_prec_abs_q50]
  }
  if (all(c("soft_evap_abs_q50", "top1_evap_abs_change") %in% names(out))) {
    out[, evap_top1_minus_soft_q50 := top1_evap_abs_change - soft_evap_abs_q50]
  }
  
  out[]
}

# Analysis =====================================================================

## Build member-level summaries ================================================

ensemble_global_member <- build_member_summary(
  dt = ensemble_global_yearly,
  id_cols = c("scenario", "sim_id")
)
ensemble_global_member[, `:=`(source_type = "mc", region = "GLOBAL")]
setcolorder(ensemble_global_member, c("source_type", "scenario", "sim_id", "region",
                                      setdiff(names(ensemble_global_member), c("source_type", "scenario", "sim_id", "region"))))

ensemble_region_member <- build_member_summary(
  dt = ensemble_region_yearly,
  id_cols = c("scenario", "sim_id", "region")
)
ensemble_region_member[, source_type := "mc"]
setcolorder(ensemble_region_member, c("source_type", "scenario", "sim_id", "region",
                                      setdiff(names(ensemble_region_member), c("source_type", "scenario", "sim_id", "region"))))

dataset_global_member <- build_member_summary(
  dt = dataset_global_yearly,
  id_cols = c("dataset")
)
dataset_global_member[, `:=`(source_type = "dataset", region = "GLOBAL")]
setcolorder(dataset_global_member, c("source_type", "dataset", "region",
                                     setdiff(names(dataset_global_member), c("source_type", "dataset", "region"))))

dataset_region_member <- build_member_summary(
  dt = dataset_region_yearly,
  id_cols = c("dataset", "region")
)
dataset_region_member[, source_type := "dataset"]
setcolorder(dataset_region_member, c("source_type", "dataset", "region",
                                     setdiff(names(dataset_region_member), c("source_type", "dataset", "region"))))

top1_global_member <- build_member_summary(
  dt = scenario_global_yearly[grepl("_top1$", scenario)],
  id_cols = c("scenario")
)
top1_global_member[, `:=`(source_type = "top1", region = "GLOBAL")]
setcolorder(top1_global_member, c("source_type", "scenario", "region",
                                  setdiff(names(top1_global_member), c("source_type", "scenario", "region"))))

top1_region_member <- build_member_summary(
  dt = scenario_region_yearly[grepl("_top1$", scenario)],
  id_cols = c("scenario", "region")
)
top1_region_member[, source_type := "top1"]
setcolorder(top1_region_member, c("source_type", "scenario", "region",
                                  setdiff(names(top1_region_member), c("source_type", "scenario", "region"))))

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
  
  saveRDS(
    member_level_global_summary,
    file.path(PATH_OUTPUT_DATA, "ensemble_members_global_summary.Rds")
  )
  
  saveRDS(
    member_level_region_summary,
    file.path(PATH_OUTPUT_DATA, "ensemble_members_region_summary.Rds")
  )
  
  ## Soft-scenario summaries ===================================================
  soft_scenario_global_summary <- summarize_soft_members(
    dt_member = ensemble_global_member,
    group_cols = c("scenario", "region")
  )
  
  soft_scenario_region_summary <- summarize_soft_members(
    dt_member = ensemble_region_member,
    group_cols = c("scenario", "region")
  )
  
  saveRDS(
    soft_scenario_global_summary,
    file.path(PATH_OUTPUT_DATA, "scenario_global_summary.Rds")
  )
  
  saveRDS(
    soft_scenario_region_summary,
    file.path(PATH_OUTPUT_DATA, "scenario_region_summary.Rds")
  )
  
  ## Dataset summaries =========================================================
  
  saveRDS(
    dataset_global_member,
    file.path(PATH_OUTPUT_DATA, "dataset_global_summary.Rds")
  )
  
  saveRDS(
    dataset_region_member,
    file.path(PATH_OUTPUT_DATA, "dataset_region_summary.Rds")
  )
  
  ## Soft vs top1 comparisons ==================================================
  
  top1_global_member_comp <- copy(top1_global_member)
  top1_global_member_comp[, scenario := sub("_top1$", "", scenario)]
  
  top1_region_member_comp <- copy(top1_region_member)
  top1_region_member_comp[, scenario := sub("_top1$", "", scenario)]
  
  soft_vs_top1_global_summary <- compare_soft_vs_top1(
    soft_summary = soft_scenario_global_summary,
    top1_member = top1_global_member_comp,
    by_cols = c("scenario", "region")
  )
  
  soft_vs_top1_region_summary <- compare_soft_vs_top1(
    soft_summary = soft_scenario_region_summary,
    top1_member = top1_region_member_comp,
    by_cols = c("scenario", "region")
  )
  
  saveRDS(
    soft_vs_top1_global_summary,
    file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_global_summary.Rds")
  )
  
  saveRDS(
    soft_vs_top1_region_summary,
    file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_region_summary.Rds")
)