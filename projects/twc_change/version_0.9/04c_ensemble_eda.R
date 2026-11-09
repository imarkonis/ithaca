# ============================================================================
# EDA SCRIPT FOR EXPLORING TWC STORYLINE RESULTS
#
# Purpose:
#   First-pass exploration of storyline outputs based on:
#     1) scenario_global_summary.Rds
#     2) scenario_region_summary.Rds
#     3) scenario_vs_top1_global_summary.Rds
#     4) scenario_vs_top1_region_summary.Rds
#     5) dataset_region_summary.Rds
#     6) member_level_region_summary.Rds
#     7) member_level_global_summary.Rds
#
# What this script does:
#   1. Global storyline overview by scenario
#   2. Global soft vs top1 consistency
#   3. Regional storyline quadrant classification
#   4. Regional storyline uncertainty and entropy
#   5. Regional decomposition into P and E contributions
#   6. Sign robustness across members
#   7. Significance robustness across members
#   8. Dataset dependence versus ensemble behavior
#   9. Stable vs fragile region ranking
#  10. Time-series inspection plan for archetype regions
#
# IMPORTANT INTERPRETATION NOTE:
#   This script is not just descriptive. Each block includes comments on:
#   - what we are checking
#   - what patterns would be interesting
#   - what those patterns may imply for storyline interpretation
#
# Suggested use:
#   Run section by section and inspect outputs interactively.
# ============================================================================


# Libraries ==================================================================

source("source/twc_change.R")

# Inputs =====================================================================

ensemble_members_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_members_global_summary.Rds")
)

ensemble_members_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "ensemble_members_region_summary.Rds")
)

scenario_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_summary.Rds")
)

scenario_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_region_summary.Rds")
)

dataset_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_summary.Rds")
)

dataset_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_summary.Rds")
)

scenario_vs_top1_global <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_global_summary.Rds")
)

scenario_vs_top1_region <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_vs_top1_region_summary.Rds")
)
# Output dir =================================================================

out_dir <- file.path(PATH_OUTPUT_FIGURES, "twc_eda")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Helpers ====================================================================

theme_eda <- function() {
  theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey95")
    )
}

classify_quadrant <- function(avail, flux, eps_avail = 0, eps_flux = 0) {
  out <- rep(NA_character_, length(avail))
  
  neutral <- abs(avail) <= eps_avail | abs(flux) <= eps_flux
  out[neutral] <- "neutral"
  
  out[!neutral & avail > 0 & flux > 0] <- "wetter-accelerated"
  out[!neutral & avail < 0 & flux > 0] <- "drier-accelerated"
  out[!neutral & avail > 0 & flux < 0] <- "wetter-decelerated"
  out[!neutral & avail < 0 & flux < 0] <- "drier-decelerated"
  
  out
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(NA_real_, length(x)))
  (x - rng[1]) / diff(rng)
}

safe_abs <- function(x) ifelse(is.na(x), NA_real_, abs(x))

get_top_n <- function(dt, col, n = 10L, decreasing = TRUE) {
  dt <- copy(as.data.table(dt))
  setorderv(dt, cols = col, order = if (decreasing) -1L else 1L, na.last = TRUE)
  dt[seq_len(min(n, nrow(dt)))]
}

plot_quadrant_background <- function() {
  list(
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4),
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4)
  )
}


# ============================================================================
# 1) GLOBAL STORYLINE OVERVIEW BY SCENARIO
# ----------------------------------------------------------------------------
# What we ask:
#   What is the central storyline for each scenario at global scale?
#
# What we look at:
#   - median change in availability (P - E)
#   - median change in throughput / flux ((P + E)/2)
#   - uncertainty envelope (q05 to q95)
#
# What it may imply:
#   - If most scenarios lie in wetter-accelerated: global moistening and
#     intensification co-occur
#   - If most lie in drier-accelerated: higher throughput but reduced
#     availability, i.e. acceleration with drying stress
#   - Wide intervals imply storyline fragility even if medians look clean
# ============================================================================

global_quad <- copy(scenario_global)
global_quad[, storyline_q50 := classify_quadrant(avail_abs_q50, flux_abs_q50)]

f_global_quad <- ggplot(global_quad, aes(x = avail_abs_q50, y = flux_abs_q50, label = scenario)) +
  plot_quadrant_background() +
  geom_errorbar(aes(ymin = flux_abs_q05, ymax = flux_abs_q95), width = 0) +
  geom_errorbarh(aes(xmin = avail_abs_q05, xmax = avail_abs_q95), height = 0) +
  geom_point(aes(shape = storyline_q50), size = 3) +
  geom_text(nudge_y = 0.02 * max(abs(global_quad$flux_abs_q50), na.rm = TRUE)) +
  labs(
    title = "Global median storyline by scenario",
    x = "Availability change median: Δ(P-E)",
    y = "Flux change median: Δ(P+E)/2",
    shape = "Median storyline"
  ) +
  theme_eda()

ggsave(file.path(out_dir, "01_global_storyline_by_scenario.png"), f_global_quad, width = 9, height = 6, dpi = 300)

global_story_table <- global_quad[
  ,
  .(
    scenario,
    n_member,
    storyline_q50,
    avail_abs_q05, avail_abs_q50, avail_abs_q95,
    flux_abs_q05, flux_abs_q50, flux_abs_q95,
    pr_wetter_accelerated,
    pr_drier_accelerated,
    pr_wetter_decelerated,
    pr_drier_decelerated,
    storyline_entropy
  )
]

fwrite(global_story_table, file.path(out_dir, "01_global_storyline_by_scenario.csv"))

print(global_story_table)

# Interpretation notes:
# - Look first at storyline_q50.
# - Then compare it with storyline_entropy and modal probabilities.
# - A scenario with clear median but high entropy means the central point is not
#   representative of many members.
# - A scenario with narrow q05-q95 and low entropy is a strong candidate for a
#   robust storyline.


# ============================================================================
# 2) GLOBAL SOFT VS TOP1 CONSISTENCY
# ----------------------------------------------------------------------------
# What we ask:
#   Does the deterministic top1 representation capture the soft scenario
#   storyline reasonably well?
#
# What we look at:
#   - same quadrant or not
#   - distance between top1 and soft q50 for P, E, availability, flux
#
# What it may imply:
#   - strong agreement: top1 is a fair simplification
#   - systematic bias: top1 may distort ensemble interpretation
# ============================================================================

global_compare <- copy(scenario_vs_top1_global)

fwrite(global_compare, file.path(out_dir, "02_global_soft_vs_top1.csv"))

f_global_compare <- ggplot(global_compare, aes(x = soft_avail_abs_q50, y = soft_flux_abs_q50, label = scenario)) +
  plot_quadrant_background() +
  geom_point(size = 3) +
  geom_point(aes(x = top1_avail_abs_change, y = top1_flux_abs_change), shape = 1, size = 4) +
  geom_segment(
    aes(
      xend = top1_avail_abs_change,
      yend = top1_flux_abs_change
    ),
    linewidth = 0.5,
    alpha = 0.7
  ) +
  geom_text(nudge_y = 0.02 * max(abs(global_compare$soft_flux_abs_q50), na.rm = TRUE)) +
  labs(
    title = "Global soft median vs top1 storyline",
    subtitle = "Filled point = soft q50, open point = top1",
    x = "Availability change",
    y = "Flux change"
  ) +
  theme_eda()

ggsave(file.path(out_dir, "02_global_soft_vs_top1.png"), f_global_compare, width = 9, height = 6, dpi = 300)

print(global_compare[
  ,
  .(
    scenario,
    soft_modal_quadrant,
    top1_modal_quadrant,
    top1_soft_storyline_agree,
    prec_top1_minus_soft_q50,
    evap_top1_minus_soft_q50,
    avail_top1_minus_soft_q50,
    flux_top1_minus_soft_q50
  )
])

# Interpretation notes:
# - Cases with disagreement in quadrant deserve immediate attention.
# - Even if quadrants agree, very large magnitude differences may mean top1 is
#   exaggerating or damping the ensemble median.
# - If top1 is consistently more extreme than soft q50, that suggests selection
#   may favor edge products, not representative products.


# ============================================================================
# 3) REGIONAL QUADRANT MAP OF MEDIAN STORYLINE
# ----------------------------------------------------------------------------
# What we ask:
#   How many regions fall into each storyline quadrant under each scenario?
#
# What we look at:
#   - quadrant counts by scenario
#   - region-level median positions in Δ(P-E), Δ(P+E)/2 space
#
# What it may imply:
#   - dominance of one quadrant across many regions suggests a coherent large-
#     scale storyline
#   - mixed quadrants imply strong regional heterogeneity
# ============================================================================

region_quad <- copy(scenario_region)
region_quad[, storyline_q50 := classify_quadrant(avail_abs_q50, flux_abs_q50)]

region_quad_counts <- region_quad[
  ,
  .N,
  by = .(scenario, storyline_q50)
][order(scenario, -N)]

fwrite(region_quad_counts, file.path(out_dir, "03_region_quadrant_counts.csv"))

f_region_quad_counts <- ggplot(region_quad_counts, aes(x = scenario, y = N, fill = storyline_q50)) +
  geom_col(position = "stack") +
  labs(
    title = "Regional storyline counts by scenario",
    x = NULL,
    y = "Number of regions",
    fill = "Median storyline"
  ) +
  theme_eda() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "03_region_quadrant_counts.png"), f_region_quad_counts, width = 10, height = 6, dpi = 300)

f_region_quad_scatter <- ggplot(region_quad, aes(x = avail_abs_q50, y = flux_abs_q50, label = region)) +
  plot_quadrant_background() +
  geom_point() +
  facet_wrap(~ scenario) +
  labs(
    title = "Regional median storylines in availability-flux space",
    x = "Availability change median: Δ(P-E)",
    y = "Flux change median: Δ(P+E)/2"
  ) +
  theme_eda()

ggsave(file.path(out_dir, "03_region_storyline_scatter.png"), f_region_quad_scatter, width = 12, height = 8, dpi = 300)

print(region_quad_counts)

# Interpretation notes:
# - If one scenario shifts many regions across the vertical axis, availability
#   diagnosis is sensitive to weighting assumptions.
# - If one scenario shifts many regions across the horizontal axis, throughput /
#   intensification diagnosis is sensitive.
# - Regions near the origin are especially likely to be classification-fragile.


# ============================================================================
# 4) REGIONAL STORYLINE UNCERTAINTY AND ENTROPY
# ----------------------------------------------------------------------------
# What we ask:
#   Which regions have robust storylines and which are ambiguous?
#
# What we look at:
#   - modal storyline probability
#   - normalized entropy
#
# What it may imply:
#   - low entropy + high modal probability = robust storyline region
#   - high entropy = strong member disagreement
#   - high entropy regions should be interpreted cautiously in synthesis
# ============================================================================

region_uncertainty <- region_quad[
  ,
  .(
    scenario,
    region,
    storyline_q50,
    modal_storyline,
    modal_storyline_prob,
    storyline_entropy,
    pr_wetter_accelerated,
    pr_drier_accelerated,
    pr_wetter_decelerated,
    pr_drier_decelerated
  )
]

fwrite(region_uncertainty, file.path(out_dir, "04_region_storyline_uncertainty.csv"))

f_entropy <- ggplot(region_uncertainty, aes(x = reorder(region, storyline_entropy), y = storyline_entropy, fill = modal_storyline)) +
  geom_col() +
  facet_wrap(~ scenario, scales = "free_x") +
  labs(
    title = "Regional storyline entropy by scenario",
    x = "Region",
    y = "Storyline entropy",
    fill = "Modal storyline"
  ) +
  theme_eda() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(file.path(out_dir, "04_region_storyline_entropy.png"), f_entropy, width = 14, height = 9, dpi = 300)

high_entropy_regions <- get_top_n(region_uncertainty, "storyline_entropy", n = 20)
low_entropy_regions  <- get_top_n(region_uncertainty, "storyline_entropy", n = 20, decreasing = FALSE)

fwrite(high_entropy_regions, file.path(out_dir, "04_high_entropy_regions.csv"))
fwrite(low_entropy_regions, file.path(out_dir, "04_low_entropy_regions.csv"))

print(high_entropy_regions)
print(low_entropy_regions)

# Interpretation notes:
# - Use high-entropy regions as "uncertain archetypes".
# - Use low-entropy regions as "robust archetypes".
# - A mismatch between storyline_q50 and modal_storyline is itself interesting:
#   it means the median point in Δ(P-E), Δ(P+E)/2 space may not reflect the most
#   common member-level quadrant.


# ============================================================================
# 5) REGIONAL DECOMPOSITION INTO P AND E CONTRIBUTIONS
# ----------------------------------------------------------------------------
# What we ask:
#   Are storylines driven more by precipitation change, evaporation change, or
#   both together?
#
# What we look at:
#   - median ΔP versus median ΔE
#   - relation to Δ(P-E) and Δ(P+E)/2
#
# What it may imply:
#   - ΔP > 0 and ΔE > 0 with ΔP > ΔE -> wetter acceleration
#   - ΔP > 0 and ΔE > 0 with ΔE > ΔP -> drier acceleration
#   - ΔP < 0 and ΔE < 0 can still yield complex availability outcomes
# ============================================================================

region_pe <- region_quad[
  ,
  .(
    scenario,
    region,
    storyline_q50,
    prec_abs_q50,
    evap_abs_q50,
    avail_abs_q50,
    flux_abs_q50
  )
]

fwrite(region_pe, file.path(out_dir, "05_region_prec_evap_decomposition.csv"))

f_prec_evap <- ggplot(region_pe, aes(x = prec_abs_q50, y = evap_abs_q50, label = region, shape = storyline_q50)) +
  plot_quadrant_background() +
  geom_point(size = 2.5) +
  facet_wrap(~ scenario) +
  labs(
    title = "Regional median ΔP versus median ΔE",
    x = "Precipitation change median: ΔP",
    y = "Evaporation change median: ΔE",
    shape = "Median storyline"
  ) +
  theme_eda()

ggsave(file.path(out_dir, "05_region_prec_vs_evap.png"), f_prec_evap, width = 12, height = 8, dpi = 300)

region_pe_ratio <- copy(region_pe)
region_pe_ratio[, pe_balance := prec_abs_q50 - evap_abs_q50]
region_pe_ratio[, pe_sum     := prec_abs_q50 + evap_abs_q50]

fwrite(region_pe_ratio, file.path(out_dir, "05_region_prec_evap_balance.csv"))

# Interpretation notes:
# - Regions with both P and E rising are candidate TWC intensification regions.
# - The sign of (ΔP - ΔE) is directly linked to availability outcome.
# - The sign of (ΔP + ΔE) indicates whether overall throughput rises or falls.
# - This is a key bridge from storyline quadrants to physical interpretation.


# ============================================================================
# 6) SIGN ROBUSTNESS ACROSS MEMBERS
# ----------------------------------------------------------------------------
# What we ask:
#   How consistent is the sign of change across ensemble members?
#
# What we look at:
#   - probability of positive or negative absolute change
#   - probability of positive or negative Sen slope
#
# What it may imply:
#   - q50 alone can be misleading if probabilities are near 0.5
#   - high sign probability indicates directional robustness
# ============================================================================

sign_robust <- region_quad[
  ,
  .(
    scenario,
    region,
    storyline_q50,
    pr_prec_abs_gt0,
    pr_evap_abs_gt0,
    pr_avail_abs_gt0,
    pr_flux_abs_gt0,
    pr_prec_sen_gt0,
    pr_evap_sen_gt0,
    pr_avail_sen_gt0,
    pr_flux_sen_gt0
  )
]

fwrite(sign_robust, file.path(out_dir, "06_region_sign_robustness.csv"))

f_avail_sign <- ggplot(sign_robust, aes(x = reorder(region, pr_avail_abs_gt0), 
                                        y = pr_avail_abs_gt0, fill = storyline_q50)) +
  geom_col() +
  facet_wrap(~ scenario) +
  labs(
    title = "Regional probability that availability change is positive",
    x = "Region",
    y = "Pr[Δ(P-E) > 0]"
  ) +
  theme_eda() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(file.path(out_dir, "06_pr_avail_positive.png"), f_avail_sign, width = 14, height = 9, dpi = 300)

f_flux_sign <- ggplot(sign_robust, aes(x = reorder(region, pr_flux_abs_gt0), 
                                       y = pr_flux_abs_gt0, fill = storyline_q50)) +
  geom_col() +
  facet_wrap(~ scenario) +
  labs(
    title = "Regional probability that flux change is positive",
    x = "Region",
    y = "Pr[Δ(P+E)/2 > 0]"
  ) +
  theme_eda() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(file.path(out_dir, "06_pr_flux_positive.png"), f_flux_sign, width = 14, height = 9, dpi = 300)

# Interpretation notes:
# - Regions with q50 > 0 but probability close to 0.5 are weakly robust.
# - Regions with probability > 0.8 or < 0.2 are good candidates for stronger
#   narrative statements.
# - Compare absolute-change sign probabilities with Sen sign probabilities:
#   differences may reveal step shifts versus monotonic trends.


# ============================================================================
# 7) SIGNIFICANCE ROBUSTNESS ACROSS MEMBERS
# ----------------------------------------------------------------------------
# What we ask:
#   Is the signal only directionally consistent, or also statistically robust
#   across members?
#
# What we look at:
#   - probability of significant MK trend
#   - probability of significant positive / negative trends
#
# What it may imply:
#   - high sign robustness but low significance robustness: consistent but weak
#     signal
#   - high significance robustness: stronger evidence for a persistent signal
# ============================================================================

sig_robust <- region_quad[
  ,
  .(
    scenario,
    region,
    storyline_q50,
    pr_prec_mk_sig,
    pr_evap_mk_sig,
    pr_avail_mk_sig,
    pr_flux_mk_sig,
    pr_prec_sig_pos,
    pr_evap_sig_pos,
    pr_avail_sig_pos,
    pr_flux_sig_pos,
    pr_prec_sig_neg,
    pr_evap_sig_neg,
    pr_avail_sig_neg,
    pr_flux_sig_neg
  )
]

fwrite(sig_robust, file.path(out_dir, "07_region_significance_robustness.csv"))

f_sig_avail <- ggplot(sig_robust, aes(x = reorder(region, pr_avail_mk_sig), y = pr_avail_mk_sig, fill = storyline_q50)) +
  geom_col() +
  facet_wrap(~ scenario) +
  labs(
    title = "Regional probability of significant availability trend",
    x = "Region",
    y = "Pr[MK significant for P-E]"
  ) +
  theme_eda() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(file.path(out_dir, "07_pr_avail_significant.png"), f_sig_avail, width = 14, height = 9, dpi = 300)

f_sig_flux <- ggplot(sig_robust, aes(x = reorder(region, pr_flux_mk_sig), y = pr_flux_mk_sig, 
                                     fill =storyline_q50)) +
  geom_col() +
  facet_wrap(~ scenario) +
  labs(
    title = "Regional probability of significant flux trend",
    x = "Region",
    y = "Pr[MK significant for (P+E)/2]"
  ) +
  theme_eda() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(file.path(out_dir, "07_pr_flux_significant.png"), f_sig_flux, width = 14, height = 9, dpi = 300)

# Interpretation notes:
# - This section is useful for separating "suggestive storyline regions" from
#   "strong evidence storyline regions".
# - Do not over-interpret low significance robustness as absence of change. It
#   may simply reflect member variability or short time-series length.
# - Strong sign agreement with weak significance often still matters for
#   storyline construction, but should be framed cautiously.


# ============================================================================
# 8) DATASET DEPENDENCE VERSUS ENSEMBLE BEHAVIOR
# ----------------------------------------------------------------------------
# What we ask:
#   Are soft-scenario regional medians representative of the underlying datasets,
#   or do datasets occupy very different quadrants?
#
# What we look at:
#   - dataset-level quadrant diversity
#   - comparison of dataset spread versus soft scenario summary
#
# What it may imply:
#   - wide dataset spread means results depend strongly on product selection
#   - agreement between datasets and soft summary suggests weighting is not
#     masking major structural conflict
# ============================================================================

dataset_region2 <- copy(dataset_region)
dataset_region2[, dataset_storyline := classify_quadrant(avail_abs_change, flux_abs_change)]

dataset_quad_diversity <- dataset_region2[
  ,
  .(
    n_dataset = .N,
    n_storyline_types = uniqueN(dataset_storyline),
    datasets = paste(sort(dataset), collapse = ", ")
  ),
  by = .(region)
][order(-n_storyline_types, -n_dataset)]

fwrite(dataset_quad_diversity, file.path(out_dir, "08_dataset_quadrant_diversity.csv"))

print(get_top_n(dataset_quad_diversity, "n_storyline_types", n = 20))

dataset_compare <- merge(
  dataset_region2[, .(dataset, region, dataset_storyline, dataset_avail = avail_abs_change, dataset_flux = flux_abs_change)],
  scenario_region[, .(scenario, region, soft_avail = avail_abs_q50, soft_flux = flux_abs_q50, modal_storyline, storyline_entropy)],
  by = "region",
  allow.cartesian = TRUE
)

fwrite(dataset_compare, file.path(out_dir, "08_dataset_vs_soft_region.csv"))

f_dataset_vs_soft <- ggplot(dataset_compare, aes(x = dataset_avail, y = dataset_flux)) +
  plot_quadrant_background() +
  geom_point(alpha = 0.6) +
  geom_point(aes(x = soft_avail, y = soft_flux), shape = 4, size = 3, linewidth = 1) +
  facet_wrap(~ scenario) +
  labs(
    title = "Dataset-level regional points versus soft scenario medians",
    subtitle = "Points = datasets, cross = soft median per region",
    x = "Availability change",
    y = "Flux change"
  ) +
  theme_eda()

ggsave(file.path(out_dir, "08_dataset_vs_soft_scatter.png"), f_dataset_vs_soft, width = 12, height = 8, dpi = 300)

# Interpretation notes:
# - Regions with many dataset storyline types are sensitive to product choice.
# - If the soft median lies far from the cloud center, weighting may be
#   reshaping the narrative.
# - If dataset spread is much larger than scenario differences, then dataset
#   epistemic uncertainty dominates storyline uncertainty.


# ============================================================================
# 9) STABLE VS FRAGILE REGION RANKING
# ----------------------------------------------------------------------------
# What we ask:
#   Which regions are robust enough to emphasize, and which are fragile enough
#   to qualify carefully?
#
# What we use:
#   - signal magnitude
#   - uncertainty width
#   - modal storyline probability
#   - entropy
#   - soft vs top1 agreement
#
# What it may imply:
#   - stable regions support stronger scientific claims
#   - fragile regions are useful as uncertainty case studies
# ============================================================================

stable_fragile <- merge(
  region_quad[
    ,
    .(
      scenario,
      region,
      avail_abs_q05, avail_abs_q50, avail_abs_q95,
      flux_abs_q05, flux_abs_q50, flux_abs_q95,
      modal_storyline,
      modal_storyline_prob,
      storyline_entropy
    )
  ],
  scenario_vs_top1_region[
    ,
    .(
      scenario,
      region,
      top1_soft_storyline_agree,
      avail_top1_minus_soft_q50,
      flux_top1_minus_soft_q50
    )
  ],
  by = c("scenario", "region"),
  all.x = TRUE
)

stable_fragile[, avail_range := avail_abs_q95 - avail_abs_q05]
stable_fragile[, flux_range  := flux_abs_q95 - flux_abs_q05]
stable_fragile[, signal_mag  := sqrt(avail_abs_q50^2 + flux_abs_q50^2)]
stable_fragile[, top1_distance := sqrt(avail_top1_minus_soft_q50^2 + flux_top1_minus_soft_q50^2)]

# heuristic robustness score: higher is more robust
stable_fragile[, robustness_score :=
                 rescale01(signal_mag) +
                 rescale01(modal_storyline_prob) +
                 rescale01(as.numeric(top1_soft_storyline_agree)) -
                 rescale01(storyline_entropy) -
                 rescale01(avail_range) -
                 rescale01(flux_range) -
                 rescale01(top1_distance)
]

stable_fragile <- stable_fragile[order(-robustness_score)]

fwrite(stable_fragile, file.path(out_dir, "09_stable_vs_fragile_region_ranking.csv"))

most_stable  <- stable_fragile[1:20]
most_fragile <- stable_fragile[.N - 19:.N]

print(most_stable)
print(most_fragile)

# Interpretation notes:
# - This is not a formal metric, only a prioritization aid.
# - Regions with large signal, low uncertainty, low entropy, and top1-soft
#   agreement are good candidates for "headline" examples.
# - Regions with low score are where your conclusions are most contingent on
#   weighting or member selection.


# ============================================================================
# 10) TIME-SERIES INSPECTION PLAN FOR ARCHETYPE REGIONS
# ----------------------------------------------------------------------------
# What we ask:
#   Are the summaries representing the actual yearly behavior well?
#
# What we do:
#   Select a few archetype regions:
#     - robust wetter-accelerated
#     - robust drier-accelerated
#     - high-entropy region
#     - soft/top1 disagreement region
#
# What it may imply:
#   - validates whether mean-period changes and Sen slope tell a coherent story
#   - helps detect step changes, reversals, outlier years, or non-monotonicity
# ============================================================================

# Choose archetypes heuristically ---------------------------------------------

arch_robust_wetter_acc <- stable_fragile[
  modal_storyline == "wetter-accelerated"
][order(-robustness_score)][1]

arch_robust_drier_acc <- stable_fragile[
  modal_storyline == "drier-accelerated"
][order(-robustness_score)][1]

arch_high_entropy <- stable_fragile[order(-storyline_entropy)][1]

arch_top1_disagree <- stable_fragile[
  top1_soft_storyline_agree == FALSE
][order(-top1_distance)][1]

archetypes <- rbindlist(list(
  if (!is.null(arch_robust_wetter_acc)) data.table(type = "robust_wetter_accelerated", arch_robust_wetter_acc) else NULL,
  if (!is.null(arch_robust_drier_acc)) data.table(type = "robust_drier_accelerated", arch_robust_drier_acc) else NULL,
  if (!is.null(arch_high_entropy))      data.table(type = "high_entropy", arch_high_entropy) else NULL,
  if (!is.null(arch_top1_disagree))     data.table(type = "top1_disagreement", arch_top1_disagree) else NULL
), fill = TRUE)

fwrite(archetypes, file.path(out_dir, "10_archetype_regions.csv"))
print(archetypes)

# Helper: prepare yearly long table ------------------------------------------

prep_yearly_long_region <- function(region_name, scenario_name = NULL) {
  out <- list()
  
  # datasets
  d1 <- copy(dataset_region_yearly[region == region_name])
  if (nrow(d1) > 0) {
    d1[, source_type := "dataset"]
    d1[, source_id := as.character(dataset)]
    out[["dataset"]] <- d1[, .(year, region, source_type, source_id, prec, evap)]
  }
  
  # ensemble members
  if (!is.null(scenario_name)) {
    d2 <- copy(ensemble_region_yearly[region == region_name & scenario == scenario_name])
    if (nrow(d2) > 0) {
      d2[, source_type := "mc"]
      d2[, source_id := paste0("sim_", sim_id)]
      out[["mc"]] <- d2[, .(year, region, source_type, source_id, prec, evap)]
    }
    
    d3 <- copy(scenario_region_yearly[region == region_name & scenario %in% c(scenario_name, paste0(scenario_name, "_top1"))])
    if (nrow(d3) > 0) {
      d3[, source_type := fifelse(grepl("_top1$", scenario), "top1", "scenario")]
      d3[, source_id := scenario]
      out[["scenario"]] <- d3[, .(year, region, source_type, source_id, prec, evap)]
    }
  }
  
  ans <- rbindlist(out, fill = TRUE)
  ans[, avail := prec - evap]
  ans[, flux  := (prec + evap) / 2]
  ans[]
}

plot_region_timeseries <- function(region_name, scenario_name, variable = "avail") {
  dt <- prep_yearly_long_region(region_name, scenario_name)
  if (nrow(dt) == 0) return(NULL)
  
  ggplot(dt, aes(x = year, y = get(variable), group = source_id, color = source_type)) +
    geom_line(alpha = 0.5) +
    labs(
      title = paste("Yearly", variable, "for region", region_name, "under", scenario_name),
      x = "Year",
      y = variable,
      color = "Source"
    ) +
    theme_eda()
}

# Produce timeseries plots for archetypes ------------------------------------

if (nrow(archetypes) > 0) {
  for (i in seq_len(nrow(archetypes))) {
    reg <- archetypes$region[i]
    scn <- archetypes$scenario[i]
    typ <- archetypes$type[i]
    
    for (var in c("prec", "evap", "avail", "flux")) {
      f_ts <- plot_region_timeseries(reg, scn, var)
      if (!is.null(f_ts)) {
        ggsave(
          file.path(out_dir, paste0("10_timeseries_", typ, "_", reg, "_", scn, "_", var, ".png")),
          f_ts,
          width = 11,
          height = 6,
          dpi = 300
        )
      }
    }
  }
}

# Interpretation notes:
# - These plots are crucial before any final narrative.
# - You may find that some "robust" mean changes are caused by a few extreme
#   years or by step shifts around 2001.
# - You may also find trend reversals or nonlinear behavior not visible in the
#   summary tables.
# - This step often determines whether you trust the storyline interpretation
#   as a genuine change signal or as an artifact of the chosen metric.


# ============================================================================
# OPTIONAL SYNTHESIS TABLE
# ----------------------------------------------------------------------------
# Compact table for manuscript notes or discussion drafting.
# ============================================================================

synthesis_table <- merge(
  region_quad[
    ,
    .(
      scenario,
      region,
      storyline_q50,
      modal_storyline,
      modal_storyline_prob,
      storyline_entropy,
      prec_abs_q50,
      evap_abs_q50,
      avail_abs_q50,
      flux_abs_q50,
      pr_avail_abs_gt0,
      pr_flux_abs_gt0,
      pr_avail_mk_sig,
      pr_flux_mk_sig
    )
  ],
  stable_fragile[
    ,
    .(
      scenario,
      region,
      robustness_score,
      top1_soft_storyline_agree,
      top1_distance
    )
  ],
  by = c("scenario", "region"),
  all.x = TRUE
)

fwrite(synthesis_table, file.path(out_dir, "synthesis_table_region_storylines.csv"))

message("EDA script finished.")
message("Outputs saved in: ", out_dir)