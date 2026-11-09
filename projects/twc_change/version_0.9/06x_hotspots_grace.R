# ============================================================
# VALIDATION OF WEIGHTED AVAILABILITY SLOPES AGAINST GRACE TWS
# Base scenario only
# Workflow:
# 1. avail_slope per region x biome x dataset
# 2. merge with dataset weights
# 3. compute weighted / equal / top-dataset slopes at region x biome
# 4. aggregate to region using biome_fraction
# 5. compare regional slopes against GRACE tws_slope
# ============================================================

install.packages("trend")
library(trend)

source("source/twc_change.R")
masks <- pRecipe::pRecipe_masks()

# -----------------------------
# Load data
# -----------------------------
total_water_storage <- readRDS(paste0(PATH_OUTPUT_RAW, "other/grace_yearly_2019.Rds"))
avail_flux          <- readRDS(paste0(PATH_OUTPUT, "avail_flux_year.Rds"))
dataset_weights     <- readRDS(paste0(PATH_OUTPUT_DATA, "weights_region_biome.Rds"))
water_energy_limited <- readRDS(file.path(PATH_OUTPUT_DATA, "ipcc_water_energy_limited.Rds"))


# -----------------------------
# Restrict period to GRACE overlap
# -----------------------------
total_water_storage <- total_water_storage[year >= 2002 & year < 2020]
avail_flux          <- avail_flux[year >= 2002 & year < 2020]

# -----------------------------
# Prepare masks: region + biome
# Adjust the biome column name below if needed
# -----------------------------
mask_rb <- masks[land_mask == "land", .(
  lon,
  lat,
  region = ipcc_short_region,
  biome = biome_short_class
)]

# -----------------------------
# Attach region and biome
# -----------------------------
total_water_storage <- merge(
  total_water_storage,
  mask_rb[, .(lon, lat, region)],
  by = c("lon", "lat"),
  all.x = FALSE
)

avail_flux <- merge(
  avail_flux,
  mask_rb,
  by = c("lon", "lat"),
  all.x = FALSE
)

# -----------------------------
# Helper for Sen slope
# -----------------------------
sen_fun <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(list(slope = NA_real_, p.value = NA_real_))
  }
  
  fit <- sens.slope(x, y)
  list(slope = as.numeric(fit$estimates), p.value = as.numeric(fit$p.value))
}

# ============================================================
# STEP 1
# avail_slope per region x biome x dataset
# ============================================================

# First compute yearly mean availability within each region x biome x dataset
avail_region_biome_year <- avail_flux[, .(
  avail = mean(avail, na.rm = TRUE)
), by = .(year, region, biome, dataset)]

# Then estimate Sen slope through time
avail_slopes_rb <- avail_region_biome_year[, {
  fit <- sen_fun(avail, year)
  .(avail_slope = fit$slope, p.value = fit$p.value)
}, by = .(region, biome, dataset)]

# Add scenario = "base" to match weights table
avail_slopes_rb[, scenario := "base"]

# ============================================================
# STEP 2
# Merge with weights
# ============================================================

weights_base <- copy(dataset_weights)[scenario == "base"]

slopes_w <- merge(
  weights_base,
  avail_slopes_rb,
  by = c("scenario", "region", "biome", "dataset"),
  all.x = TRUE
)

# Optional check
# slopes_w[is.na(avail_slope)]

# For biome_fraction, use one value per region x biome
# Since slight differences may exist across datasets, take the mean
biome_frac_rb <- slopes_w[, .(
  biome_fraction = mean(biome_fraction, na.rm = TRUE)
), by = .(region, biome)]

# ============================================================
# STEP 3
# Within each scenario x region x biome compute:
# weighted ensemble slope
# equal-weight slope
# top-weight dataset slope
# Also validation-support metrics at region-biome level
# ============================================================

ensemble_rb <- slopes_w[, {
  dt <- .SD[is.finite(avail_slope) & is.finite(w_region_biome)]
  
  if (nrow(dt) == 0) {
    .(
      weighted_slope = NA_real_,
      equal_slope = NA_real_,
      top_slope = NA_real_,
      top_dataset = NA_character_,
      top_weight = NA_real_,
      n_datasets_used = 0L
    )
  } else {
    top_i <- which.max(dt$w_region_biome)
    
    .(
      weighted_slope = sum(dt$avail_slope * dt$w_region_biome, na.rm = TRUE) /
        sum(dt$w_region_biome, na.rm = TRUE),
      equal_slope = mean(dt$avail_slope, na.rm = TRUE),
      top_slope = dt$avail_slope[top_i],
      top_dataset = as.character(dt$dataset[top_i]),
      top_weight = dt$w_region_biome[top_i],
      n_datasets_used = nrow(dt)
    )
  }
}, by = .(scenario, region, biome)]

ensemble_rb <- merge(
  ensemble_rb,
  biome_frac_rb,
  by = c("region", "biome"),
  all.x = TRUE
)

# Normalize biome fractions within each region
ensemble_rb[, biome_fraction_norm := biome_fraction / sum(biome_fraction, na.rm = TRUE), by = region]

# ============================================================
# GRACE regional slope
# ============================================================

tws_means_ipcc <- total_water_storage[, .(
  value = mean(value, na.rm = TRUE)
), by = .(year, region)]

tws_slopes_ipcc <- tws_means_ipcc[, {
  fit <- sen_fun(value, year)
  .(tws_slope = fit$slope, tws_p.value = fit$p.value)
}, by = region]

# ============================================================
# Add GRACE sign at region-biome level for agreement diagnostics
# We compare region-biome product slopes against regional GRACE sign
# ============================================================

ensemble_rb <- merge(
  ensemble_rb,
  tws_slopes_ipcc,
  by = "region",
  all.x = TRUE
)

sign_agree <- function(x, y) {
  if (!is.finite(x) || !is.finite(y) || x == 0 || y == 0) return(NA)
  sign(x) == sign(y)
}

# Weight mass on agreeing datasets within each region-biome
agree_mass_rb <- merge(
  slopes_w,
  tws_slopes_ipcc[, .(region, tws_slope)],
  by = "region",
  all.x = TRUE
)

agree_mass_rb[, agree_with_grace := mapply(sign_agree, avail_slope, tws_slope)]

agree_mass_rb_summary <- agree_mass_rb[, {
  dt <- .SD[is.finite(w_region_biome)]
  .(
    weight_mass_agree = sum(w_region_biome[agree_with_grace %in% TRUE], na.rm = TRUE),
    weight_mass_disagree = sum(w_region_biome[agree_with_grace %in% FALSE], na.rm = TRUE)
  )
}, by = .(scenario, region, biome)]

ensemble_rb <- merge(
  ensemble_rb,
  agree_mass_rb_summary,
  by = c("scenario", "region", "biome"),
  all.x = TRUE
)

ensemble_rb[, weighted_sign_agree := mapply(sign_agree, weighted_slope, tws_slope)]
ensemble_rb[, equal_sign_agree    := mapply(sign_agree, equal_slope,    tws_slope)]
ensemble_rb[, top_sign_agree      := mapply(sign_agree, top_slope,      tws_slope)]

# ============================================================
# STEP 4
# Aggregate region x biome -> region using biome_fraction
# ============================================================

regional_summary <- ensemble_rb[, .(
  weighted_slope = sum(weighted_slope * biome_fraction_norm, na.rm = TRUE),
  equal_slope    = sum(equal_slope    * biome_fraction_norm, na.rm = TRUE),
  top_slope      = sum(top_slope      * biome_fraction_norm, na.rm = TRUE),
  
  # aggregated validation-support metrics
  weight_mass_agree = sum(weight_mass_agree * biome_fraction_norm, na.rm = TRUE),
  weight_mass_disagree = sum(weight_mass_disagree * biome_fraction_norm, na.rm = TRUE)
), by = .(scenario, region)]

regional_summary <- merge(
  regional_summary,
  tws_slopes_ipcc,
  by = "region",
  all.x = TRUE
)

# ============================================================
# STEP 5
# Compare regional slopes to GRACE
# Strongest practical validation set:
# 1. weighted ensemble sign vs GRACE
# 2. weight mass on agreeing datasets
# 3. top dataset agreement
# ============================================================

regional_summary[, weighted_sign_agree := mapply(sign_agree, weighted_slope, tws_slope)]
regional_summary[, equal_sign_agree    := mapply(sign_agree, equal_slope,    tws_slope)]
regional_summary[, top_sign_agree      := mapply(sign_agree, top_slope,      tws_slope)]

regional_summary[, grace_sign := fifelse(
  tws_slope > 0, "positive",
  fifelse(tws_slope < 0, "negative", NA_character_)
)]

regional_summary[, weighted_sign := fifelse(
  weighted_slope > 0, "positive",
  fifelse(weighted_slope < 0, "negative", NA_character_)
)]

regional_summary[, top_sign := fifelse(
  top_slope > 0, "positive",
  fifelse(top_slope < 0, "negative", NA_character_)
)]

# ============================================================
# Final validation tables
# ============================================================

# Main table
validation_table <- regional_summary[, .(
  scenario,
  region,
  tws_slope,
  weighted_slope,
  equal_slope,
  top_slope,
  weighted_sign_agree,
  equal_sign_agree,
  top_sign_agree,
  weight_mass_agree,
  weight_mass_disagree
)]

# Summary metrics across regions
validation_metrics <- validation_table[, .(
  n_regions = .N,
  weighted_accuracy = mean(weighted_sign_agree, na.rm = TRUE),
  equal_accuracy    = mean(equal_sign_agree,    na.rm = TRUE),
  top_accuracy      = mean(top_sign_agree,      na.rm = TRUE),
  mean_agree_mass   = mean(weight_mass_agree,   na.rm = TRUE),
  median_agree_mass = median(weight_mass_agree, na.rm = TRUE)
), by = scenario]

# ============================================================
# Optional: classify strength of support per region
# ============================================================

validation_table[, support_class := fifelse(
  weighted_sign_agree %in% TRUE & top_sign_agree %in% TRUE & weight_mass_agree >= 0.60,
  "strong_support",
  fifelse(
    weighted_sign_agree %in% TRUE & (top_sign_agree %in% TRUE | weight_mass_agree >= 0.50),
    "moderate_support",
    fifelse(weighted_sign_agree %in% FALSE, "contradicted", "mixed")
  )
)]

# ============================================================
# Save outputs
# ============================================================

saveRDS(avail_slopes_rb,        paste0(PATH_OUTPUT_DATA, "avail_slopes_region_biome_base.Rds"))
saveRDS(slopes_w,               paste0(PATH_OUTPUT_DATA, "avail_slopes_weights_merged_base.Rds"))
saveRDS(ensemble_rb,            paste0(PATH_OUTPUT_DATA, "ensemble_region_biome_base.Rds"))
saveRDS(tws_slopes_ipcc,        paste0(PATH_OUTPUT_DATA, "tws_slopes_ipcc.Rds"))
saveRDS(validation_table,       paste0(PATH_OUTPUT_DATA, "validation_table_base_vs_grace.Rds"))
saveRDS(validation_metrics,     paste0(PATH_OUTPUT_DATA, "validation_metrics_base_vs_grace.Rds"))

# ============================================================
# Quick inspection
# ============================================================

print(validation_metrics)
print(validation_table[order(region)])

# Regions where weighted ensemble agrees with GRACE
validation_table[weighted_sign_agree == TRUE][order(-weight_mass_agree)]

# Regions where weighted ensemble disagrees with GRACE
validation_table[weighted_sign_agree == FALSE][order(weight_mass_agree)]

make a clean copy
vt <- copy(validation_table)

# convert logicals safely to numeric summaries
summary_metrics <- vt[, .(
  n_regions = .N,
  weighted_agree_rate = mean(weighted_sign_agree, na.rm = TRUE),
  equal_agree_rate    = mean(equal_sign_agree, na.rm = TRUE),
  top_agree_rate      = mean(top_sign_agree, na.rm = TRUE),
  mean_weight_mass_agree   = mean(weight_mass_agree, na.rm = TRUE),
  median_weight_mass_agree = median(weight_mass_agree, na.rm = TRUE)
)]

print(summary_metrics)

# how often weighted improves over equal
improvement_metrics <- vt[, .(
  weighted_better_than_equal = mean(weighted_sign_agree == TRUE & equal_sign_agree == FALSE, na.rm = TRUE),
  equal_better_than_weighted = mean(weighted_sign_agree == FALSE & equal_sign_agree == TRUE, na.rm = TRUE),
  both_agree = mean(weighted_sign_agree == TRUE & equal_sign_agree == TRUE, na.rm = TRUE),
  both_disagree = mean(weighted_sign_agree == FALSE & equal_sign_agree == FALSE, na.rm = TRUE)
)]

print(improvement_metrics)

vt_plot <- copy(vt)

vt_plot[, support_class := factor(
  support_class,
  levels = c("strong_support", "moderate_support", "contradicted")
)]

vt_plot <- vt_plot[order(weight_mass_agree)]
vt_plot[, region := factor(region, levels = region)]

ggplot(vt_plot, aes(x = region, y = weight_mass_agree, fill = support_class)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0.5, linetype = 2, linewidth = 0.4) +
  coord_flip() +
  scale_fill_manual(values = c(
    strong_support   = "#1b9e77",
    moderate_support = "#e6ab02",
    contradicted     = "#d95f02"
  )) +
  labs(
    x = NULL,
    y = "Weight mass on datasets agreeing with GRACE sign",
    fill = NULL,
    title = "Regional physical support of weighted availability trends",
    subtitle = "Agreement assessed against GRACE TWS trend sign"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

acc_dt <- data.table(
  method = c("Weighted ensemble", "Equal weight", "Top-weight dataset"),
  agreement_rate = c(
    mean(vt$weighted_sign_agree, na.rm = TRUE),
    mean(vt$equal_sign_agree, na.rm = TRUE),
    mean(vt$top_sign_agree, na.rm = TRUE)
  )
)

ggplot(acc_dt, aes(x = method, y = agreement_rate)) +
  geom_col(width = 0.7, fill = "steelblue") +
  ylim(0, 1) +
  geom_text(aes(label = sprintf("%.2f", agreement_rate)), vjust = -0.4, size = 4) +
  labs(
    x = NULL,
    y = "Fraction of regions agreeing with GRACE sign",
    title = "Does weighting improve regional agreement with GRACE?"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )


heat_dt <- melt(
  vt[, .(region, weighted_sign_agree, equal_sign_agree, top_sign_agree)],
  id.vars = "region",
  variable.name = "method",
  value.name = "agree"
)

heat_dt[, method := factor(method,
                           levels = c("weighted_sign_agree", "equal_sign_agree", "top_sign_agree"),
                           labels = c("Weighted ensemble", "Equal weight", "Top dataset")
)]

heat_dt[, agree_lab := fifelse(
  agree == TRUE, "Agree",
  fifelse(agree == FALSE, "Disagree", NA_character_)
)]

# order regions by support
region_order <- vt[order(weight_mass_agree), region]
heat_dt[, region := factor(region, levels = region_order)]

ggplot(heat_dt[!is.na(agree_lab)], aes(x = method, y = region, fill = agree_lab)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c(
    Agree = "#1b9e77",
    Disagree = "#d95f02"
  )) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Regional agreement with GRACE by method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )


ggplot(vt, aes(x = tws_slope, y = weighted_slope, color = support_class)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text(aes(label = region), nudge_y = 0.02, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c(
    strong_support   = "#1b9e77",
    moderate_support = "#e6ab02",
    contradicted     = "#d95f02"
  )) +
  labs(
    x = "GRACE TWS slope",
    y = "Weighted availability slope",
    color = NULL,
    title = "Weighted availability trends versus independent GRACE storage trends"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )



# already available
water_energy_limited <- readRDS(file.path(PATH_OUTPUT_DATA, "ipcc_water_energy_limited.Rds"))
water_energy_limited <- as.data.table(water_energy_limited)

# keep only needed columns and make sure names/types are clean
water_energy_limited <- unique(
  water_energy_limited[, .(region, limited)]
)

water_energy_limited[, region  := as.character(region)]
water_energy_limited[, limited := factor(as.character(limited), levels = c("energy", "water"))]

# validation table
vt_plot <- copy(vt)
vt_plot[, region := as.character(region)]

vt_plot[, support_class := factor(
  support_class,
  levels = c("strong_support", "moderate_support", "contradicted")
)]

# merge limitation regime
vt_plot <- merge(vt_plot, water_energy_limited, by = "region", all.x = TRUE)

# optional check for missing classifications
print(vt_plot[is.na(limited), unique(region)])

# order separately within each regime
vt_plot_energy <- vt_plot[limited == "energy"][order(weight_mass_agree)]
vt_plot_water  <- vt_plot[limited == "water"][order(weight_mass_agree)]

vt_plot_energy[, region_f := factor(region, levels = region)]
vt_plot_water[,  region_f := factor(region, levels = region)]

vt_plot2 <- rbindlist(list(vt_plot_energy, vt_plot_water), use.names = TRUE)

p_support_regime <- ggplot(vt_plot2, aes(x = region_f, y = weight_mass_agree, fill = support_class)) +
  geom_col(width = 0.82) +
  geom_hline(yintercept = 0.5, linetype = 2, linewidth = 0.45, color = "grey30") +
  coord_flip() +
  facet_grid(limited ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(
    strong_support   = "#1b9e77",
    moderate_support = "#e6ab02",
    contradicted     = "#d95f02"
  )) +
  labs(
    x = NULL,
    y = "Weight mass on datasets agreeing with GRACE sign",
    fill = NULL,
    title = "Regional physical support of weighted availability trends",
    subtitle = "Regions grouped by hydroclimatic limitation regime"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

print(p_support_regime)

vt_plot[, .(
  n_regions = .N,
  mean_agree_mass = mean(weight_mass_agree, na.rm = TRUE),
  weighted_agree_rate = mean(weighted_sign_agree, na.rm = TRUE)
), by = limited]


circ_map <- data.table(
  region = c(
    "WNA","CNA","ENA","NCA","CAR",
    "NWS","NSA","NES","SAM","SWS","SES","SSA",
    "NEU","WCE","EEU","MED",
    "SAH","WAF","CAF","NEAF","SEAF","WSAF","ESAF","MDG",
    "RAR","ARP","TIB",
    "SAS","EAS","SEA",
    "NAU","CAU","EAU","SAU","NZ"
  ),
  
  family6 = c(
    "stormtrack","continental_transition","stormtrack","tropical_margin","tropical_margin",
    "tropical_margin","tropical_margin","tropical_margin","monsoon","dry_subsidence","dry_subsidence","stormtrack",
    "stormtrack","stormtrack","continental_transition","continental_transition",
    "dry_subsidence","monsoon","monsoon","dry_subsidence","tropical_margin","dry_subsidence","tropical_margin","tropical_margin",
    "continental_transition","highland","highland",
    "monsoon","monsoon","monsoon",
    "tropical_margin","dry_subsidence","tropical_margin","dry_subsidence","stormtrack"
  ),
  
  family_detail = c(
    "stormtrack_pacific_margin","mixed_transition","stormtrack_core","mixed_transition","tropical_margin",
    "tropical_margin","tropical_margin","tropical_margin","monsoon_margin","dry_interior","dry_transition","stormtrack_core",
    "stormtrack_core","stormtrack_core","mixed_transition","mixed_transition",
    "dry_desert_core","monsoon_margin","monsoon_margin","dry_transition","tropical_margin","dry_interior","tropical_margin","tropical_margin",
    "mixed_transition","highland","highland",
    "monsoon_core","monsoon_frontal","monsoon_frontal",
    "tropical_margin","dry_interior","tropical_margin","dry_interior","stormtrack_core"
  )
)

circ_map[, family6 := factor(
  family6,
  levels = c(
    "stormtrack",
    "monsoon",
    "dry_subsidence",
    "highland",
    "tropical_margin",
    "continental_transition"
  )
)]

circ_map[, family_detail := factor(
  family_detail,
  levels = c(
    "stormtrack_core",
    "stormtrack_pacific_margin",
    "monsoon_core",
    "monsoon_frontal",
    "monsoon_margin",
    "dry_desert_core",
    "dry_interior",
    "dry_transition",
    "highland",
    "mixed_transition",
    "tropical_margin"
  )
)]

vt_circ <- merge(
  copy(vt),
  circ_map,
  by = "region",
  all.x = TRUE
)

vt_circ[, region := as.character(region)]

circ_summary6 <- vt_circ[, .(
  n_regions = .N,
  mean_agree_mass = mean(weight_mass_agree, na.rm = TRUE),
  median_agree_mass = median(weight_mass_agree, na.rm = TRUE),
  weighted_agree_rate = mean(weighted_sign_agree, na.rm = TRUE),
  top_agree_rate = mean(top_sign_agree, na.rm = TRUE),
  strong_support_rate = mean(support_class == "strong_support", na.rm = TRUE),
  contradicted_rate = mean(support_class == "contradicted", na.rm = TRUE)
), by = family6][order(family6)]

print(circ_summary6)

circ_summary_detail <- vt_circ[, .(
  n_regions = .N,
  mean_agree_mass = mean(weight_mass_agree, na.rm = TRUE),
  median_agree_mass = median(weight_mass_agree, na.rm = TRUE),
  weighted_agree_rate = mean(weighted_sign_agree, na.rm = TRUE),
  strong_support_rate = mean(support_class == "strong_support", na.rm = TRUE),
  contradicted_rate = mean(support_class == "contradicted", na.rm = TRUE)
), by = family_detail][order(family_detail)]

print(circ_summary_detail)

ggplot(circ_summary6, aes(x = family6, y = mean_agree_mass)) +
  geom_col(width = 0.8, fill = "steelblue") +
  geom_hline(yintercept = 0.5, linetype = 2, linewidth = 0.4) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean weight mass agreeing with GRACE",
    title = "GRACE support across circulation families"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

ggplot(circ_summary6, aes(x = family6, y = weighted_agree_rate)) +
  geom_col(width = 0.8, fill = "grey40") +
  ylim(0, 1) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Fraction of regions with weighted sign agreement",
    title = "Regional weighted sign agreement with GRACE by circulation family"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

vt_plot <- copy(vt_circ)

vt_plot[, support_class := factor(
  support_class,
  levels = c("strong_support", "moderate_support", "contradicted")
)]

vt_plot <- vt_plot[!is.na(family6)]

# order separately within each family
vt_plot <- vt_plot[order(family6, weight_mass_agree)]
vt_plot[, region_f := factor(region, levels = region)]

p_circ_support <- ggplot(vt_plot, aes(x = region_f, y = weight_mass_agree, fill = support_class)) +
  geom_col(width = 0.82) +
  geom_hline(yintercept = 0.5, linetype = 2, linewidth = 0.45, color = "grey30") +
  coord_flip() +
  facet_grid(family6 ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(
    strong_support   = "#1b9e77",
    moderate_support = "#e6ab02",
    contradicted     = "#d95f02"
  )) +
  labs(
    x = NULL,
    y = "Weight mass on datasets agreeing with GRACE sign",
    fill = NULL,
    title = "Regional physical support of weighted availability trends",
    subtitle = "Regions grouped by circulation family"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

print(p_circ_support)
