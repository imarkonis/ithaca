# ============================================================
# Plot suite for your MC regional estimates (data.table + ggplot2)
# ============================================================
# Assumed inputs (you likely already have these):
# 1) mc_reg_period:  ipcc_short_region, sim, period, P, E, P_minus_E, P_plus_E
# 2) mc_reg_change:  ipcc_short_region, sim, dP, dE, dP_minus_E, dP_plus_E
# 3) dataset_region_biome: ipcc_short_region, biome, dataset,
#                          w_region_biome_pe_minus, w_region_biome_pe_plus
# 4) biome_area: ipcc_short_region, biome, A_region_biome
# 5) expected_region_weights (optional): ipcc_short_region, dataset, w_region_expected
# 6) (optional) grid-level: dt with lon, lat, ipcc_short_region, biome, area_w and w_cell_*,
#    plus value grid tables for change or period if you want maps.

library(data.table)
library(ggplot2)

# optional packages used by some plots
install.packages(c("ggridges","ggalluvial","patchwork","scales","sf"))
suppressWarnings({
  has_ggridges   <- requireNamespace("ggridges", quietly = TRUE)
  has_ggalluvial <- requireNamespace("ggalluvial", quietly = TRUE)
  has_scales     <- requireNamespace("scales", quietly = TRUE)
})

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
qsum_dt <- function(x, group_cols, value_col,
                    qs = c(0.05, 0.25, 0.50, 0.75, 0.95)) {
  stopifnot(is.data.table(x))
  x[,
    c(
      as.list(quantile(get(value_col), probs = qs, na.rm = TRUE, names = FALSE)),
      list(mean = mean(get(value_col), na.rm = TRUE))
    ),
    by = group_cols
  ][
    ,
    setNames(.SD,
             c(group_cols, paste0("q", sprintf("%02d", qs*100)), "mean"))
  ]
}

ensure_factor_order <- function(dt, region_col = "ipcc_short_region") {
  dt <- copy(dt)
  dt[, (region_col) := factor(get(region_col), levels = sort(unique(get(region_col))))]
  dt
}

# ============================================================
# 1) Ridge plots per region
# ============================================================
plot_ridges <- function(mc, value_col,
                        facet_col = NULL,
                        region_col = "ipcc_short_region",
                        title = NULL) {
  if (!has_ggridges) stop("Package ggridges not installed.")
  mc <- copy(mc)
  mc <- mc[is.finite(get(value_col)) & !is.na(get(value_col))]
  mc <- ensure_factor_order(mc, region_col)
  
  p <- ggplot(mc, aes(x = get(value_col), y = get(region_col))) +
    ggridges::geom_density_ridges(scale = 1.2, rel_min_height = 0.01, alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(x = value_col, y = "IPCC region", title = title) +
    theme_minimal()
  
  if (!is.null(facet_col)) p <- p + facet_wrap(as.formula(paste("~", facet_col)), scales = "free_x")
  p
}

# Example:
plot_ridges(mc_reg_change, "dP_minus_E", title = "Δ(P−E) distributions by region")
# plot_ridges(mc_reg_period[period %in% c("ref","fut")], "P_minus_E", facet_col="period")

# ============================================================
# 2) Interval forest plot (median, IQR, 5-95)
# ============================================================
plot_interval_forest <- function(mc, value_col,
                                 facet_col = NULL,
                                 region_col = "ipcc_short_region",
                                 title = NULL) {
  
  mc <- copy(mc)
  mc <- mc[is.finite(get(value_col)) & !is.na(get(value_col))]
  group_cols <- c(region_col, if (!is.null(facet_col)) facet_col)
  s <- qsum_dt(mc, group_cols, value_col)
  s <- ensure_factor_order(s, region_col)
  
  # add sign of median for optional aesthetics
  s[, med := q50]
  s[, sign_med := fifelse(med > 0, "pos", fifelse(med < 0, "neg", "zero"))]
  
  p <- ggplot(s, aes(y = get(region_col))) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbarh(aes(xmin = q05, xmax = q95), height = 0.15, alpha = 0.7) +
    geom_errorbarh(aes(xmin = q25, xmax = q75), height = 0.35, linewidth = 1.2, alpha = 0.9) +
    geom_point(aes(x = q50, shape = sign_med), size = 2.4) +
    labs(x = value_col, y = "IPCC region", title = title) +
    theme_minimal()
  
  if (!is.null(facet_col)) p <- p + facet_wrap(as.formula(paste("~", facet_col)), scales = "free_x")
  p
}

# Example:
plot_interval_forest(mc_reg_change, "dP_minus_E", title="Δ(P−E) by region (MC)")
# plot_interval_forest(mc_reg_change, "dP_plus_E",  title="Δ(P+E) by region (MC)")

# ============================================================
# 3) Joint uncertainty scatter: ΔP vs ΔE (with Δ(P−E)=0 line)
# ============================================================
plot_joint_scatter <- function(mc_change,
                               xcol = "dP", ycol = "dE",
                               region_col = "ipcc_short_region",
                               facet_ncol = 4,
                               title = NULL) {
  mc <- copy(mc_change)
  mc <- mc[is.finite(get(xcol)) & is.finite(get(ycol))]
  mc <- ensure_factor_order(mc, region_col)
  
  p <- ggplot(mc, aes(x = get(xcol), y = get(ycol))) +
    geom_point(alpha = 0.15, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +  # y = x -> dP - dE = 0
    labs(x = xcol, y = ycol, title = title) +
    theme_minimal() +
    facet_wrap(as.formula(paste("~", region_col)), ncol = facet_ncol, scales = "free")
  p
}

# Example:
plot_joint_scatter(mc_reg_change, title="Joint MC uncertainty: ΔP vs ΔE")

# ============================================================
# 4) Quadrant probability bars (sign classes)
# ============================================================
make_quadrant_probs <- function(mc_change,
                                xcol = "dP", ycol = "dE",
                                region_col = "ipcc_short_region",
                                facet_col = NULL) {
  mc <- copy(mc_change)
  mc <- mc[is.finite(get(xcol)) & is.finite(get(ycol))]
  mc[, prec_sign := fifelse(get(xcol) >= 0, "prec_pos", "prec_neg")]
  mc[, evap_sign := fifelse(get(ycol) >= 0, "evap_pos", "evap_neg")]
  mc[, quadrant := paste0(prec_sign, "-", evap_sign)]
  
  group_cols <- c(region_col, if (!is.null(facet_col)) facet_col, "quadrant")
  out <- mc[, .N, by = group_cols]
  denom_cols <- c(region_col, if (!is.null(facet_col)) facet_col)
  out[, p := N / sum(N), by = denom_cols]
  out[]
}

plot_quadrant_bars <- function(mc_change,
                               xcol = "dP", ycol = "dE",
                               region_col = "ipcc_short_region",
                               facet_col = NULL,
                               title = NULL) {
  qp <- make_quadrant_probs(mc_change, xcol, ycol, region_col, facet_col)
  qp <- ensure_factor_order(qp, region_col)
  
  p <- ggplot(qp, aes(x = get(region_col), y = p, fill = quadrant)) +
    geom_col(position = "fill") +
    labs(x = "IPCC region", y = "Probability", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (!is.null(facet_col)) p <- p + facet_wrap(as.formula(paste("~", facet_col)))
  p
}

# Example:
plot_quadrant_bars(mc_reg_change, title="Quadrant probabilities by region (ΔP, ΔE)")

# ============================================================
# 5) Violin + box overlay
# ============================================================
plot_violin_box <- function(mc, value_col,
                            facet_col = NULL,
                            region_col = "ipcc_short_region",
                            title = NULL) {
  mc <- copy(mc)
  mc <- mc[is.finite(get(value_col)) & !is.na(get(value_col))]
  mc <- ensure_factor_order(mc, region_col)
  
  p <- ggplot(mc, aes(x = get(region_col), y = get(value_col))) +
    geom_violin(alpha = 0.7, trim = TRUE) +
    geom_boxplot(width = 0.12, outlier.alpha = 0.05) +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(x = "IPCC region", y = value_col, title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (!is.null(facet_col)) p <- p + facet_wrap(as.formula(paste("~", facet_col)), scales = "free_y")
  p
}

# Example:
plot_violin_box(mc_reg_change, "dP_minus_E", title="Δ(P−E) distribution by region")

# ============================================================
# 6) Biome contribution heatmaps
# ============================================================
# (a) w_{region,biome}(dataset) heatmap for a region
plot_heatmap_region_biome_dataset <- function(dataset_region_biome,
                                              region = "MED",
                                              prob_col = "w_region_biome_pe_minus",
                                              title = NULL) {
  x <- copy(dataset_region_biome)
  x <- x[ipcc_short_region == region, .(biome, dataset, w = get(prob_col))]
  x <- x[is.finite(w) & !is.na(w)]
  
  ggplot(x, aes(x = dataset, y = biome, fill = w)) +
    geom_tile() +
    labs(x = "Dataset", y = "Biome", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# (b) effective contribution heatmap: A_{region,biome} * w_{region,biome}(dataset)
plot_heatmap_effective_contrib <- function(dataset_region_biome, biome_area,
                                           region = "MED",
                                           prob_col = "w_region_biome_pe_minus",
                                           title = NULL) {
  x <- merge(
    dataset_region_biome[ipcc_short_region == region,
                         .(ipcc_short_region, biome, dataset, w = get(prob_col))],
    biome_area[ipcc_short_region == region, .(ipcc_short_region, biome, A_region_biome)],
    by = c("ipcc_short_region","biome"),
    all.x = TRUE
  )
  x[, eff := w * A_region_biome]
  x <- x[is.finite(eff) & !is.na(eff)]
  
  ggplot(x, aes(x = dataset, y = biome, fill = eff)) +
    geom_tile() +
    labs(x = "Dataset", y = "Biome", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example:
plot_heatmap_region_biome_dataset(dataset_region_biome, "MED", "w_region_biome",
                                 title="MED: biome-specific dataset probabilities")
plot_heatmap_effective_contrib(dataset_region_biome, biome_area, "MED", "w_region_biome",
                                title="MED: effective contribution A×w")

# ============================================================
# 7) Alluvial (biome area -> dataset choice weights)
# ============================================================
plot_alluvial_region <- function(dataset_region_biome, biome_area,
                                 region = "MED",
                                 prob_col = "w_region_biome_pe_minus",
                                 title = NULL) {
  if (!has_ggalluvial) stop("Package ggalluvial not installed.")
  
  x <- merge(
    dataset_region_biome[ipcc_short_region == region,
                         .(ipcc_short_region, biome, dataset, w = get(prob_col))],
    biome_area[ipcc_short_region == region, .(ipcc_short_region, biome, A_region_biome)],
    by = c("ipcc_short_region","biome"),
    all.x = TRUE
  )
  x[, eff := w * A_region_biome]
  x <- x[is.finite(eff) & !is.na(eff) & eff > 0]
  
  ggplot(x, aes(axis1 = biome, axis2 = dataset, y = eff)) +
    geom_alluvium(aes(fill = biome), alpha = 0.8) +
    geom_stratum(width = 0.25, alpha = 0.9) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_x_discrete(limits = c("Biome", "Dataset")) +
    labs(title = title, y = "Effective weight (A×w)") +
    theme_minimal()
}

# Example:
plot_alluvial_region(dataset_region_biome, biome_area, "MED", "w_region_biome",
                      title="MED: biome area → dataset mixture")

# ============================================================
# 8) Map of sign agreement / uncertainty (grid level)
# ============================================================
# This needs grid-level MC-derived outputs.
# If you want this, the clean approach is:
# - compute expected change per cell: sum_datasets w_cell * change(dataset)
# - and agreement metric per cell: sum_datasets w_cell * I(change>0)
#
# Below are utilities if you have:
#  - w_cell_dt: lon, lat, dataset, w_cell (or w_cell_pe_minus)
#  - cell_change_dt: lon, lat, dataset, prec_change, evap_change (your prec_evap_change)

map_cell_expected_and_agreement <- function(w_cell_dt, cell_change_dt,
                                            which = c("dP_minus_E","dP_plus_E"),
                                            weight_col = "w_cell_pe_minus") {
  which <- match.arg(which)
  
  x <- merge(
    w_cell_dt[, .(lon, lat, dataset, w = get(weight_col))],
    cell_change_dt[, .(lon, lat, dataset, prec_change, evap_change)],
    by = c("lon","lat","dataset"),
    all.x = TRUE
  )
  
  x[, dP := prec_change]
  x[, dE := evap_change]
  if (which == "dP_minus_E") x[, val := dP - dE]
  if (which == "dP_plus_E")  x[, val := dP + dE]
  
  out <- x[
    ,
    .(
      expected = sum(w * val, na.rm = TRUE),
      p_pos = sum(w * (val > 0), na.rm = TRUE),
      p_neg = sum(w * (val < 0), na.rm = TRUE)
    ),
    by = .(lon, lat)
  ]
  out[]
}

plot_map_simple <- function(cell_map_dt, fill_col = "expected", title = NULL) {
  ggplot(cell_map_dt, aes(x = lon, y = lat, fill = get(fill_col))) +
    geom_raster() +
    coord_equal() +
    labs(title = title, fill = fill_col) +
    theme_minimal()
}

# Example (only if you have w_cell_dt with weights per cell and dataset):
# cell_map <- map_cell_expected_and_agreement(w_cell_dt = dt[,.(lon,lat,dataset,w_cell_pe_minus)],
#                                            cell_change_dt = prec_evap_change,
#                                            which="dP_minus_E",
#                                            weight_col="w_cell_pe_minus")
# plot_map_simple(cell_map, "expected", title="Expected Δ(P−E) per grid cell")
# plot_map_simple(cell_map, "p_pos", title="P(Δ(P−E) > 0) per grid cell")

# ============================================================
# 9) Paired period dumbbell plot (absolute P/E per period)
# ============================================================
# Requires mc_reg_period with two periods (e.g., ref and fut).
plot_dumbbell_periods <- function(mc_reg_period,
                                  value_col = "P_minus_E",
                                  period_ref = "ref",
                                  period_fut = "fut",
                                  region_col = "ipcc_short_region",
                                  title = NULL) {
  mc <- copy(mc_reg_period)
  mc <- mc[period %in% c(period_ref, period_fut) & is.finite(get(value_col))]
  mc <- ensure_factor_order(mc, region_col)
  
  s <- mc[
    ,
    .(
      med = median(get(value_col), na.rm = TRUE),
      q05 = quantile(get(value_col), 0.05, na.rm = TRUE),
      q95 = quantile(get(value_col), 0.95, na.rm = TRUE)
    ),
    by = .(get(region_col), period)
  ]
  setnames(s, "get", region_col)
  
  wide_med <- dcast(s, paste(region_col, "~ period"), value.var = "med")
  wide_q05 <- dcast(s, paste(region_col, "~ period"), value.var = "q05")
  wide_q95 <- dcast(s, paste(region_col, "~ period"), value.var = "q95")
  
  # merge wide
  d <- merge(wide_med, wide_q05, by = region_col, suffixes = c("_med","_q05"))
  d <- merge(d, wide_q95, by = region_col)
  # rename q95 columns after second merge (dcast reused names)
  setnames(d,
           old = c(period_ref, period_fut),
           new = c(paste0(period_ref,"_q95"), paste0(period_fut,"_q95")),
           skip_absent = TRUE)
  
  # build long for plotting
  dd <- rbindlist(list(
    d[, .(ipcc = get(region_col),
          period = period_ref,
          med = get(period_ref),
          q05 = get(paste0(period_ref,"_q05")),
          q95 = get(paste0(period_ref,"_q95")))],
    d[, .(ipcc = get(region_col),
          period = period_fut,
          med = get(period_fut),
          q05 = get(paste0(period_fut,"_q05")),
          q95 = get(paste0(period_fut,"_q95")))]
  ), use.names = TRUE, fill = TRUE)
  
  dd[, ipcc := factor(ipcc, levels = levels(mc[[region_col]]))]
  
  # segment table
  seg <- d[, .(
    ipcc = get(region_col),
    x = get(period_ref),
    xend = get(period_fut)
  )]
  seg[, ipcc := factor(ipcc, levels = levels(mc[[region_col]]))]
  
  ggplot() +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_segment(data = seg, aes(x = x, xend = xend, y = ipcc, yend = ipcc), alpha = 0.8) +
    geom_errorbarh(data = dd, aes(y = ipcc, xmin = q05, xmax = q95), height = 0.15, alpha = 0.6) +
    geom_point(data = dd, aes(y = ipcc, x = med, shape = period), size = 2.2) +
    labs(x = value_col, y = "IPCC region", title = title) +
    theme_minimal()
}

# Example:
 plot_dumbbell_periods(mc_reg_period, value_col="P_minus_E",
                       period_ref="ref", period_fut="fut",
                       title="P−E by region: ref vs fut (median + 5-95)")

# ============================================================
# QUICK “RUN ALL” EXAMPLES (adapt to your objects)
# ============================================================

# Change-focused main figures
# p1 <- plot_interval_forest(mc_reg_change, "dP_minus_E", title="Δ(P−E) by region")
# p2 <- plot_interval_forest(mc_reg_change, "dP_plus_E",  title="Δ(P+E) by region")
# p3 <- plot_joint_scatter(mc_reg_change, title="Joint uncertainty: ΔP vs ΔE")
# p4 <- plot_quadrant_bars(mc_reg_change, title="Quadrant probabilities by region")

# Period-focused distributions (if you have mc_reg_period)
# p5 <- plot_ridges(mc_reg_period[period %in% c("ref","fut")], "P_minus_E", facet_col="period",
#                   title="(P−E) distributions by region and period")
# p6 <- plot_dumbbell_periods(mc_reg_period, value_col="P_plus_E", title="P+E: ref vs fut")

# Mechanism / transparency for one region
# p7 <- plot_heatmap_effective_contrib(dataset_region_biome, biome_area, "MED",
#                                      prob_col="w_region_biome_pe_minus",
#                                      title="MED: A×w (effective dataset contribution)")
# p8 <- plot_alluvial_region(dataset_region_biome, biome_area, "MED",
#                            prob_col="w_region_biome_pe_minus",
#                            title="MED: biome area → dataset mixture")
 

 
 
 
 library(data.table)
 library(ggplot2)
 
 # 1) Build a unique region -> continent lookup from masks
 make_region_continent_lookup <- function(masks,
                                          region_col = "ipcc_short_region",
                                          cont_col   = "ipcc_continent") {
   m <- as.data.table(masks)
   
   # keep only the needed cols, drop empties, then unique
   lut <- unique(
     m[!is.na(get(region_col)) & get(region_col) != "" &
         !is.na(get(cont_col)) & get(cont_col) != "",
       .(ipcc_short_region = get(region_col),
         ipcc_continent    = get(cont_col))]
   )
   
   # guard: if any region maps to multiple continents, choose the most frequent
   # (should not happen, but safe)
   lut <- m[
     !is.na(get(region_col)) & get(region_col) != "" &
       !is.na(get(cont_col)) & get(cont_col) != "",
     .N,
     by = .(ipcc_short_region = get(region_col),
            ipcc_continent = get(cont_col))
   ][order(ipcc_short_region, -N)][, .SD[1], by = ipcc_short_region][, N := NULL]
   
   lut[]
 }
 
 # 2) Plot change vectors in (Δ(P−E), Δ(P+E)) faceted by continent
 plot_change_vectors_by_continent_from_masks <- function(mc_reg_change,
                                                         masks,
                                                         region_col = "ipcc_short_region",
                                                         xcol = "dP_minus_E",
                                                         ycol = "dP_plus_E",
                                                         facet_col = "ipcc_continent",
                                                         ncol = 3,
                                                         label_regions = TRUE,
                                                         title = "Δ(P−E) vs Δ(P+E): vectors from origin (5–95%)") {
   
   d <- as.data.table(mc_reg_change)
   d <- d[is.finite(get(xcol)) & is.finite(get(ycol))]
   
   # summarise per region FIRST
   s <- d[, .(
     x_med = median(get(xcol), na.rm = TRUE),
     x_q05 = quantile(get(xcol), 0.05, na.rm = TRUE, names = FALSE),
     x_q95 = quantile(get(xcol), 0.95, na.rm = TRUE, names = FALSE),
     
     y_med = median(get(ycol), na.rm = TRUE),
     y_q05 = quantile(get(ycol), 0.05, na.rm = TRUE, names = FALSE),
     y_q95 = quantile(get(ycol), 0.95, na.rm = TRUE, names = FALSE)
   ), by = region_col]
   
   # attach continent from masks lookup (unique mapping)
   lut <- make_region_continent_lookup(masks, region_col = "ipcc_short_region", cont_col = "ipcc_continent")
   s <- merge(s, lut, by.x = region_col, by.y = "ipcc_short_region", all.x = TRUE)
   s[is.na(ipcc_continent) | ipcc_continent == "", ipcc_continent := "Other/Unknown"]
   
   # plot
   p <- ggplot(s, aes(x = x_med, y = y_med)) +
     geom_vline(xintercept = 0, linetype = 2) +
     geom_hline(yintercept = 0, linetype = 2) +
     geom_errorbarh(aes(xmin = x_q05, xmax = x_q95), height = 0.0, alpha = 0.8) +
     geom_errorbar(aes(ymin = y_q05, ymax = y_q95), width = 0.0, alpha = 0.8) +
     geom_segment(aes(x = 0, y = 0, xend = x_med, yend = y_med), linewidth = 1.0) +
     geom_point(size = 2.1) +
     labs(x = expression(Delta*(P-E)), y = expression(Delta*(P+E)), title = title) +
     theme_minimal() +
     facet_wrap(~ ipcc_continent, ncol = ncol, scales = "free")
   
   if (label_regions) {
     p <- p + geom_text(aes(label = get(region_col)), nudge_x = 0.02, nudge_y = 0.02, size = 3)
   }
   p
 }
 
 # Example:
 plot_change_vectors_by_continent_from_masks(
   mc_reg_change = mc_reg_change,
   masks = masks,
   xcol = "dP_minus_E",
   ycol = "dP_plus_E",
   ncol = 3,
   title = "Regional change vectors grouped by continent"
 )

 
 
 # ============================================================
 # Beautiful continent-faceted vector plot in (Δ(P−E), Δ(P+E)) space
 # Styled to match your example: quadrant shading, conditions labels,
 # origin vectors, base vs highlighted points, label_repel.
 # ============================================================
 
 library(data.table)
 library(ggplot2)
 library(ggrepel)
 library(scales)
 
 make_region_continent_lookup <- function(masks,
                                          region_col = "ipcc_short_region",
                                          cont_col   = "ipcc_continent") {
   m <- as.data.table(masks)
   m[
     !is.na(get(region_col)) & get(region_col) != "" &
       !is.na(get(cont_col))   & get(cont_col)   != "",
     .N,
     by = .(ipcc_short_region = get(region_col),
            ipcc_continent    = get(cont_col))
   ][order(ipcc_short_region, -N)][, .SD[1], by = ipcc_short_region][, N := NULL]
 }
 
 plot_change_vectors_pretty_by_continent <- function(mc_reg_change,
                                                     masks,
                                                     region_col = "ipcc_short_region",
                                                     xcol = "dP_minus_E",
                                                     ycol = "dP_plus_E",
                                                     highlight_regions = NULL,
                                                     ncol = 3,
                                                     title = "Regional change vectors grouped by continent",
                                                     palettes = NULL) {
   
   d <- as.data.table(mc_reg_change)
   d <- d[is.finite(get(xcol)) & is.finite(get(ycol))]
   
   # ---- summarise per region (median + q05/q95 on both axes) ----
   s <- d[, .(
     x_med = median(get(xcol), na.rm = TRUE),
     x_q05 = quantile(get(xcol), 0.05, na.rm = TRUE, names = FALSE),
     x_q95 = quantile(get(xcol), 0.95, na.rm = TRUE, names = FALSE),
     y_med = median(get(ycol), na.rm = TRUE),
     y_q05 = quantile(get(ycol), 0.05, na.rm = TRUE, names = FALSE),
     y_q95 = quantile(get(ycol), 0.95, na.rm = TRUE, names = FALSE)
   ), by = region_col]
   
   # drop any weird rows (prevents viewport errors in empty facets)
   s <- s[is.finite(x_med) & is.finite(y_med)]
   
   # ---- attach continent from masks ----
   lut <- make_region_continent_lookup(masks)
   s <- merge(s, lut, by.x = region_col, by.y = "ipcc_short_region", all.x = TRUE)
   s[is.na(ipcc_continent) | ipcc_continent == "", ipcc_continent := "Other/Unknown"]
   
   # ---- classify quadrant conditions using medians ----
   s[, Conditions := "Unknown"]
   s[x_med > 0 & y_med > 0, Conditions := "WETTER - ACCELERATED"]
   s[x_med > 0 & y_med < 0, Conditions := "WETTER - DECCELERATED"]
   s[x_med < 0 & y_med > 0, Conditions := "DRIER - ACCELERATED"]
   s[x_med < 0 & y_med < 0, Conditions := "DRIER - DECCELERATED"]
   s[, Conditions := factor(Conditions, levels = c(
     "WETTER - ACCELERATED", "WETTER - DECCELERATED",
     "DRIER - ACCELERATED",  "DRIER - DECCELERATED"
   ))]
   
   # ---- highlight group ----
   s[, highlight_group := "OTHER"]
   if (!is.null(highlight_regions)) {
     s[get(region_col) %in% highlight_regions, highlight_group := "HIGHLIGHT"]
   }
   s[, highlight_group := factor(highlight_group, levels = c("HIGHLIGHT", "OTHER"))]
   
   # ---- palettes (use yours if available) ----
   if (!is.null(palettes) && "water_cycle_change" %in% names(palettes)) {
     col4 <- palettes$water_cycle_change
     if (length(col4) > 4) col4 <- col4[1:4]
   } else {
     # fallback, ordered to match Conditions levels
     col4 <- c("#2E86AB", "#8FB6E9", "#D95F02", "#F6B26B")
   }
   names(col4) <- levels(s$Conditions)
   quad_fill <- alpha(unname(col4), 0.10)
   names(quad_fill) <- names(col4)
   
   # ---- quadrant rectangles (works with free scales) ----
   quad_rects <- data.table(
     xmin = c(0, 0, -Inf, -Inf),
     xmax = c(Inf, Inf, 0, 0),
     ymin = c(0, -Inf, 0, -Inf),
     ymax = c(Inf, 0, Inf, 0),
     Conditions = levels(s$Conditions)
   )
   
   # ---- quadrant labels computed PER CONTINENT facet (prevents zero viewport) ----
   quad_labels <- s[, {
     xmax <- max(x_med, na.rm = TRUE); xmin <- min(x_med, na.rm = TRUE)
     ymax <- max(y_med, na.rm = TRUE); ymin <- min(y_med, na.rm = TRUE)
     
     # handle rare collapsed ranges
     dx <- ifelse(is.finite(xmax - xmin) && (xmax - xmin) > 0, (xmax - xmin), 1)
     dy <- ifelse(is.finite(ymax - ymin) && (ymax - ymin) > 0, (ymax - ymin), 1)
     
     buffer_x <- 0.07 * dx
     buffer_y <- 0.07 * dy
     
     data.table(
       Conditions = levels(Conditions),
       label = levels(Conditions),
       x = c(xmax - buffer_x, xmax - buffer_x, xmin + buffer_x, xmin + buffer_x),
       y = c(ymax - buffer_y, ymin + buffer_y, ymax - buffer_y, ymin + buffer_y)
     )
   }, by = ipcc_continent]
   
   # ---- final plot ----
   ggplot(s, aes(x = x_med, y = y_med)) +
     
     # quadrant shading
     geom_rect(
       data = quad_rects,
       aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Conditions),
       inherit.aes = FALSE, alpha = 0.18
     ) +
     
     # vectors from origin
     geom_segment(
       aes(x = 0, y = 0, xend = x_med, yend = y_med, colour = Conditions),
       alpha = 0.55, linewidth = 0.55
     ) +
     
     # base points
     geom_point(
       data = s[highlight_group == "OTHER"],
       size = 2.2, colour = "grey35"
     ) +
     
     # highlighted points
     geom_point(
       data = s[highlight_group == "HIGHLIGHT"],
       size = 3.2, stroke = 1.1, shape = 21, fill = "white", colour = "black"
     ) +
     
     # uncertainty bars at median point
     geom_errorbarh(aes(xmin = x_q05, xmax = x_q95), height = 0.0, alpha = 0.8, linewidth = 0.35) +
     geom_errorbar(aes(ymin = y_q05, ymax = y_q95), width = 0.0, alpha = 0.8, linewidth = 0.35) +
     
     # axes
     geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
     geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
     
     # region labels
     geom_label_repel(
       aes(label = get(region_col)),
       size = 3.2,
       fill = "white",
       colour = "grey15",
       label.size = 0.25,
       label.r = unit(0.15, "lines"),
       label.padding = unit(0.22, "lines"),
       min.segment.length = 0,
       segment.colour = "grey60",
       max.overlaps = Inf
     ) +
     
     # quadrant text labels (use SAME colour scale via Conditions)
     geom_text(
       data = quad_labels,
       aes(x = x, y = y, label = label, colour = Conditions),
       inherit.aes = FALSE,
       fontface = "bold",
       size = 4.0,
       show.legend = FALSE
     ) +
     
     scale_colour_manual(values = col4, guide = "none") +
     scale_fill_manual(values = quad_fill, guide = "none") +
     
     xlab(expression(Delta~"(P - E)")) +
     ylab(expression(Delta~"(P + E)")) +
     ggtitle(title) +
     
     theme_classic(base_size = 12) +
     theme(
       axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
       axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
       panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
       legend.position = "none"
     ) +
     facet_wrap(~ ipcc_continent, ncol = ncol, scales = "free")
 }
 
 # Example:
 # p <- plot_change_vectors_pretty_by_continent(
 #   mc_reg_change = mc_reg_change,
 #   masks = masks,
 #   xcol = "dP_minus_E",
 #   ycol = "dP_plus_E",
 #   highlight_regions = c("MED","WAF","SAH"),
 #   ncol = 3,
 #   palettes = if (exists("PALETTES")) PALETTES else NULL
 # )
 # print(p)te.
 
 plot_change_vectors_pretty(
   mc_reg_change = mc_reg_change,
   masks = masks,
   xcol = "dP_minus_E",
   ycol = "dP_plus_E",
   highlight_regions = c("MED", "WAF", "SAH"),
   ncol = 3,
   title = "Regional change vectors grouped by continent",
   palettes = if (exists("PALETTES")) PALETTES else NULL
 )
 # print(p)


      