#WNA gives you the midlatitude west-coast / mountain-storm / Pacific storm-track regime over North America, with strong orographic and cool-season synoptic control. SSA adds South America with a Southern Hemisphere extratropical regime influenced by the westerlies and frontal systems. MED captures the classic Mediterranean transition zone between subtropical drying and midlatitude storm influence. NEU represents a wetter high-latitude to midlatitude Atlantic storm-track regime.
#SAH is useful because it anchors the hyper-arid descending branch of the Hadley circulation and gives you a true dry-limit reference region. WAF captures the West African monsoon belt and strong land-atmosphere seasonality. SAS represents the South Asian monsoon core. EAS adds the East Asian monsoon / subtropical frontal regime, which is dynamically distinct from SAS despite both being monsoonal. CAU gives you an interior subtropical dryland regime in Australasia, complementing SAH but in a different continental and hemispheric setting.

#So the logic is:
#  
# North America: WNA
# South America: SSA
# Europe: MED + NEU
# Africa: SAH + WAF
# Asia: SAS + EAS
# Australasia: CAU

#This gives you coverage of the major circulation families:
#  
# midlatitude storm tracks / westerlies: WNA, NEU, SSA
# subtropical dry zones / descending Hadley branch: SAH, CAU, partly MED
# monsoon regimes: WAF, SAS, EAS
# transition/orographic complexity: MED, WNA, EAS

library(ggrepel)

regions_9 <- c(
  "WNA",  # Western North America
  "SSA",  # Southern South America
  "MED",  # Mediterranean
  "NEU",  # Northern Europe
  "SAH",  # Sahara
  "WAF",  # Western Africa
  "SAS",  # South Asia
  "EAS",  # East Asia
  "CAU"   # Central Australia
)

plot_dt <- copy(region_sim_means)[region %in% regions_9]
sum_dt <- plot_dt[, .(
  avail_mean = median(avail_change, na.rm = TRUE),
  flux_mean  = median(flux_change,  na.rm = TRUE),
  avail_lo   = quantile(avail_change, 0.10, na.rm = TRUE),
  avail_hi   = quantile(avail_change, 0.90, na.rm = TRUE),
  flux_lo    = quantile(flux_change,  0.10, na.rm = TRUE),
  flux_hi    = quantile(flux_change,  0.90, na.rm = TRUE),
  n_sims     = .N
), by = .(scenario, region)]

ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_point(data = plot_dt, aes(avail_change, flux_change, col = region), alpha = 0.15, size = 1) +
  geom_errorbar(
    data = sum_dt,
    aes(x = avail_mean, y = flux_mean, ymin = flux_lo, ymax = flux_hi, col = region),
    width = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_errorbarh(
    data = sum_dt,
    aes(x = avail_mean, y = flux_mean, xmin = avail_lo, xmax = avail_hi, col = region),
    height = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_point(data = sum_dt, aes(avail_mean, flux_mean, col = region), size = 2.8) +
  ggrepel::geom_text_repel(
    data = sum_dt,
    aes(avail_mean, flux_mean, label = region),
    size = 3.2,
    seed = 42
  ) +
  facet_wrap(~scenario, ncol = 3) +
  theme_bw()

sum_dt <- plot_dt[, .(
  prec_mean = median(prec_change, na.rm = TRUE),
  evap_mean  = median(evap_change,  na.rm = TRUE),
  prec_lo   = quantile(prec_change, 0.10, na.rm = TRUE),
  prec_hi   = quantile(prec_change, 0.90, na.rm = TRUE),
  evap_lo    = quantile(evap_change,  0.10, na.rm = TRUE),
  evap_hi    = quantile(evap_change,  0.90, na.rm = TRUE),
  n_sims     = .N
), by = .(scenario, region)]

ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_point(data = plot_dt, aes(prec_change, evap_change, col = region), alpha = 0.15, size = 1) +
  geom_errorbar(
    data = sum_dt,
    aes(x = prec_mean, y = evap_mean, ymin = evap_lo, ymax = evap_hi, col = region),
    width = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_errorbarh(
    data = sum_dt,
    aes(x = prec_mean, y = evap_mean, xmin = prec_lo, xmax = prec_hi, col = region),
    height = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_point(data = sum_dt, aes(prec_mean, evap_mean, col = region), size = 2.8) +
  ggrepel::geom_text_repel(
    data = sum_dt,
    aes(prec_mean, evap_mean, label = region),
    size = 3.2,
    seed = 42
  ) +
  facet_wrap(~scenario, ncol = 3) +
  theme_bw()

## All regions
plot_dt <- copy(region_sim_means)[scenario == "base"]

sum_dt <- plot_dt[, .(
  avail_mean = median(avail_change, na.rm = TRUE),
  flux_mean  = median(flux_change,  na.rm = TRUE),
  avail_lo   = quantile(avail_change, 0.10, na.rm = TRUE),
  avail_hi   = quantile(avail_change, 0.90, na.rm = TRUE),
  flux_lo    = quantile(flux_change,  0.10, na.rm = TRUE),
  flux_hi    = quantile(flux_change,  0.90, na.rm = TRUE),
  n_sims     = .N
), by = .(scenario, region)]

# 1. order regions by effect size
region_order <- sum_dt[
  , .(size = sqrt(avail_mean^2 + flux_mean^2)),
  by = region
][order(-size), region]

# 2. assign ordered regions into exactly 9 facet groups
region_map <- data.table(
  region = region_order,
  panel = paste0("Panel ", cut(
    seq_along(region_order),
    breaks = 9,
    labels = FALSE
  ))
)

# 3. join panel info
plot_dt <- merge(plot_dt, region_map, by = "region", all.x = TRUE)
sum_dt  <- merge(sum_dt,  region_map, by = "region", all.x = TRUE)

# 4. apply factor ordering
plot_dt[, region := factor(region, levels = region_order)]
sum_dt[,  region := factor(region, levels = region_order)]

plot_dt[, panel := factor(panel, levels = paste0("Panel ", 1:9))]
sum_dt[,  panel := factor(panel, levels = paste0("Panel ", 1:9))]

# 5. plot
ggplot(data = plot_dt) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_point(
    aes(avail_change, flux_change, col = region),
    alpha = 0.15, size = 1
  ) +
  geom_errorbar(
    data = sum_dt,
    aes(x = avail_mean, y = flux_mean, ymin = flux_lo, ymax = flux_hi),
    width = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_errorbarh(
    data = sum_dt,
    aes(x = avail_mean, y = flux_mean, xmin = avail_lo, xmax = avail_hi),
    height = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_point(
    data = sum_dt,
    aes(avail_mean, flux_mean, col = region),
    size = 2.8
  ) +
  ggrepel::geom_text_repel(
    data = sum_dt,
    aes(avail_mean, flux_mean, label = region),
    size = 3.2,
    seed = 42
  ) +
  facet_wrap(~ panel, ncol = 3, scales = "free") +
  theme_bw()


plot_dt <- copy(region_sim_means)[scenario == "base"]

sum_dt <- plot_dt[, .(
  prec_mean = median(prec_change, na.rm = TRUE),
  evap_mean = median(evap_change, na.rm = TRUE),
  prec_lo   = quantile(prec_change, 0.10, na.rm = TRUE),
  prec_hi   = quantile(prec_change, 0.90, na.rm = TRUE),
  evap_lo   = quantile(evap_change, 0.10, na.rm = TRUE),
  evap_hi   = quantile(evap_change, 0.90, na.rm = TRUE),
  n_sims    = .N
), by = .(scenario, region)]

# order regions by effect size in prec-evap space
region_order <- sum_dt[
  , .(size = sqrt(prec_mean^2 + evap_mean^2)),
  by = region
][order(-size), region]

# split ordered regions into 9 panels
region_map <- data.table(
  region = region_order,
  panel = paste0("Panel ", cut(
    seq_along(region_order),
    breaks = 9,
    labels = FALSE
  ))
)

# join panel info
plot_dt <- merge(plot_dt, region_map, by = "region", all.x = TRUE)
sum_dt  <- merge(sum_dt,  region_map, by = "region", all.x = TRUE)

# factor order
plot_dt[, region := factor(region, levels = region_order)]
sum_dt[,  region := factor(region, levels = region_order)]

plot_dt[, panel := factor(panel, levels = paste0("Panel ", 1:9))]
sum_dt[,  panel := factor(panel, levels = paste0("Panel ", 1:9))]

# plot
ggplot(data = plot_dt) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_point(
    aes(prec_change, evap_change, col = region),
    alpha = 0.15, size = 1
  ) +
  geom_errorbar(
    data = sum_dt,
    aes(x = prec_mean, y = evap_mean, ymin = evap_lo, ymax = evap_hi),
    width = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_errorbarh(
    data = sum_dt,
    aes(x = prec_mean, y = evap_mean, xmin = prec_lo, xmax = prec_hi),
    height = 0, linewidth = 0.45, colour = "grey35"
  ) +
  geom_point(
    data = sum_dt,
    aes(prec_mean, evap_mean, col = region),
    size = 2.8
  ) +
  ggrepel::geom_text_repel(
    data = sum_dt,
    aes(prec_mean, evap_mean, label = region),
    size = 3.2,
    seed = 42
  ) +
  facet_wrap(~ panel, ncol = 3, scales = "free") +
  theme_bw()




sc_use <- "base"

dt <- copy(region_sim_means)[scenario == sc_use]

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

dt <- merge(dt, circ_map, by = "region", all.x = TRUE)

dt[is.na(family6), unique(region)]

dt[, pathway := fcase(
  avail_change > 0 & flux_change > 0, "wet_intensification",
  avail_change <= 0 & flux_change > 0, "stress_intensification",
  avail_change <= 0 & flux_change <= 0, "contraction_or_drying",
  avail_change > 0 & flux_change <= 0, "availability_gain_flux_loss",
  default = NA_character_
)]

dt[, pathway := factor(
  pathway,
  levels = c(
    "wet_intensification",
    "stress_intensification",
    "contraction_or_drying",
    "availability_gain_flux_loss"
  )
)]

centroids6 <- dt[, .(
  avail_mean = median(avail_change, na.rm = TRUE),
  flux_mean  = median(flux_change,  na.rm = TRUE),
  avail_lo   = quantile(avail_change, 0.10, na.rm = TRUE),
  avail_hi   = quantile(avail_change, 0.90, na.rm = TRUE),
  flux_lo    = quantile(flux_change,  0.10, na.rm = TRUE),
  flux_hi    = quantile(flux_change,  0.90, na.rm = TRUE),
  n          = .N
), by = family6]

pal6 <- c(
  stormtrack = "#2C7FB8",
  monsoon = "#1A9850",
  dry_subsidence = "#D95F02",
  highland = "#7570B3",
  tropical_margin = "#1FBFCF",
  continental_transition = "#E7298A"
)

ggplot() +
  geom_hline(yintercept = 0, linewidth = 1.3, colour = "grey70") +
  geom_vline(xintercept = 0, linewidth = 1.3, colour = "grey70") +
  
  geom_point(
    data = dt,
    aes(x = avail_change, y = flux_change, colour = region),
    alpha = 0.12,
    size = 1.1
  ) +
  
  geom_errorbar(
    data = centroids6,
    aes(x = avail_mean, y = flux_mean, ymin = flux_lo, ymax = flux_hi),
    width = 0,
    linewidth = 0.7
  ) +
  geom_errorbarh(
    data = centroids6,
    aes(x = avail_mean, y = flux_mean, xmin = avail_lo, xmax = avail_hi),
    height = 0,
    linewidth = 0.7
  ) +
  
  facet_wrap(~family6, scales = 'free') +
  labs(
    title = paste("Circulation-family centroids |", sc_use),
    x = "Availability change",
    y = "Flux change",
    colour = "Family"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

centroids_detail <- dt[, .(
  avail_mean = median(avail_change, na.rm = TRUE),
  flux_mean  = median(flux_change,  na.rm = TRUE),
  avail_lo   = quantile(avail_change, 0.10, na.rm = TRUE),
  avail_hi   = quantile(avail_change, 0.90, na.rm = TRUE),
  flux_lo    = quantile(flux_change,  0.10, na.rm = TRUE),
  flux_hi    = quantile(flux_change,  0.90, na.rm = TRUE),
  n          = .N
), by = family_detail]

pal_detail <- c(
  stormtrack_core = "#2C7FB8",
  stormtrack_pacific_margin = "#08519C",
  monsoon_core = "#1A9850",
  monsoon_frontal = "#4DAF4A",
  monsoon_margin = "#66C2A5",
  dry_desert_core = "#D95F02",
  dry_interior = "#E6AB02",
  dry_transition = "#FDAE61",
  highland = "#7570B3",
  mixed_transition = "#E7298A",
  tropical_margin = "#1FBFCF"
)

ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70") +
  
  geom_point(
    data = dt,
    aes(x = avail_change, y = flux_change, colour = family_detail),
    alpha = 0.10,
    size = 1
  ) +
  
  geom_errorbar(
    data = centroids_detail,
    aes(x = avail_mean, y = flux_mean, ymin = flux_lo, ymax = flux_hi, colour = family_detail),
    width = 0,
    linewidth = 0.7
  ) +
  geom_errorbarh(
    data = centroids_detail,
    aes(x = avail_mean, y = flux_mean, xmin = avail_lo, xmax = avail_hi, colour = family_detail),
    height = 0,
    linewidth = 0.7
  ) +
  geom_point(
    data = centroids_detail,
    aes(x = avail_mean, y = flux_mean, colour = family_detail),
    size = 3.2
  ) +
  
  scale_colour_manual(values = pal_detail, drop = FALSE) +
  labs(
    title = paste("Detailed circulation classes |", sc_use),
    x = "Availability change",
    y = "Flux change",
    colour = "Detailed family"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )


pathway_frac <- dt[, .N, by = .(family6, pathway)]
pathway_frac[, frac := N / sum(N), by = family6]

path_cols <- c(
  wet_intensification = "#1A9850",
  stress_intensification = "#2C7FB8",
  contraction_or_drying = "#D95F02",
  availability_gain_flux_loss = "#7570B3"
)

ggplot(pathway_frac, aes(x = family6, y = frac, fill = pathway)) +
  geom_col(position = "fill", width = 0.8) +
  scale_fill_manual(values = path_cols, drop = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = paste("Pathway fractions by circulation family |", sc_use),
    x = NULL,
    y = "Fraction",
    fill = "Pathway"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

unc_reg <- dt[, .(
  sd_avail  = sd(avail_change, na.rm = TRUE),
  sd_flux   = sd(flux_change,  na.rm = TRUE),
  qrange_avail = quantile(avail_change, 0.9, na.rm = TRUE) - quantile(avail_change, 0.1, na.rm = TRUE),
  qrange_flux  = quantile(flux_change,  0.9, na.rm = TRUE) - quantile(flux_change,  0.1, na.rm = TRUE)
), by = .(region, family6)]

unc_family <- unc_reg[, .(
  mean_qrange_avail = mean(qrange_avail, na.rm = TRUE),
  mean_qrange_flux  = mean(qrange_flux,  na.rm = TRUE),
  n_regions = .N
), by = family6]

unc_long <- melt(
  unc_family,
  id.vars = c("family6", "n_regions"),
  measure.vars = c("mean_qrange_avail", "mean_qrange_flux"),
  variable.name = "metric",
  value.name = "value"
)

unc_long[, metric := factor(
  metric,
  levels = c("mean_qrange_avail", "mean_qrange_flux"),
  labels = c("Availability uncertainty", "Flux uncertainty")
)]

ggplot(unc_long, aes(x = family6, y = value, fill = metric)) +
  geom_col(position = "dodge", width = 0.75) +
  labs(
    title = paste("Mean simulation uncertainty by circulation family |", sc_use),
    x = NULL,
    y = "Mean 0.1–0.9 range",
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

dominant_pathway <- pathway_frac[order(family6, -frac)][, .SD[1], by = family6]
dominant_pathway[]

