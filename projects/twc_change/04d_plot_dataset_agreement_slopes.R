source('source/twc_change.R')

prec_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
evap_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))
prec_slopes <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_ensemble_slopes.Rds'))
evap_slopes <- readRDS(file.path(PATH_OUTPUT_DATA, 'evap_ensemble_slopes.Rds'))

to_plot <- copy(prec_ensemble_stats)

to_plot[, agreement_cat := fcase(
  majority_significant == TRUE & majority_agrees == TRUE & n_pos > n_neg, "agreement_pos",
  majority_significant == TRUE & majority_agrees == TRUE & n_neg > n_pos, "agreement_neg",
  majority_significant == TRUE & (is.na(majority_agrees) | majority_agrees == FALSE), "significant_disagreement",
  default = "not_significant"
)]

# Define the custom color palette
agreement_colors <- c(
  "agreement_pos" = "#3EA34D",   # green
  "agreement_neg" = "#A0522D",   # brown
  "significant_disagreement" = "#333333", # dark grey
  "not_significant" = "#D3D3D3"  # light grey
)


# Plot
ggplot(to_plot, aes(x = lon, y = lat, color = agreement_cat)) +
  geom_point(size = 0.7) +
  scale_color_manual(
    values = agreement_colors,
    breaks = c("agreement_pos", "agreement_neg", "significant_disagreement", "not_significant"),
    labels = c("Agreement (positive)", "Agreement (negative)", "Significant Disagreement", "Not Significant")
  ) +
  theme_minimal() +
  labs(color = "Trend Agreement")

to_plot <- copy(evap_ensemble_stats)

to_plot[, agreement_cat := fcase(
  majority_significant == TRUE & majority_agrees == TRUE & n_pos > n_neg, "agreement_pos",
  majority_significant == TRUE & majority_agrees == TRUE & n_neg > n_pos, "agreement_neg",
  majority_significant == TRUE & (is.na(majority_agrees) | majority_agrees == FALSE), "significant_disagreement",
  default = "not_significant"
)]

# Define the custom color palette
agreement_colors <- c(
  "agreement_pos" = "#3EA34D",   # green
  "agreement_neg" = "#A0522D",   # brown
  "significant_disagreement" = "#333333", # dark grey
  "not_significant" = "#D3D3D3"  # light grey
)

# Plot
ggplot(to_plot, aes(x = lon, y = lat, color = agreement_cat)) +
  geom_point(size = 0.7) +
  scale_color_manual(
    values = agreement_colors,
    breaks = c("agreement_pos", "agreement_neg", "significant_disagreement", "not_significant"),
    labels = c("Agreement (positive)", "Agreement (negative)", "Significant Disagreement", "Not Significant")
  ) +
  theme_minimal() +
  labs(color = "Trend Agreement")

# Prep agreement columns for both
prec_to_plot <- copy(prec_ensemble_stats)
prec_to_plot <- prec_to_plot[, .(lon, lat, majority_significant, p_agree = majority_agrees)]

evap_to_plot <- copy(evap_ensemble_stats)
evap_to_plot <- evap_to_plot[, .(lon, lat, majority_significant, e_agree = majority_agrees)]

agree_map <- merge(prec_to_plot, evap_to_plot, by = c("lon", "lat"), all = TRUE)

# Create a plotting category
agree_map[, plot_cat := fcase(
  p_agree == TRUE & e_agree == TRUE, "both_agree",
  p_agree == TRUE & (is.na(e_agree) | e_agree == FALSE), "p_only",
  (is.na(p_agree) | p_agree == FALSE) & e_agree == TRUE, "e_only",
  p_agree == FALSE | e_agree == FALSE, "no_agree",
  default = "not_significant"
)]

agree_colors <- c(
  "both_agree" = "#43a047",   
  "p_only"     = "#1976d2",   
  "e_only"     = "#ff9800",   
  "no_agree"   = "darkred",    
  "not_significant"   = "#d3d3d3"    
)

ggplot(agree_map, aes(x = lon, y = lat, color = plot_cat)) +
  geom_point(size = 0.9) +
  scale_color_manual(
    values = agree_colors,
    breaks = c("both_agree", "p_only", "e_only", "no_agree", "not_significant"),
    labels = c("Both agree", "Only precipitation", "Only evaporation", "No agreement", "Not significant")
  ) +
  theme_minimal() +
  labs(color = "Agreement")
