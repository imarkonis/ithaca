# --- Load libraries and sources ---
source('source/twc_change.R')
library(patchwork)

# --- Read in Data ---
prec_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
evap_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))
prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_evap.Rds'))


## ANALYSIS
# --- Calculate summary statistics per dataset/gridcell ---
prec_stats <- prec_evap[, .(prec_mean = mean(prec), prec_sd = sd(prec)), .(lon, lat, dataset)]
evap_stats <- prec_evap[year >= 2000, .(evap_mean = mean(evap), evap_sd = sd(evap)), .(lon, lat, dataset)]

dataset_stats <- merge(prec_stats, evap_stats, by = c('lon', 'lat', 'dataset'))

# --- Compare precipitation datasets ---
prec_comparison <- merge(prec_stats, prec_ensemble_stats)
prec_comparison[, diff_mean := abs(prec_mean - ens_median)] #ens_median is the ensemble median of the dataset means
prec_comparison[, diff_sd := abs(prec_sd - ens_sd)]
prec_comparison[, rank_mean := frank(diff_mean, ties.method = "min"), by = .(lon, lat)]
prec_comparison[, rank_sd := frank(diff_sd, ties.method = "min"), by = .(lon, lat)]
prec_comparison[, prec_ranks := rank_mean + rank_sd]
prec_comparison[, best_dataset := dataset[which.min(prec_ranks)], by = .(lon, lat)]

# --- Compare evaporation datasets ---
evap_comparison <- merge(evap_stats, evap_ensemble_stats)
evap_comparison[, diff_mean := abs(evap_mean - ens_median)]
evap_comparison[, diff_sd := abs(evap_sd - ens_sd)]
evap_comparison[, rank_mean := frank(diff_mean, ties.method = "min"), by = .(lon, lat)]
evap_comparison[, rank_sd := frank(diff_sd, ties.method = "min"), by = .(lon, lat)]
evap_comparison[, evap_ranks := rank_mean + rank_sd]
evap_comparison[, best_dataset := dataset[which.min(evap_ranks)], by = .(lon, lat)]

dataset_ranks <- merge(prec_comparison[, .(lon, lat, dataset, prec_mean_rank = rank_mean, prec_sd_rank = rank_sd)], 
      evap_comparison[, .(lon, lat, dataset, evap_mean_rank = rank_mean, evap_sd_rank = rank_sd)],
      by = c("lon", "lat", "dataset"))
saveRDS(dataset_ranks, file = paste0(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))
saveRDS(dataset_stats, file.path(PATH_OUTPUT_DATA, 'prec_evap_stats.Rds'))


## PLOTS

# --- Functions for combined ranking and plotting ---
plot_best_dataset_map_density <- function(to_plot, title_label = "Dataset") {
  p_map <- ggplot(to_plot) +
    geom_point(aes(x = lon, y = lat, col = best_dataset), size = 0.7) +
    theme_minimal() +
    labs(col = title_label)
  
  p_density <- ggplot(to_plot, aes(x = lat, col = best_dataset)) +
    geom_density(alpha = 0.4, adjust = 1.5) +
    labs(x = "Latitude", y = "Density", fill = title_label) +
    theme_minimal()
  
  p_density_side <- p_density + coord_flip() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  p_map + p_density_side + plot_layout(widths = c(3, 1))
}

# --- Plot best precipitation dataset ---
to_plot_prec <- unique(prec_comparison[, .(lon, lat, best_dataset)])
print(plot_best_dataset_map_density(to_plot_prec, "Dataset"))

# --- Plot best evaporation dataset ---
to_plot_evap <- unique(evap_comparison[, .(lon, lat, best_dataset)])
print(plot_best_dataset_map_density(to_plot_evap, "Dataset"))

# --- Combined metric (precip + evap) ---
combined_ranks <- merge(
  prec_comparison[, .(lon, lat, dataset, prec_ranks)],
  evap_comparison[, .(lon, lat, dataset, evap_ranks)],
  by = c("lon", "lat", "dataset")
)
combined_ranks[, all_ranks := prec_ranks + evap_ranks]
combined_ranks[, best_dataset := dataset[which.min(all_ranks)], by = .(lon, lat)]

to_plot_combined <- unique(combined_ranks[, .(lon, lat, best_dataset)])
print(plot_best_dataset_map_density(to_plot_combined, "Dataset"))

# --- Optionally: Plot worst dataset (max all_ranks) ---
combined_ranks[, worst_dataset := dataset[which.max(all_ranks)], by = .(lon, lat)]
to_plot_worst <- unique(combined_ranks[, .(lon, lat, worst_dataset)])
setnames(to_plot_worst, "worst_dataset", "best_dataset") # to use same plotting function
print(plot_best_dataset_map_density(to_plot_worst, "Worst Dataset"))

# --- Faceted rank plot (continuous color scale) ---
ggplot(combined_ranks) +
  geom_point(aes(x = lon, y = lat, col = all_ranks), size = 0.7) +
  theme_minimal() +
  facet_wrap(~dataset) +
  labs(col = "Rank") +
  scale_color_gradient2(
    low = PALETTES$smooth[2],
    mid = "white",
    high = PALETTES$smooth[11],
    midpoint = median(combined_ranks$all_ranks, na.rm = TRUE)
  )

# --- Map of best rank (across all datasets per gridcell) ---
best_rank_map <- combined_ranks[, .SD[which.min(all_ranks)], by = .(lon, lat)]
ggplot(best_rank_map) +
  geom_point(aes(x = lon, y = lat, col = all_ranks), size = 0.7) +
  theme_minimal() +
  labs(col = "Rank") +
  scale_color_gradient2(
    low = PALETTES$smooth[2],
    mid = "white",
    high = PALETTES$smooth[11],
    midpoint = median(best_rank_map$all_ranks, na.rm = TRUE)
  )
