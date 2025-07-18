source('source/twc_change.R')
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))

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

dataset_ranks[, all_ranks := 
                 prec_mean_rank + prec_sd_rank + prec_rank_slope +
                 evap_mean_rank + evap_sd_rank + evap_rank_slope]

# Add penalties based on logical checks (optional but recommended):
dataset_ranks[, all_ranks_penalized := all_ranks +
                 (!pe_ratio_check)*2 + # penalty if pe_ratio_check is FALSE
                 (!prec_check_significance)*1 +
                 (!evap_check_significance)*1]

dataset_ranks[, best_dataset := dataset[which.min(all_ranks)], by = .(lon, lat)]
dataset_ranks[, worst_dataset := dataset[which.max(all_ranks)], by = .(lon, lat)]

to_plot_combined <- unique(dataset_ranks[, .(lon, lat, best_dataset)])
print(plot_best_dataset_map_density(to_plot_combined, "Best Dataset"))

to_plot_worst <- unique(dataset_ranks[, .(lon, lat, worst_dataset)])
setnames(to_plot_worst, "worst_dataset", "best_dataset") # to use same plotting function
print(plot_best_dataset_map_density(to_plot_worst, "Worst Dataset"))


ggplot(dataset_ranks) +
  geom_point(aes(x = lon, y = lat, col = all_ranks), size = 0.7) +
  theme_minimal() +
  facet_wrap(~dataset) +
  labs(col = "Composite Rank (All Metrics)") +
  scale_color_gradient2(
    low = PALETTES$smooth[2],
    mid = "white",
    high = PALETTES$smooth[11],
    midpoint = median(dataset_ranks$all_ranks_penalized, na.rm = TRUE)
  )


best_rank_map <- dataset_ranks[, .SD[which.min(all_ranks)], by = .(lon, lat)]
ggplot(best_rank_map) +
  geom_point(aes(x = lon, y = lat, col = all_ranks), size = 0.7) +
  theme_minimal() +
  labs(col = "Best Composite Rank") +
  scale_color_gradient2(
    low = PALETTES$smooth[2],
    mid = "white",
    high = PALETTES$smooth[11],
    midpoint = median(best_rank_map$all_ranks_penalized, na.rm = TRUE)
  )
