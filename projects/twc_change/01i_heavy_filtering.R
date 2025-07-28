library(data.table)

# Assume your dataset is 'dataset_ranks'

# Step 1: Identify the highest rank (lowest-performing) per category per grid cell
rank_cols <- c("prec_mean_rank", "prec_sd_rank", "prec_rank_slope",
               "evap_mean_rank", "evap_sd_rank", "evap_rank_slope")

# For each grid cell, compute max rank per category
dataset_ranks[, `:=`(
  worst_prec_mean_rank = max(prec_mean_rank, na.rm = TRUE),
  worst_prec_sd_rank = max(prec_sd_rank, na.rm = TRUE),
  worst_prec_slope_rank = max(prec_rank_slope, na.rm = TRUE),
  worst_evap_mean_rank = max(evap_mean_rank, na.rm = TRUE),
  worst_evap_sd_rank = max(evap_sd_rank, na.rm = TRUE),
  worst_evap_slope_rank = max(evap_rank_slope, na.rm = TRUE)
), by = .(lon, lat)]

# Step 2: Flag datasets having the worst rank in at least one category
dataset_ranks[, worst_in_any_category := 
                (prec_mean_rank == worst_prec_mean_rank) |
                (prec_sd_rank == worst_prec_sd_rank) |
                (prec_rank_slope == worst_prec_slope_rank) |
                (evap_mean_rank == worst_evap_mean_rank) |
                (evap_sd_rank == worst_evap_sd_rank) |
                (evap_rank_slope == worst_evap_slope_rank)]

# Step 3: Identify datasets that fail at least one penalty check (i.e., FALSE)
# Penalty columns: pe_ratio_check, prec_check_significance, evap_check_significance
dataset_ranks[, failed_penalty := (pe_ratio_check  == FALSE) | 
                (prec_check_significance == FALSE) |
                (evap_check_significance == FALSE)]

# Important: handle NA explicitly (treat NA as not failing)
dataset_ranks[is.na(failed_penalty), failed_penalty := FALSE]

# Step 4: Identify datasets to remove (worst ranks AND failed penalty)
dataset_ranks[, remove_dataset := worst_in_any_category | failed_penalty]

# Alternative step 4: Identify datasets to remove (only failed penalty)
dataset_ranks[, remove_dataset := failed_penalty]

# Step 5: Remove these datasets from your analysis
filtered_dataset_ranks <- dataset_ranks[remove_dataset == FALSE]


# Count datasets remaining per grid cell
remaining_counts <- filtered_dataset_ranks[, .N, by = .(lon, lat)]

# Plot to verify dataset counts
ggplot(remaining_counts) +
  geom_point(aes(x = lon, y = lat, col = factor(N)), size = 0.7) +
  theme_minimal() +
  labs(title = "Datasets Remaining per Grid Cell after Filtering",
       color = "Dataset Count") +
  scale_color_brewer(palette = "Spectral", direction = -1)
