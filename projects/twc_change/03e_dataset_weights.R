source('source/twc_change.R')
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))

dataset_ranks[, value_ranks := 
                2 * prec_mean_rank + prec_sd_rank + 2 * evap_mean_rank + evap_sd_rank]

dataset_ranks[, change_ranks := prec_rank_slope + evap_rank_slope]
                
# Remove datasets failing pe_ratio_check
filtered_data <- dataset_ranks[pe_ratio_check == TRUE]

# Apply penalties for other failed checks
filtered_data[, change_ranks_ranks_adjusted := change_ranks]
filtered_data[prec_check_significance == FALSE | prec_check_non_significance == FALSE,
              change_ranks_ranks_adjusted := change_ranks * 1.5]
filtered_data[evap_check_significance == FALSE | evap_check_non_significance == FALSE,
              change_ranks_ranks_adjusted := change_ranks * 1.5]

# Calculate probabilistic weights
lambda      <- 0.1 #to minimize the probability of low scores

filtered_data[, value_weight_scaled := exp(-lambda * value_ranks)]
filtered_data[, value_weight := value_weight_scaled / sum(value_weight_scaled, na.rm = T), by = .(lon, lat)]

filtered_data[, change_weight_scaled := exp(-lambda * change_ranks_ranks_adjusted)]
filtered_data[, change_weight := change_weight_scaled / sum(change_weight_scaled, na.rm = T), by = .(lon, lat)]

saveRDS(filtered_data[, .(lon, lat, dataset, value_weight, change_weight)], file.path(PATH_OUTPUT_DATA, 'dataset_weights.Rds'))








