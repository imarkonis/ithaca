source('source/twc_change.R')
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))


# Original Ranking
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

# Strict slope-sensitive ranking Ranking 
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))
BIAS_THRES <- 2


filtered_data <- dataset_ranks[pe_ratio_check == TRUE & 
                                 (prec_check_significance != FALSE | is.na(prec_check_significance)) &
                               (evap_check_significance != FALSE | is.na(evap_check_significance))]

filtered_data <- filtered_data[prec_mean_bias < BIAS_THRES & prec_sd_bias < BIAS_THRES & 
                                 evap_mean_bias < BIAS_THRES & evap_sd_bias < BIAS_THRES &
                                 prec_bias_slope < BIAS_THRES & evap_bias_slope < BIAS_THRES]

filtered_data[, .N, dataset] 

oo <- filtered_data[, .(lon, lat)]
ggplot(oo[!duplicated(oo)]) +
  geom_point(aes(x = lon, y = lat))
