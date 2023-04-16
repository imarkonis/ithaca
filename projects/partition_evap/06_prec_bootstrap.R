# Use Monte Carlo simulation to estimate precipitation mean and its confidence intervals

source('source/partition_prec.R')

## Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.rds"))
prec_mask <- merge(prec_mask, prec_grid, by = c("lon", "lat"))

## Variables
sample_high_agreement <- prec_mask[rel_dataset_agreement == 'high' | rel_dataset_agreement == 'above average', .(prec_volume_year, KG_class_3, elev_class, land_use_class, biome_class)] #Biome class is not
sample_summary <- sample_high_agreement[, .(n_size = .N), .(KG_class_3, elev_class, land_use_class, biome_class)]
sample_summary[, range(n_size)]
prec_mask <- prec_mask[complete.cases(prec_mask)]
n_grid_cells <- nrow(prec_mask)

## Analysis
registerDoParallel(cores = N_CORES - 1)
prec_mean_sampled <- foreach(grid_cell_count = 1:n_grid_cells, .combine = rbind, .verbose = T) %dopar% {
    dummy <- prec_mask[grid_cell_count, ]
    dummy_coord <- dummy[, .(lon, lat)]
    dummy_properties <- dummy[,  .(KG_class_3, elev_class, land_use_class, biome_class)]
    dummy_merge <- merge(sample_high_agreement, dummy_properties, by = c('KG_class_3', 'elev_class', 'land_use_class', 'biome_class'))
    if(nrow(dummy_merge) > 0) {
      dummy_sample <- sample(x = dummy_merge$prec_volume_year, size = 1) #If there is no value in High Agreement then use the average (for 10,000 values)
    } else {dummy_sample <- prec_mask[grid_cell_count, prec_volume_year]} 
    cbind(dummy_coord, prec_volume_year = dummy_sample)
  }
prec_mean_sampled_mc <- cbind(prec_mean_sampled, instance = 1)

for(mc_instance in 2:100){
  prec_mean_sampled <- foreach(grid_cell_count = 1:n_grid_cells, .combine = rbind, .verbose = T) %dopar% {
    dummy <- prec_mask[grid_cell_count, ]
    dummy_coord <- dummy[, .(lon, lat)]
    dummy_properties <- dummy[,  .(KG_class_3, elev_class, land_use_class, biome_class)]
    dummy_merge <- merge(sample_high_agreement, dummy_properties, by = c('KG_class_3', 'elev_class', 'land_use_class', 'biome_class'))
    if(nrow(dummy_merge) > 0) {
      dummy_sample <- sample(x = dummy_merge$prec_volume_year, size = 1) #If there is no value in High Agreement then use the average (for 10,000 values)
    } else {dummy_sample <- prec_mask[grid_cell_count, prec_volume_year]} 
    cbind(dummy_coord, prec_volume_year = dummy_sample)
  }
  prec_mean_sampled <- cbind(prec_mean_sampled, instance = mc_instance)
  prec_mean_sampled_mc <- rbind(prec_mean_sampled_mc, prec_mean_sampled)
  print(mc_instance)
  saveRDS(prec_mean_sampled_mc, paste0(PATH_SAVE_PARTITION_PREC, 'bootstrap_temp.rds'))
}

file.rename(paste0(PATH_SAVE_PARTITION_PREC, 'bootstrap_temp.rds'), 
            paste0(PATH_SAVE_PARTITION_PREC, 'bootstrap_prec.rds'))

prec_sum_mc <- prec_mean_sampled_mc[, .(prec_sum = sum(prec_volume_year)), instance]
prec_sum_mc[, mean(prec_sum)]
prec_sum_mc[, median(prec_sum)]
prec_sum_mc[, quantile(prec_sum, 0.01)]
prec_sum_mc[, quantile(prec_sum, 0.99)]


prec_mean_sampled_mc <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, 'bootstrap_prec.rds'))
