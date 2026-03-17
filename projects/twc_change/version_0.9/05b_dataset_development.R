source('source/twc_change.R')

masks <- pRecipe::pRecipe_masks()
dataset_weights <- readRDS(paste0(PATH_OUTPUT_DATA, 'dataset_weights.Rds'))
avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_grid.rds'))

# Ensemble & Evaluation weighting
dummy <- merge(dataset_weights, avail_flux_change, by = c("lon", "lat", "dataset"))

avail_flux_change_weighted <- dummy[, .(avail_change = sum(avail_change * weight), 
                                                    flux_change = sum(flux_change * weight)), 
                                                .(lon, lat)]
avail_flux_change_weighted <- avail_flux_change_weighted[complete.cases(avail_flux_change_weighted)]

saveRDS(avail_flux_change_weighted, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_grid_weighted.rds'))

# KG-adaptive best-performer
best_performing_dataset <- dataset_weights[ , .SD[which.max(weight)], by = .(lon, lat)]

results_classification <- merge(best_performing_dataset, masks[land_mask == 'land', .(lon, lat, KG_class)]) 
dataset_per_kg <- results_classification[, .(weight = mean(weight)), .(KG_class, dataset)]
best_performing_dataset_KG <- dataset_per_kg[ , .SD[which.max(weight)], by = KG_class]

dummy <- merge(avail_flux_change, masks[land_mask == 'land', .(lon, lat, KG_class)], by = c("lon", "lat")) 
avail_flux_change_best_performer_kg <- merge(best_performing_dataset_KG, dummy, by = c('KG_class', 'dataset'))

saveRDS(avail_flux_change_best_performer_kg[, .(lon, lat, avail_change, flux_change)], file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_best_performer_kg.rds'))

