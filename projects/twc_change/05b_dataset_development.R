source('source/twc_change.R')

masks <- pRecipe::pRecipe_masks()
dataset_weights <- readRDS(paste0(PATH_OUTPUT, 'dataset_pair_weights.rds'))
avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))

# GRACE+CCI weighting
dummy <- merge(dataset_weights, avail_flux_change, by = c("lon", "lat", "dataset_pair"))

avail_flux_change_weighted <- dummy[, .(avail_change = sum(avail_change * weight), 
                                                    flux_change = sum(flux_change * weight)), 
                                                .(lon, lat)]
avail_flux_change_weighted <- avail_flux_change_weighted[complete.cases(avail_flux_change_weighted)]

saveRDS(avail_flux_change_weighted, file = paste0(PATH_OUTPUT, 'avail_flux_change_grid_weighted.rds'))

# KG-adaptive best-performer
best_performing_pair <- dataset_weights[ , .SD[which.max(weight)], by = .(lon, lat)]

results_classification <- merge(best_performing_pair, masks[land_mask == 'land', .(lon, lat, KG_class)]) 
pair_per_kg <- results_classification[, .(weight = mean(weight)), .(KG_class, dataset_pair)]
best_performing_pair_KG <- pair_per_kg[ , .SD[which.max(weight)], by = KG_class]

dummy <- merge(avail_flux_change, masks[land_mask == 'land', .(lon, lat, KG_class)], by = c("lon", "lat")) 
avail_flux_change_best_performer_kg <- merge(best_performing_pair_KG, dummy, by = c('KG_class', 'dataset_pair'))

saveRDS(avail_flux_change_best_performer_kg[, .(lon, lat, avail_change, flux_change)], file = paste0(PATH_OUTPUT, 'avail_flux_change_best_performer_kg.rds'))

