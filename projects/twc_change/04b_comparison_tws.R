install.packages('trend')
library(trend)

source('source/avail_change.R')
masks <- pRecipe::pRecipe_masks()

total_water_storage <- readRDS(paste0(PATH_OUTPUT_RAW, 'other/grace_yearly_2019.Rds'))
total_water_storage <- merge(total_water_storage, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)]) 
setnames(total_water_storage, 'ipcc_short_region', 'region')

avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))
prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
prec_evap <- prec_evap[year >= 2002]
prec_evap <- merge(prec_evap, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)]) 
setnames(prec_evap, 'ipcc_short_region', 'region')

#TWS slope estimation
tws_slopes_grid <- total_water_storage[, {
  fit <- sens.slope(value, year)
  .(slope = fit$estimates, p.value = fit$p.value)
}, by = .(lon, lat)]
setnames(tws_slopes_grid, 'slope', 'tws_slope')
saveRDS(tws_slopes_grid, paste0(PATH_OUTPUT_DATA, 'tws_slopes_grid.Rds'))

tws_means_ipcc <- total_water_storage[, .(value = mean(value)), .(year, region)]
tws_slopes_ipcc <- tws_means_ipcc[, {
  fit <- sens.slope(value, year)
  .(slope = fit$estimates, p.value = fit$p.value)
}, by = region]
setnames(tws_slopes_ipcc, 'slope', 'tws_slope')
saveRDS(tws_slopes_ipcc, paste0(PATH_OUTPUT_DATA, 'tws_slopes_ipcc.Rds'))
       
#IPCC comparison
prec_evap_ipcc_means <- prec_evap[, .(value = mean(value)), .(variable, year, region, dataset)]

dt_wide <- dcast(prec_evap_ipcc_means, dataset + year + region ~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(year, region, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(year, region, dataset_evap = dataset, evap)]
avail_flux <- merge(prec_dt, evap_dt, by = c("year", 'region'), allow.cartesian = TRUE)
avail_flux[, avail := prec - evap]
avail_flux[, dataset_pair := paste(dataset_prec, dataset_evap, sep = "-")]

slopes_avail_ipcc <- avail_flux[, {
  fit <- sens.slope(avail, year)
  .(slope = fit$estimates, p.value = fit$p.value)
}, by = .(dataset_pair, region)]

avail_change <- merge(slopes_avail_ipcc[, .(region, dataset_pair, slope)], tws_slopes_ipcc[, .(region, tws_slope)])

avail_change[, agreement := factor("Uknown")]
avail_change[slope > 0 & tws_slope > 0,  agreement := factor("yes")]
avail_change[slope < 0 & tws_slope < 0,  agreement := factor("yes")]
avail_change[slope > 0 & tws_slope < 0,  agreement := factor("no")]
avail_change[slope < 0 & tws_slope > 0,  agreement := factor("no")]

ggplot(avail_change[agreement != "Uknown"]) +
  geom_bar(aes(agreement)) +
  facet_wrap(~dataset_pair)

#Grid cell comparison
avail_flux <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_year.rds') )

dataset_count <- 1
dummy_dataset <- avail_flux[dataset_pair == dataset_pairs[dataset_count]]
slopes_avail_grid <- dummy_dataset[, {
  if (length(unique(year)) > 1 && length(unique(avail)) > 1) {
    fit <- sens.slope(avail, year)
    .(slope = fit$estimates, p.value = fit$p.value)
  } else {
    .(slope = NA_real_, p.value = NA_real_)
  }
}, by = .(lon, lat)]
slopes_avail_grid$dataset_pair <- dataset_pairs[dataset_count]

n_pairs <- length(dataset_pairs)

for(dataset_count in 3:n_pairs){
  dummy_dataset <- avail_flux[dataset_pair == dataset_pairs[dataset_count]]
  result <- dummy_dataset[, {
    if (length(unique(year)) > 1 && length(unique(avail)) > 1) {
      fit <- sens.slope(avail, year)
      .(slope = fit$estimates, p.value = fit$p.value)
    } else {
      .(slope = NA_real_, p.value = NA_real_)
    }
  }, by = .(lon, lat)]
  result$dataset_pair <- dataset_pairs[dataset_count]
  slopes_avail_grid <- rbind(slopes_avail_grid, result)
  print(dataset_count)
}
saveRDS(slopes_avail_grid, file = paste0(PATH_OUTPUT, 'slopes_avail_grid.rds'))

avail_change_grid <- merge(slopes_avail_grid, tws_slopes_grid[, .(lon, lat, tws_slope)], 
                         by = c('lon', 'lat'), allow.cartesian=TRUE)
avail_change_grid <- merge(avail_change_grid, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)]) 
setnames(avail_change_grid, 'ipcc_short_region', 'region')


avail_change_grid[, agreement := factor("yes")]
avail_change_grid[slope > 0 & tws_slope > 0,  agreement := factor("yes")]
avail_change_grid[slope < 0 & tws_slope < 0,  agreement := factor("yes")]
avail_change_grid[slope > 0 & tws_slope < 0,  agreement := factor("no")]
avail_change_grid[slope < 0 & tws_slope > 0,  agreement := factor("no")]

to_plot <- copy(avail_change_grid)

ggplot(to_plot) +
  geom_bar(aes(dataset_pair , fill = agreement), position = "fill") 

ggplot(to_plot) +
  geom_bar(aes(dataset_pair, fill = agreement), position = "fill") +
  facet_wrap(~region)

avail_change_agreement_grid <- avail_change_grid[agreement != "Uknown", .N, by = .(agreement, dataset_pair, region)][
  , agree_ratio := N / sum(N), by = .(dataset_pair, region)]

highest_agreement_grid <- avail_change_agreement_grid[agreement == 'yes', .SD[which.max(agree_ratio)], by = region]

#Figures
to_plot <- merge(highest_agreement_grid[, .(dataset_pair, region)], 
                 avail_change_grid, by = c("dataset_pair", 'region'), allow.cartesian = TRUE)
to_plot[slope > 0 & tws_slope > 0,  agreement := factor("yes")]
to_plot[slope < 0 & tws_slope < 0,  agreement := factor("yes")]
to_plot[slope > 0 & tws_slope < 0,  agreement := factor("no")]
to_plot[slope < 0 & tws_slope > 0,  agreement := factor("no")]

test <- merge(to_plot, avail_flux_change, by = c('lon', 'lat', 'dataset_pair'))
test[, Conditions := factor("Uknown")]
levels(test$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
test[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
test[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
test[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
test[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]





#Map of highest agreement dataset pair per region
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

ggplot(test) +
  geom_point(aes(x = lon, y = lat, col = Conditions)) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  theme_minimal()

