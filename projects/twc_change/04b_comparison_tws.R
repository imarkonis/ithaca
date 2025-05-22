source('source/twc_change.R')
install.packages('trend')
library(trend)

#Global comparison

water_storage <- readRDS(paste0(PATH_OUTPUT_RAW, 'other/grace_yearly_2019.Rds'))
twc_means_global <- water_storage[, .(value = mean(value)), year]

data_all <- readRDS(paste0(PATH_OUTPUT_RAW, 'data_all.Rds'))
data_all <- data_all[date >= '2002-01-01']

data_global_means <- data_all[, .(value = mean(value)), .(variable, date, dataset)]

dt_wide <- dcast(data_global_means, dataset + date ~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(date, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(date, dataset_evap = dataset, evap)]
merged <- merge(prec_dt, evap_dt, by = c("date"), allow.cartesian = TRUE)
merged[, pe_diff := prec - evap]
merged[, dataset_pair := paste(dataset_prec, dataset_evap, sep = "-")]

merged[, year := as.numeric(format(date, "%Y"))]

# Compute Theil-Sen slope per dataset_pair
merged[, year := as.numeric(format(date, "%Y"))]

slopes_pe_diff_global <- merged[, {
  fit <- sens.slope(pe_diff, year)
  .(slope = fit$estimates, p.value = fit$p.value)
}, by = dataset_pair]

#IPCC comparison
masks <- pRecipe::pRecipe_masks()
water_storage <- merge(water_storage, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)]) 
data_all <- merge(data_all, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)]) 
colnames(data_all)[7] <- 'region'
colnames(water_storage)[5] <- 'region'

tws_means_ipcc <- water_storage[, .(value = mean(value)), .(year, region)]

tws_slopes_ipcc <- tws_means_ipcc[, {
  fit <- sens.slope(value, year)
  .(slope = fit$estimates, p.value = fit$p.value)
}, by = region]

colnames(tws_slopes_ipcc)[2] <- 'tws_slope'

data_ipcc_means <- data_all[, .(value = mean(value)), .(variable, date, region, dataset)]

dt_wide <- dcast(data_ipcc_means, dataset + date + region ~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(date, region, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(date, region, dataset_evap = dataset, evap)]
merged <- merge(prec_dt, evap_dt, by = c("date", 'region'), allow.cartesian = TRUE)
merged[, pe_diff := prec - evap]
merged[, dataset_pair := paste(dataset_prec, dataset_evap, sep = "-")]


slopes_pe_diff_ipcc <- merged[, {
  fit <- sens.slope(pe_diff, year)
  .(slope = fit$estimates, p.value = fit$p.value)
}, by = .(dataset_pair, region)]

twc_change <- merge(slopes_pe_diff_ipcc[, .(region, dataset_pair, slope)], tws_slopes_ipcc[, .(region, tws_slope)])

twc_change[, agreement := factor("Uknown")]
twc_change[slope > 0 & tws_slope > 0,  agreement := factor("yes")]
twc_change[slope < 0 & tws_slope < 0,  agreement := factor("yes")]
twc_change[slope > 0 & tws_slope < 0,  agreement := factor("no")]
twc_change[slope < 0 & tws_slope > 0,  agreement := factor("no")]

ggplot(twc_change[agreement != "Uknown"]) +
  geom_bar(aes(agreement)) +
  facet_wrap(~dataset_pair)


#Grid cell comparison
data_all <- readRDS(paste0(PATH_OUTPUT_RAW, 'data_all.Rds'))
data_all <- data_all[, unique(data_all)] #WHY?????
data_all <- merge(data_all, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)])
colnames(data_all)[7] <- 'region'

dt_wide <- dcast(data_all, lon + lat + dataset + region + date ~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(lon, lat, date, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(lon, lat, date, dataset_evap = dataset, evap)]
merged <- merge(prec_dt, evap_dt, by = c("date", 'lon', 'lat'), allow.cartesian = TRUE)
merged[, pe_diff := prec - evap]
merged[, pe_mean := (prec + evap) / 2]
merged[, dataset_pair := paste(dataset_prec, dataset_evap, sep = "|")]
merged[, dataset_prec := NULL]
merged[, dataset_evap := NULL]
merged[, year := as.numeric(format(date, "%Y"))]

saveRDS(merged, file = paste0(PATH_OUTPUT, 'avail_flux_grid.rds') )

dataset_count <- 1
dummy_dataset <- merged[dataset_pair == dataset_pairs[dataset_count]]
slopes_pe_diff_grid <- dummy_dataset[, {
  if (length(unique(year)) > 1 && length(unique(pe_diff)) > 1) {
    fit <- sens.slope(pe_diff, year)
    .(slope = fit$estimates, p.value = fit$p.value)
  } else {
    .(slope = NA_real_, p.value = NA_real_)
  }
}, by = .(lon, lat)]
slopes_pe_diff_grid$dataset_pair <- dataset_pairs[dataset_count]

n_pairs <- length(dataset_pairs)

for(dataset_count in 3:n_pairs){
  dummy_dataset <- merged[dataset_pair == dataset_pairs[dataset_count]]
  result <- dummy_dataset[, {
    if (length(unique(year)) > 1 && length(unique(pe_diff)) > 1) {
      fit <- sens.slope(pe_diff, year)
      .(slope = fit$estimates, p.value = fit$p.value)
    } else {
      .(slope = NA_real_, p.value = NA_real_)
    }
  }, by = .(lon, lat)]
  result$dataset_pair <- dataset_pairs[dataset_count]
  slopes_pe_diff_grid <- rbind(slopes_pe_diff_grid, result)
  print(dataset_count)
}
saveRDS(slopes_pe_diff_grid, file = paste0(PATH_OUTPUT, 'slopes_pe_diff_grid.rds'))

tws_slopes_grid <- water_storage[, {
  fit <- sens.slope(value, year)
  .(slope = fit$estimates, p.value = fit$p.value)
}, by = .(lon, lat)]
colnames(tws_slopes_grid)[3] <- 'tws_slope'






twc_change_grid <- merge(slopes_pe_diff_grid, tws_slopes_grid[, .(lon, lat, tws_slope)], 
                         by = c('lon', 'lat'), allow.cartesian=TRUE)

twc_change_grid[, agreement := factor("Uknown")]
twc_change_grid[slope > 0 & tws_slope > 0,  agreement := factor("yes")]
twc_change_grid[slope < 0 & tws_slope < 0,  agreement := factor("yes")]
twc_change_grid[slope > 0 & tws_slope < 0,  agreement := factor("no")]
twc_change_grid[slope < 0 & tws_slope > 0,  agreement := factor("no")]

to_plot <- copy(twc_change_grid)

ggplot(to_plot) +
  geom_bar(aes(dataset_pair , fill = agreement), position = "fill") 

ggplot(to_plot) +
  geom_bar(aes(dataset_pair, fill = agreement), position = "fill") +
  facet_wrap(~region)

twc_change_agreement_grid <- twc_change_grid[agreement != "Uknown", .N, by = .(agreement, dataset_pair, region)][
  , agree_ratio := N / sum(N), by = .(dataset_pair, region)]

highest_agreement_grid <- twc_change_agreement_grid[agreement == 'yes', .SD[which.max(agree_ratio)], by = region]
highest_agree[N > 100]
