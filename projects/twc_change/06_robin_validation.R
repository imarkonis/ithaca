source('source/twc_change.R')

robin_meta <- fread('~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv')
robin_coords <- readRDS(paste0(PATH_OUTPUT_RAW, 'robin_coords.rds'))
avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_year.Rds'))
runoff_year <- readRDS('~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_yearly.rds')

avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))
avail_flux_change_weighted <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid_weighted.rds'))
avail_flux_change_best_performer_kg <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_best_performer_kg.rds'))
#avail_flux_change_hybrid <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_hybrid.rds'))

avail_flux_change_weighted$dataset <- factor("WEIGHTED")
avail_flux_change_best_performer_kg$dataset <- factor("BEST KG")
#avail_flux_change_hybrid$dataset <- factor("HYBRID")

avail_flux_change <- rbind(avail_flux_change, avail_flux_change_weighted)
avail_flux_change <- rbind(avail_flux_change, avail_flux_change_best_performer_kg)
#avail_flux_change <- rbind(avail_flux_change, avail_flux_change_hybrid)

robin_coords <- merge(robin_coords, robin_meta[, .(robin_id = ROBIN_ID, area = AREA, hydrobelt = HYDROBELT)])

runoff_periods <- runoff_year[year >= year(START_PERIOD_1) & year <= year(END_PERIOD_2)]
runoff_periods[, period := ordered('pre_2001')]
runoff_periods[year > year(END_PERIOD_1), period := ordered('aft_2001')]

runoff_change <- runoff_periods[, .(value = mean(flow)), .(robin_id, period)]
runoff_change_wide <- dcast(runoff_change, robin_id~period)
runoff_change_wide[, runoff_diff := aft_2001 - pre_2001]

water_avail_change_robin <- merge(avail_flux_change, robin_coords, by = c('lon', 'lat'))
water_avail_change_robin <- water_avail_change_robin[, .(water_avail_change_mean = mean(avail_change)), .(robin_id, dataset, hydrobelt)]
water_avail_change_robin <- merge(water_avail_change_robin, runoff_change_wide[, .(robin_id, runoff_diff)], by = 'robin_id') 

water_avail_change_robin[, residual := water_avail_change_mean - runoff_diff]

water_avail_change_robin[, sign_agreement := TRUE]
water_avail_change_robin[water_avail_change_mean  * runoff_diff < 0, sign_agreement := FALSE]

sign_agreement_change <- water_avail_change_robin[sign_agreement == TRUE, table(sign_agreement), dataset]
sign_agreement_change <- sign_agreement_change[order(V1, decreasing = TRUE)] 
sign_agreement_change[, V1 / nrow(unique(water_avail_change_robin[, .(robin_id)]))]

water_avail_robin_min_resid <- water_avail_change_robin[sign_agreement == TRUE, .SD[which.min(abs(residual))], by = .(robin_id)]
water_avail_robin_min_resid[, residual_ratio := residual / water_avail_change_mean ]  

residual_ranking <- water_avail_robin_min_resid[, mean(abs(residual), na.rm = T), .(dataset)]
residual_ranking[order(V1), ]

residual_ranking <- water_avail_change_robin[, mean(abs(residual), na.rm = T), .(dataset, hydrobelt)]
residual_ranking[hydrobelt == 4, .SD[order(V1)]]

n_sample <- 10000
robin_sample <- water_avail_robin_min_resid[, .SD[sample(.N, min(.N, n_sample))], by = hydrobelt]
residual_ranking <- robin_sample[, mean(abs(residual), na.rm = T), .(dataset)]
residual_ranking[order(V1), ]

rmse_entropy_rmse <- robin_sample[, {
  sampled_residuals <- sample(residual, n_sample, replace = TRUE)
  rmse_value <- sqrt(mean(sampled_residuals^2, na.rm = TRUE))
  list(rmse = rmse_value)
}, by = .(hydrobelt, dataset)]
residual_ranking <- rmse_entropy_rmse[, mean(rmse, na.rm = T), .(dataset)]
residual_ranking[order(V1), ]

saveRDS(avail_flux_change, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_grid_all.rds'))

#TO DIFFERENT FILE - ARCHIVE?

runoff_mean <- runoff_year[, .(runoff = mean(flow)), robin_id]

water_avail <-  avail_flux[, .(water_avail = mean(water_avail)), .(lon, lat, dataset)]
water_avail_robin <- merge(water_avail, robin_coords, by = c('lon', 'lat'))
water_avail_robin[, water_avail_mean := mean(water_avail), .(robin_id, dataset)]
water_avail_robin_mean <- merge(runoff_mean, water_avail_robin[, .(robin_id, dataset, water_avail_mean, hydrobelt)], allow.cartesian = TRUE)
water_avail_robin_mean[, residual := water_avail_mean - runoff]

mean_residuals_dataset <- water_avail_robin_mean[, .(residual = mean(abs(residual))), dataset]
mean_residuals_dataset[order(residual)]
water_avail_robin_mean[,  mean(abs(residual))/mean(water_avail_mean), hydrobelt]

min_residuals <- water_avail_robin_mean[ , .SD[which.min(abs(residual))], by = .(robin_id, hydrobelt)]
min_residuals[, table(dataset)]
min_residuals_datasets <- min_residuals[, .(robin_id, dataset, water_avail_mean, residual)]
min_residuals[,  mean(abs(residual))/mean(water_avail_mean), hydrobelt]

min_residuals_mean <- min_residuals[, mean(abs(residual)), dataset]
min_residuals_mean[order(V1, decreasing = FALSE)]
min_residuals_mean

water_avail_change_mean <- water_avail_change_robin[, .(avail_change = mean(avail_change), flux_change = mean(flux_change)), .(robin_id, area, dataset, hydrobelt)]
water_avail_change_catchments <- merge(water_avail_change_mean, min_residuals_datasets)
acceleration_catchment_ids <- water_avail_change_catchments[flux_change > 0, robin_id]

to_plot <- copy(water_avail_change_catchments)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Drier - Accelerated', 'Wetter - Deccelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

ggplot(avail_flux_change[dataset == 'EARTH-GLEAM']) +
  geom_point(aes(x = avail_change, flux_change, col = Conditions))

ggplot(avail_flux_change[dataset == 'CPC-MERRA']) +
  geom_point(aes(x = avail_change, flux_change))

ggplot(avail_flux_change[dataset == 'GPCC-TERRA']) +
  geom_point(aes(x = avail_change, flux_change, col = lat))

ggplot(to_plot) +
  geom_point(aes(x = avail_change, flux_change, col = Conditions)) +
  scale_color_manual(values = PALETTES$water_cycle_change[c(1, 3, 2, 4)]) +
  theme_light()

ggplot(to_plot) +
  geom_bar(aes(hydrobelt, fill = Conditions)) +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 3, 2, 4)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(to_plot) +
  geom_point(aes(log(abs(residual)), log(area), col = factor(hydrobelt)))


# Water availability change

runoff_periods <- runoff_year[year >= year(START_PERIOD_1) & year <= year(END_PERIOD_2)]
runoff_periods[, period := ordered('pre_2001')]
runoff_periods[year > year(END_PERIOD_1), period := ordered('aft_2001')]

runoff_change <- runoff_periods[, .(value = mean(flow)), .(robin_id, period)]
runoff_change_wide <- dcast(runoff_change, robin_id~period)
runoff_change_wide[, runoff_diff := aft_2001 - pre_2001]

water_avail_change_robin <- merge(avail_flux_change, robin_coords, by = c('lon', 'lat'))
water_avail_change_robin <- water_avail_change_robin[, .(water_avail_change_mean = mean(avail_change)), .(robin_id, dataset)]
water_avail_change_robin <- merge(water_avail_change_robin, runoff_change_wide[, .(robin_id, runoff_diff)], by = 'robin_id') 

water_avail_change_robin[, residual := water_avail_change_mean - runoff_diff]

water_avail_change_robin[, sign_agreement := TRUE]
water_avail_change_robin[water_avail_change_mean  * runoff_diff < 0, sign_agreement := FALSE]

sign_agreement_change <- water_avail_change_robin[sign_agreement == TRUE, table(sign_agreement), dataset]
sign_agreement_change <- sign_agreement_change[order(V1, decreasing = TRUE)] 
sign_agreement_change[, V1 / nrow(unique(water_avail_change_robin[, .(robin_id)]))]

water_avail_change_robin[, min(abs(residual))/water_avail_change_mean, .(robin_id)]
water_avail_robin_min_resid <- water_avail_change_robin[ , .SD[which.min(abs(residual))], by = .(robin_id)]
water_avail_robin_min_resid[, residual_ratio := residual / water_avail_change_mean ]  

residual_ranking <- water_avail_change_robin[, mean(abs(residual), na.rm = T), dataset]
residual_ranking[order(V1),]
### Similar but for abs difference per year to avoid masking mean biases (slower)

avail_flux_year <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_year.rds'))

water_avail_robin_year <- merge(avail_flux_year, robin_coords, by = c('lon', 'lat'))
water_avail_robin_year <- merge(runoff_year, water_avail_robin_year, allow.cartesian = TRUE)
setnames(water_avail_robin_year, "flow", "runoff")

water_avail_robin_year[, residual := avail - runoff]
mean_residuals_dataset_year <- water_avail_robin_year[, .(residual = mean(abs(residual))), dataset]
mean_residuals_dataset_year[order(residual)]

min_residuals_year <- water_avail_robin_year[ , .SD[which.min(abs(residual))], by = .(lon, lat, year)]
min_residuals_mean_year <- min_residuals_year[, mean(abs(residual)), dataset]
min_residuals_mean_year[order(V1)]
min_residuals_year[, table(dataset)]


