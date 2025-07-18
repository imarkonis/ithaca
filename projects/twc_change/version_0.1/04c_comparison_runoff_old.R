source('source/twc_change.R')

robin_coords <- readRDS(paste0(PATH_OUTPUT_RAW, 'robin_coords.rds'))
avail_flux <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_grid.rds'))
runoff_year <- readRDS('~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_yearly.rds')
avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))

runoff_mean <- runoff_year[, .(runoff = mean(flow)), robin_id]
water_avail <-  avail_flux[, .(water_avail = mean(water_avail)), .(lon, lat, dataset_pair)]
water_avail_robin <- merge(water_avail, robin_coords, by = c('lon', 'lat'))
water_avail_robin <- merge(runoff_mean, water_avail_robin, allow.cartesian = TRUE)

water_avail_robin[, residual := water_avail - runoff]
mean_residuals_dataset <- water_avail_robin[, .(residual = mean(abs(residual))), dataset_pair]
mean_residuals_dataset[order(residual)]

min_residuals <- water_avail_robin[ , .SD[which.min(abs(residual))], by = .(lon, lat)]
min_residuals_mean <- min_residuals[, mean(residual), dataset_pair]
min_residuals_mean[order(V1, decreasing = TRUE)]
min_residuals[, table(dataset_pair)]

runoff_periods <- runoff_year[year >= year(START_PERIOD_1) & year <= year(END_PERIOD_2)]
runoff_periods[, period := ordered('pre_2001')]
runoff_periods[year > year(END_PERIOD_1), period := ordered('aft_2001')]

runoff_change <- runoff_periods[, .(value = mean(flow)), .(robin_id, period)]
runoff_change_wide <- dcast(runoff_change, robin_id~period)
runoff_change_wide[, runoff_diff := aft_2001 - pre_2001]
water_avail_change_robin <- merge(avail_flux_change, robin_coords, by = c('lon', 'lat'))
water_avail_change_robin <- merge(water_avail_change_robin, runoff_change_wide[, .(robin_id, runoff_diff)], by = 'robin_id') 

water_avail_change_robin[, sign_agreement := TRUE]
water_avail_change_robin[avail_change * runoff_diff < 0, sign_agreement := FALSE]

sign_agreement_change <- water_avail_change_robin[sign_agreement == TRUE, table(sign_agreement), dataset_pair]
sign_agreement_change <- sign_agreement_change[order(V1, decreasing = TRUE)] 
sign_agreement_change[, V1 / nrow(unique(water_avail_change_robin[, .(lon, lat)]))]

water_avail_robin[, min(abs(residual))/water_avail, .(lon, lat)]
water_avail_robin_min_resid <- water_avail_robin[ , .SD[which.min(abs(residual))], by = .(lon, lat)]
water_avail_robin_min_resid[, residual_ratio := residual / water_avail]  

ggplot(water_avail_robin_min_resid) +
  geom_point(aes(x = lon, y = lat, col = residual_ratio))

### Similar but for abs difference per year to avoid masking mean biases (slower)

avail_flux_year <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_year.rds'))

water_avail_robin_year <- merge(avail_flux_year, robin_coords, by = c('lon', 'lat'))
water_avail_robin_year <- merge(runoff_year, water_avail_robin_year, allow.cartesian = TRUE)
setnames(water_avail_robin_year, "flow", "runoff")

water_avail_robin_year[, residual := avail - runoff]
mean_residuals_dataset_year <- water_avail_robin_year[, .(residual = mean(abs(residual))), dataset_pair]
mean_residuals_dataset_year[order(residual)]

min_residuals_year <- water_avail_robin_year[ , .SD[which.min(abs(residual))], by = .(lon, lat, year)]
min_residuals_mean_year <- min_residuals_year[, mean(abs(residual)), dataset_pair]
min_residuals_mean_year[order(V1)]
min_residuals_year[, table(dataset_pair)]


