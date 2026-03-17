source('source/twc_change.R')

dataset_weights <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_weights.Rds'))

water_avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_periods.Rds'))
water_avail_slopes <- readRDS(paste0(PATH_OUTPUT_DATA, 'slopes_avail_grid.rds'))

sm_cci <- readRDS(paste0(PATH_OUTPUT_RAW, '/other/esa-cci_yearly.Rds'))
tws_grace <- readRDS(paste0(PATH_OUTPUT_DATA, 'tws_slopes_grid.Rds'))

### Water availability from datasets 
water_avail_flux_wide <- dcast(water_avail_flux, lon + lat + dataset ~ period, value.var = "avail")
water_avail_flux_wide[, avail_change_ratio := (aft_2001 - pre_2001) / pre_2001]
water_avail_change <- water_avail_flux_wide[, .(lon, lat, dataset, avail_change_ratio)]

### Soil moisture from cci 
sm_cci[, period := ordered('pre_2001')]
sm_cci[date > END_PERIOD_1, period := ordered('aft_2001')]

sm_period_mean <- sm_cci[, .(value = mean(value)), .(lon, lat, period)]
sm_period_mean_wide <- dcast(sm_period_mean, lon + lat ~ period, value.var = "value")
sm_period_mean_wide[, sm_change_ratio := (aft_2001 - pre_2001) / pre_2001]

water_avail_change <- merge(water_avail_change, sm_period_mean_wide[, .(lon, lat, sm_change_ratio)], 
                           by = c('lon', 'lat'), allow.cartesian = TRUE)
water_avail_slopes <- merge(water_avail_slopes[, .(lon, lat, dataset, avail_slope = slope)], tws_grace[, .(lon, lat, tws_slope = slope)], 
      by = c('lon', 'lat'), allow.cartesian = TRUE)

eval_weights <- merge(
    water_avail_change, 
    water_avail_slopes, 
    by = c("lon", "lat", "dataset")
  )

# Evaluation metrics
eval_weights[, relbias_cci := (avail_change_ratio - sm_change_ratio) / sm_change_ratio]
eval_weights[, sign_agree_cci := as.numeric(sign(avail_change_ratio) == sign(sm_change_ratio))]

eval_weights[, relbias_grace := (avail_slope  - tws_slope) / tws_slope]
eval_weights[, sign_agree_grace := as.numeric(sign(avail_slope) == sign(tws_slope))]

#Handle possible division by zero (where grace_change_ratio or sm_change_ratio == 0)
eval_weights[is.na(relbias_cci)   | is.infinite(relbias_cci), relbias_cci := 0]
eval_weights[is.na(relbias_grace) | is.infinite(relbias_grace), relbias_grace := 0]
  
# Normalize relbias within each grid cell (lon-lat group)
eval_weights[, relbias_cci_norm   := abs(relbias_cci) / quantile(abs(relbias_cci), 0.95), by = .(lon, lat)]
eval_weights[, relbias_grace_norm := abs(relbias_grace) / quantile(abs(relbias_grace), 0.95), by = .(lon, lat)]

#Weights
alpha_grace <- 1.0 
beta_grace  <- 2.0
alpha_cci   <- 1.0
beta_cci    <- 2.0
lambda      <- 2 #to minimize the probability of low scores

# Compute score
eval_weights[, score := (alpha_grace * relbias_grace_norm + beta_grace * (1 - sign_agree_grace) +
                   alpha_cci * relbias_cci_norm + beta_cci * (1 - sign_agree_cci)) / 2]
  
# Exponentiate score to get unnormalized weight
eval_weights[, weight_raw := exp(-lambda * score)]
  
# Normalize weights within each grid cell
eval_weights[, eval_weight := weight_raw / sum(weight_raw), by = .(lon, lat)]

to_save <- merge(dataset_weights, eval_weights[, .(lon, lat, dataset, eval_weight)], allow.cartesian = TRUE)

to_save[, weight := value_weight * change_weight * eval_weight]
to_save[is.na(weight), weight := value_weight * change_weight]
to_save[, weight := weight / sum(weight), by = .(lon, lat)]

saveRDS(to_save, file = paste0(PATH_OUTPUT_DATA, 'dataset_weights.Rds'))
     