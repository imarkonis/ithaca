# Dividing soil moisture data according to ssi based on era5-land data----
source('source/partition_evap.R')

## Load data ----
ssi <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ssi_200001_201912_era5-land.rds"))
ssi[, year := as.numeric(year(time))]
ssi[, time := NULL]
ssi[, rank_ssi := rank(value), .(x, y)]

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))
evap_datasets[, year := as.numeric(as.character(year))]

## merge data ----
evap_datasets_ssi <- merge(evap_datasets, ssi, by.y =  c("x", "y", "year"), by.x = c("lon", "lat", "year"), all = TRUE)
evap_datasets_ssi <- evap_datasets_ssi[complete.cases(evap_datasets_ssi)]


## Categorise data ----


evap_datasets_dry <- evap_datasets_ssi[value < -1]
evap_datasets_normal <- evap_datasets_ssi[value >= -1 & value <= 1]
evap_datasets_wet <- evap_datasets_ssi[value > 1]

evap_datasets_dry_rank <- evap_datasets_ssi[rank_ssi < 6, ]
evap_datasets_wet_rank <- evap_datasets_ssi[rank_ssi > 14, ]

evap_datasets_dry_mean <- evap_datasets_dry[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_normal_mean <- evap_datasets_normal[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_wet_mean <- evap_datasets_wet[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]

evap_datasets_dry_rank_mean <- evap_datasets_dry_rank[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_wet_rank_mean <- evap_datasets_wet_rank[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]

## rank data ----

evap_datasets_dry <- evap_datasets_ssi[value < -1]
evap_datasets_normal <- evap_datasets_ssi[value >= -1 & value <= 1]
evap_datasets_wet <- evap_datasets_ssi[value > 1]

# Ensemble stats

## Functions ----
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Analysis ----
### Dry group ----
evap_ens_stats_dry <- evap_datasets_dry_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_dry_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_dry[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_dry[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_dry, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_ssi_era5-land.rds"))

### Wet group ----
evap_ens_stats_wet <- evap_datasets_wet_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_wet_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

### Bias measures ----
evap_ens_stats_wet[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_wet[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_wet, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_ssi_era5-land.rds"))


### Normal group ----
evap_ens_stats_normal <- evap_datasets_normal_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_normal_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_normal <- merge(evap_ens_stats_normal, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_normal_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_normal <- merge(evap_ens_stats_normal, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_normal_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_normal <- merge(evap_ens_stats_normal, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_normal_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_normal <- merge(evap_ens_stats_normal, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_normal_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_normal <- merge(evap_ens_stats_normal, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_normal[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_normal[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_normal, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_normal_ssi_era5-land.rds"))

## Dry rank 
evap_ens_stats_dry <- evap_datasets_dry_rank_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_dry_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_dry_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_dry <- merge(evap_ens_stats_dry, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_dry[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_dry[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_dry, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_rank_ssi_era5-land.rds"))

### Wet rank ----
evap_ens_stats_wet <- evap_datasets_wet_rank_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_wet_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_wet_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_wet <- merge(evap_ens_stats_wet, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_wet[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_wet[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_wet, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_rank_ssi_era5-land.rds"))
