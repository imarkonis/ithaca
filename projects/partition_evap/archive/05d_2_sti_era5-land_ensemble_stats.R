# Dividing soil moisture data according to sti based on era5-land data----
source('source/partition_evap.R')

## Load data ----
sti <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "sti_200001_201912_era5-land.rds"))
sti[, year := as.numeric(year(time))]
sti[, time := NULL]
sti[, rank_sti := rank(value), .(x, y)]

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))
evap_datasets[, year := as.numeric(as.character(year))]

## merge data ----
evap_datasets_sti <- merge(evap_datasets, sti, by.y =  c("x", "y", "year"), by.x = c("lon", "lat", "year"), all = TRUE)
evap_datasets_sti <- evap_datasets_sti[complete.cases(evap_datasets_sti)]


## Categorise data ----


evap_datasets_cold <- evap_datasets_sti[value < -1]
evap_datasets_normal <- evap_datasets_sti[value >= -1 & value <= 1]
evap_datasets_warm <- evap_datasets_sti[value > 1]

evap_datasets_cold_rank <- evap_datasets_sti[rank_sti < 6, ]
evap_datasets_warm_rank <- evap_datasets_sti[rank_sti > 14, ]

evap_datasets_cold_mean <- evap_datasets_cold[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_normal_mean <- evap_datasets_normal[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_warm_mean <- evap_datasets_warm[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]

evap_datasets_cold_rank_mean <- evap_datasets_cold_rank[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_warm_rank_mean <- evap_datasets_warm_rank[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]

## rank data ----

evap_datasets_cold <- evap_datasets_sti[value < -1]
evap_datasets_normal <- evap_datasets_sti[value >= -1 & value <= 1]
evap_datasets_warm <- evap_datasets_sti[value > 1]

# Ensemble stats

## Functions ----
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Analysis ----
### cold group ----
evap_ens_stats_cold <- evap_datasets_cold_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_cold_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_cold[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_cold[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_cold, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_cold_sti_era5-land.rds"))

### warm group ----
evap_ens_stats_warm <- evap_datasets_warm_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_warm_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

### Bias measures ----
evap_ens_stats_warm[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_warm[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_warm, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_warm_sti_era5-land.rds"))


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

saveRDS(evap_ens_stats_normal, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_normal_sti_era5-land.rds"))

## cold rank 
evap_ens_stats_cold <- evap_datasets_cold_rank_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_cold_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_cold_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_cold <- merge(evap_ens_stats_cold, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_cold[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_cold[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_cold, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_cold_rank_sti_era5-land.rds"))

### warm rank ----
evap_ens_stats_warm <- evap_datasets_warm_rank_mean[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_warm_mean[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_warm_mean[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_warm <- merge(evap_ens_stats_warm, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_warm[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_warm[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_warm, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_warm_rank_sti_era5-land.rds"))
