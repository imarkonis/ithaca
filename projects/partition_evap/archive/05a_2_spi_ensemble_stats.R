# Dividing soil moisture data according to SPI based on GPCC data----
source('source/partition_evap.R')

## Load data ----
spi <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "spi_200001_201912_gpcc.rds"))
spi[, year := as.numeric(year(time))]
spi[, time := NULL]
spi[, rank_spi := rank(value), .(x, y)]

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))
evap_datasets[, year := as.numeric(as.character(year))]

## merge data ----
evap_datasets_spi <- merge(evap_datasets, spi, by.y =  c("x", "y", "year"), by.x = c("lon", "lat", "year"), all.x = TRUE)
evap_datasets_spi <- evap_datasets_spi[complete.cases(evap_datasets_spi)]

## Categorise data ----

evap_datasets_dry <- evap_datasets_spi[value < -1]
evap_datasets_normal <- evap_datasets_spi[value >= -1 & value <= 1]
evap_datasets_wet <- evap_datasets_spi[value > 1]

evap_datasets_dry_rank <- evap_datasets_spi[rank_spi < 6, ]
evap_datasets_wet_rank <- evap_datasets_spi[rank_spi > 14, ]

evap_datasets_dry_mean <- evap_datasets_dry[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_normal_mean <- evap_datasets_normal[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_wet_mean <- evap_datasets_wet[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]

evap_datasets_dry_rank_mean <- evap_datasets_dry_rank[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_wet_rank_mean <- evap_datasets_wet_rank[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]

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

saveRDS(evap_ens_stats_dry, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_spi.rds"))


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

### Bias measures
evap_ens_stats_wet[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_wet[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_wet, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_spi.rds"))

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

saveRDS(evap_ens_stats_normal, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_normal_spi.rds"))


## Dry rank ----
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

saveRDS(evap_ens_stats_dry, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_rank_spi.rds"))

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

saveRDS(evap_ens_stats_wet, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_rank_spi.rds"))
