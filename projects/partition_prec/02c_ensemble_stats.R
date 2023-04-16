# Estimation of ensemble statistics
source('source/partition_prec.R')
source('source/geo_functions.R')

## Data
prec_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))

## Functions
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Analysis
prec_ens_stats <- prec_mean_datasets[, .(ens_mean_mean = round(mean(prec_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- prec_mean_datasets[, .(ens_mean_median = round(median(prec_mean, na.rm = TRUE), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- prec_mean_datasets[, .(ens_mean_sd = round(sd(prec_mean, na.rm = TRUE), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- prec_mean_datasets[, .(ens_mean_q25 = round(estimate_q25(prec_mean), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- prec_mean_datasets[, .(ens_mean_q75 = round(estimate_q75(prec_mean), 2)), .(lat, lon)]
prec_ens_stats <- merge(prec_ens_stats, dummy, by = c('lon', 'lat'))

### Bias measures
prec_ens_stats[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
prec_ens_stats[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(prec_ens_stats, paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
