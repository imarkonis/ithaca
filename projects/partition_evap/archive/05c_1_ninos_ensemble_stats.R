# Evap ensemble stats according to ENSO events----
source('source/partition_evap.R')

# https://psl.noaa.gov/enso/past_events.html
el_nino_years <- c(2003, 2007, 2010, 2016)
la_nina_years <- c(2000, 2008, 2011, 2012)
neutral_years <- c(2001, 2002, 2004, 2005, 2006, 2009, 2013, 2014, 2015, 2017, 2018, 2019)

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))
evap_datasets[, year := as.numeric(as.character(year))]
evap_datasets[year %in% el_nino_years, ENSO := "el_nino"]
evap_datasets[year %in% la_nina_years, ENSO := "la_nina"]
evap_datasets[year %in% neutral_years, ENSO := "neutral"]


## Categorise data ----

evap_datasets_el_nino <- evap_datasets[ENSO == "el_nino"]
evap_datasets_la_nina <- evap_datasets[ENSO == "la_nina"]
evap_datasets_neutral <- evap_datasets[ENSO == "neutral"]

evap_datasets_el_nino <- evap_datasets_el_nino[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_la_nina <- evap_datasets_la_nina[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]
evap_datasets_neutral <- evap_datasets_neutral[, .(evap_mean = mean(evap)), .(lon, lat, dataset)]

## Calculate stats ----

## Functions ----
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Analysis ----
### el_nino group ----
evap_ens_stats_el_nino <- evap_datasets_el_nino[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_el_nino[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_el_nino <- merge(evap_ens_stats_el_nino, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_el_nino[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_el_nino <- merge(evap_ens_stats_el_nino, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_el_nino[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_el_nino <- merge(evap_ens_stats_el_nino, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_el_nino[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_el_nino <- merge(evap_ens_stats_el_nino, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_el_nino[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_el_nino <- merge(evap_ens_stats_el_nino, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_el_nino[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_el_nino[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_el_nino, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_el_nino.rds"))

### la_nina group ----
evap_ens_stats_la_nina <- evap_datasets_la_nina[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_la_nina[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_la_nina <- merge(evap_ens_stats_la_nina, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_la_nina[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_la_nina <- merge(evap_ens_stats_la_nina, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_la_nina[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_la_nina <- merge(evap_ens_stats_la_nina, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_la_nina[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_la_nina <- merge(evap_ens_stats_la_nina, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_la_nina[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_la_nina <- merge(evap_ens_stats_la_nina, dummy, by = c('lon', 'lat'))

### Bias measures ----
evap_ens_stats_la_nina[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_la_nina[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_la_nina, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_la_nina.rds"))


### neutral group ----
evap_ens_stats_neutral <- evap_datasets_neutral[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_datasets_neutral[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats_neutral <- merge(evap_ens_stats_neutral, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_neutral[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_neutral <- merge(evap_ens_stats_neutral, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_neutral[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats_neutral <- merge(evap_ens_stats_neutral, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_neutral[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_neutral <- merge(evap_ens_stats_neutral, dummy, by = c('lon', 'lat'))

dummy <- evap_datasets_neutral[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats_neutral <- merge(evap_ens_stats_neutral, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats_neutral[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats_neutral[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats_neutral, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_neutral.rds"))