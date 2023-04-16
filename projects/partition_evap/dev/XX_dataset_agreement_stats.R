source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_ens_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
prec_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))

dataset_agreement <- merge(prec_ens_stats, prec_mean_datasets, by = c('lon', 'lat'))
dataset_agreement[ens_mean_mean != 0, ratio_to_ens_mean := prec_mean / ens_mean_mean]
dataset_agreement[!is.infinite(ratio_to_ens_mean), mean(ratio_to_ens_mean, na.rm = T), dataset]

dataset_agreement[, within_interquantile := FALSE]
dataset_agreement[prec_mean >= ens_mean_q25 & prec_mean <= ens_mean_q75, within_interquantile := TRUE]

dataset_agreement[, underestimates := FALSE]
dataset_agreement[prec_mean < ens_mean_q25 , underestimates := TRUE]

dataset_agreement[, overestimates := FALSE]
dataset_agreement[prec_mean > ens_mean_q75 , overestimates := TRUE]

dataset_agreement[within_interquantile == TRUE, .N, .(lat, lon)]
dataset_agreement[underestimates == TRUE, .N, .(lat, lon)]
dataset_agreement[overestimates == TRUE, .N, .(lat, lon)]

dummy <- dataset_agreement[, .(lon, lat, dataset, dataset_type, ratio_to_ens_mean, within_interquantile, 
                                underestimates, overestimates)]
dataset_agreement_summary <- dummy[, .(sample_size = .N), dataset]
dataset_agreement_summary <- merge(dataset_agreement_summary, dummy[within_interquantile == TRUE, 
                                       .(within_interquantile = .N), dataset], 'dataset')
dataset_agreement_summary <- merge(dataset_agreement_summary, dummy[underestimates == TRUE, 
                                       .(underestimates = .N), dataset], 'dataset')
dataset_agreement_summary <- merge(dataset_agreement_summary, dummy[overestimates == TRUE, 
                                       .(overestimates = .N), dataset], 'dataset')

dataset_agreement_summary[, overestimates := round(overestimates / sample_size, 2)]
dataset_agreement_summary[, within_interquantile := round(within_interquantile / sample_size, 2)]
dataset_agreement_summary[, underestimates := round(underestimates / sample_size, 2)]
dataset_agreement_summary[, sample_size := NULL]
dataset_agreement_summary <- dataset_agreement_summary[order(-within_interquantile)]

dataset_sd_comparison <- merge(prec_ens_stats[, .(lon, lat, ens_mean_median, ens_mean_sd)], prec_mean_datasets, by = c('lon', 'lat'))
dataset_sd_comparison[, mean(prec_sd), dataset]
dataset_sd_comparison[, mean(ens_mean_sd)]

