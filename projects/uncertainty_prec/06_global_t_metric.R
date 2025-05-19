# Calculate T-metric
source("source/uncertainty_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_roi.rds"))

PREC_REPS <- c("cmap", "cpc-global", "cru-ts-v4-08", "em-earth", "era5-land",
               "fldas", "gpcp-cdr-v3-2", "gpm-imerg-v7", "jra55", "ncep-doe",
               "precl")

## Analysis
prec_mean <- prec_data[dataset %in% PREC_REPS]
prec_mean <- prec_mean[, .(mean_prec = median(prec, na.rm = TRUE)),
                       .(lon, lat, date)]

saveRDS(prec_mean, paste0(PATH_SAVE_UNCERTAINTY_PREC, "pRecipe_ensemble.rds"))

prec_data <- merge(prec_mean, prec_data, by = c("lon", "lat", "date"),
                   allow.cartesian = TRUE)

prec_data <- prec_data[, .(mse_prec = mean((mean_prec - prec)^2, na.rm = TRUE),
                           r_prec = cor(prec, mean_prec, use = "pairwise.complete.obs"),
                           bias_prec = mean(prec, na.rm = TRUE) - mean(mean_prec, na.rm = TRUE),
                           var_prec = sd(prec, na.rm = TRUE)^2,
                           mean_var = sd(mean_prec, na.rm = TRUE)^2),
                       .(lon, lat, dataset)]

prec_data <- prec_data[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + mean_var)))),
                         .(lon, lat, dataset)]

prec_data[t_prec < 0, t_prec := 0]

## Save
saveRDS(prec_data, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric.rds"))
