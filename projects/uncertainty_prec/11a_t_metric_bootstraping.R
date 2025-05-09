# Calculate T-metric
source("source/uncertainty_prec.R")

registerDoParallel(cores = N_CORES)

## Data
prec_data_all <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                "prec_data_roi.rds"))
prec_data_all[, coord_id := .GRP, by = c("lon", "lat")]

PREC_REPS <- c("cmap", "cpc-global", "cru-ts-v4-08", "em-earth", "era5-land",
               "fldas", "gpcp-cdr-v3-2", "gpm-imerg-v7", "jra55", "ncep-doe",
               "precl")

ALL_COMBS <- combn(PREC_REPS, 7, simplify = FALSE)

COORD_IDX <- max(prec_data_all$coord_id)

gc()
## Analysis
foreach (idx = 1:COORD_IDX) %dopar% {
  
  dummie_prec <- prec_data_all[coord_id == idx]
  
  prec_mean <- lapply(ALL_COMBS, function(combination_idx){
    x <- dummie_prec[dataset %in% combination_idx
                     ][, .(mean_prec = median(prec, na.rm = TRUE)),
                       .(lon, lat, date)]
    return(x)
  })
  prec_mean <- rbindlist(prec_mean, idcol = "combination_idx")
  prec_mean <- merge(prec_mean, dummie_prec, by = c("lon", "lat", "date"),
                     allow.cartesian = TRUE)
  prec_mean <- prec_mean[, .(mse_prec = mean((mean_prec - prec)^2, na.rm = TRUE),
                             r_prec = cor(prec, mean_prec, use = "pairwise.complete.obs"),
                             bias_prec = mean(prec, na.rm = TRUE) - mean(mean_prec, na.rm = TRUE),
                             var_prec = sd(prec, na.rm = TRUE)^2,
                             mean_var = sd(mean_prec, na.rm = TRUE)^2),
                         .(lon, lat, dataset, combination_idx)]
  prec_mean <- prec_mean[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + mean_var)))),
                         .(lon, lat, dataset, combination_idx)]
  prec_mean[t_prec < 0, t_prec := 0]
  
  fwrite(prec_mean,
         paste0(PATH_SAVE_UNCERTAINTY_PREC, "temp/tmp_", idx, ".csv"))
  
  rm(prec_mean, dummie_prec)
  
  gc()
  
}

rm(prec_data_all, ALL_COMBS)

gc()

bootstrap_data <- list.files(path = paste0(PATH_SAVE_UNCERTAINTY_PREC, "temp"),
                             full.names = TRUE)
bootstrap_data <- lapply(bootstrap_data, fread)
bootstrap_data <- rbindlist(bootstrap_data)

## Save
saveRDS(bootstrap_data, paste0(PATH_SAVE_UNCERTAINTY_PREC,
                               "t_metric_bootstrap.rds"))
