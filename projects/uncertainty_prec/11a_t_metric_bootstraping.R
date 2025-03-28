# Calculate T-metric
source("source/uncertainty_prec.R")

registerDoParallel(cores = 32)

## Data
prec_data_all <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                "prec_data_roi.rds"))

prec_data_all[, coord_id := .GRP, by = c("lon", "lat")]

#prec_data_all <- prec_data_all[lon >= -124.736342 & lon <= -66.945392 & lat >= 24.521208 & lat <= 49.382808]

PREC_REPS <- c("cmap", "cpc", "cru-ts-v4-07", "em-earth", "era5-land", "fldas",
               "gpcp-v3-2", "jra55", "ncep-doe", "precl")

ALL_COMBS <- combn(PREC_REPS, 7, simplify = FALSE)

COORD_IDX <- max(prec_data_all$coord_id)

## Analysis
bootstrap_data <- foreach (idx = 1:COORD_IDX, .combine = rbind) %dopar% {
  dummie_prec <- prec_data_all[coord_id == idx]
  prec_median <- lapply(ALL_COMBS, function(combination_idx){
    x <- dummie_prec[dataset %in% combination_idx
                     ][, .(median_prec = median(prec, na.rm = TRUE)),
                       .(lon, lat, date)]
    return(x)
  })
  prec_median <- rbindlist(prec_median, idcol = "combination_idx")
  
  prec_median <- merge(prec_median, dummie_prec, by = c("lon", "lat", "date"),
                       allow.cartesian = TRUE)
  prec_median <- prec_median[, .(mse_prec = mean((median_prec - prec)^2, na.rm = TRUE),
                                 r_prec = cor(prec, median_prec, use = "pairwise.complete.obs"),
                                 bias_prec = mean(prec, na.rm = TRUE) - mean(median_prec, na.rm = TRUE),
                                 var_prec = sd(prec, na.rm = TRUE)^2,
                                 median_var = sd(median_prec, na.rm = TRUE)^2),
                             .(lon, lat, dataset, combination_idx)]
  prec_median <- prec_median[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                             .(lon, lat, dataset, combination_idx)]
  prec_median[t_prec < 0, t_prec := 0]
  return(prec_median)
}

## Save
saveRDS(bootstrap_data, paste0(PATH_SAVE_UNCERTAINTY_PREC,
                               "t_metric_bootstrap.rds"))
