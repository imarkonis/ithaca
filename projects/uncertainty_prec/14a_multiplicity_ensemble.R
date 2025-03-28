# Time Series
source("source/uncertainty_prec.R")

registerDoParallel(cores = 32)

prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_time_series.rds"))

prec_data[dataset == "era5", dataset := "era-5"]

PREC_REPS <- c("cmap", "cpc", "cru-ts-v4-07", "em-earth", "era5-land", "fldas",
               "gpcp-v3-2", "jra55", "ncep-doe", "precl")

ALL_COMBS <- combn(unique(prec_data$dataset), 7, simplify = FALSE)

REP_COMBS <- combn(PREC_REPS, 7, simplify = FALSE)

CMAP_FAM <- grepl("cmap", ALL_COMBS) & grepl("cpc", ALL_COMBS) &
  grepl("cmorph-cdr", ALL_COMBS) & grepl("gsmap", ALL_COMBS)
CMAP_FAM <- ALL_COMBS[CMAP_FAM]

REAS_FAM <- grepl("era-5", ALL_COMBS) & grepl("era5-land", ALL_COMBS) &
  grepl("jra55", ALL_COMBS) & grepl("em-earth", ALL_COMBS)
REAS_FAM <- ALL_COMBS[REAS_FAM]

bootstrap_all <- foreach (idx = 1:length(ALL_COMBS), .combine = rbind) %dopar% {
  dummie_set <- ALL_COMBS[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = median(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

bootstrap_rep <- foreach (idx = 1:length(REP_COMBS), .combine = rbind) %dopar% {
  dummie_set <- REP_COMBS[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = median(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

bootstrap_cmap <- foreach(idx = 1:length(CMAP_FAM), .combine = rbind) %dopar% {
  dummie_set <- CMAP_FAM[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = median(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

bootstrap_reas <- foreach(idx = 1:length(REAS_FAM), .combine = rbind) %dopar% {
  dummie_set <- REAS_FAM[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = median(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

save(bootstrap_all, bootstrap_rep, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                                 "multiplicity_ensemble.rda"))

saveRDS(bootstrap_cmap, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                      "multiplicity_cmap.rds"))

saveRDS(bootstrap_reas, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                      "multiplicity_reas.rds"))
