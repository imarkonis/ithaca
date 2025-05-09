# Time Series
source("source/uncertainty_prec.R")

registerDoParallel(cores = N_CORES)

prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_time_series.rds"))

prec_data[dataset == "ERA5", dataset := "ERA-5"]

PREC_REPS <- c("CMAP", "CPC-Global", "CRU TS v4.08", "EM-Earth", "ERA5-Land",
               "FLDAS", "GPCP CDR v3.2", "GPM IMERG v7", "JRA-55",
               "NCEP/DOE R2", "PREC/L")

ALL_COMBS <- combn(unique(prec_data$dataset), 7, simplify = FALSE)

REP_COMBS <- combn(PREC_REPS, 7, simplify = FALSE)

CMAP_FAM <- grepl("CMAP", ALL_COMBS) & grepl("CPC-Global", ALL_COMBS) &
  grepl("CMORPH CDR", ALL_COMBS) & grepl("GSMaP v8", ALL_COMBS)
CMAP_FAM <- ALL_COMBS[CMAP_FAM]

REAS_FAM <- grepl("ERA-5", ALL_COMBS) & grepl("ERA5-Land", ALL_COMBS) &
  grepl("JRA-55", ALL_COMBS) & grepl("NCEP/NCAR R1", ALL_COMBS)
REAS_FAM <- ALL_COMBS[REAS_FAM]

bootstrap_all <- foreach (idx = 1:length(ALL_COMBS), .combine = rbind) %dopar% {
  dummie_set <- ALL_COMBS[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = mean(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

bootstrap_rep <- foreach (idx = 1:length(REP_COMBS), .combine = rbind) %dopar% {
  dummie_set <- REP_COMBS[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = mean(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

bootstrap_cmap <- foreach(idx = 1:length(CMAP_FAM), .combine = rbind) %dopar% {
  dummie_set <- CMAP_FAM[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = mean(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

bootstrap_reas <- foreach(idx = 1:length(REAS_FAM), .combine = rbind) %dopar% {
  dummie_set <- REAS_FAM[[idx]]
  dummie_prec <- prec_data[dataset %in% dummie_set]
  dummie_prec <- dummie_prec[, .(prec = mean(value, na.rm = TRUE)), .(date)]
  dummie_prec$idx <- idx
  return(dummie_prec)
}

save(bootstrap_all, bootstrap_rep, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                                 "multiplicity_ensemble.rda"))

saveRDS(bootstrap_cmap, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                      "multiplicity_cmap.rds"))

saveRDS(bootstrap_reas, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                      "multiplicity_reas.rds"))
