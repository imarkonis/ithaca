#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))

prec_ts <- foreach(idx = 1:length(PREC_REPS), .combine = rbind) %do% {
  dummie_name <- PREC_REPS[idx]
  dummie <- prec_data[dataset == dummie_name, .(lon, lat, date, value = prec)]
  dummie <- fldmean(dummie)
  dummie <- dummie[, .(date, prec = value, dataset = dummie_name)]
  return(dummie)
  gc()
}

prec_data <- prec_data[, .(lon, lat, date, value = ensemble)] %>% unique()
prec_data <- fldmean(prec_data)
prec_data <- prec_data[, .(date, prec = value, dataset = "ensemble")]

prec_ts <- rbind(prec_ts, prec_data)

## Save data
saveRDS(prec_ts, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_ts.rds"))
