#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))
prec_data <- prec_data[, .(lon, lat, date, value = prec, dataset)]

for (idx in 1:length(PREC_REPS)) {
  dummie <- prec_data[dataset == PREC_REPS[idx]]
  dummie <- fldmean(dummie)
  dummie <- dummie[, .(date, prec = value, dataset = PREC_REPS[idx])]
  
  saveRDS(dummie,
          paste0(PATH_SAVE_CHANGE_PREC_TEMP, "temp_", PREC_REPS[idx],".rds"))
  rm(dummie)
  gc()
}

prec_data <- prec_data[, .(lon, lat, date, value = ensemble)] %>% unique()
prec_data <- fldmean(prec_data)
prec_data <- prec_data[, .(date, prec = value, dataset = "ensemble")]

prec_ts <- rbind(prec_ts, prec_data)

## Save data
saveRDS(prec_ts, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_ts.rds"))
