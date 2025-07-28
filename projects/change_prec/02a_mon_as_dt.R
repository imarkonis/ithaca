# Transform data sets from brick to a single data table
source("source/change_prec.R")

## Data
load(paste0(PATH_SAVE_CHANGE_PREC, "prec_names_1991_2024.rda"))

## Analysis
foreach(data_count = 1:length(PREC_NAMES_1991_2024_MON)) %do% {
  dummie <- tabular(PREC_NAMES_1991_2024_MON[data_count])
  dummie_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_NAMES_1991_2024_MON[data_count])
  dummie$dataset <- dummie_name
  saveRDS(dummie,
          paste0(PATH_SAVE_CHANGE_PREC_TEMP, "temp_", dummie_name, ".rds"))
  rm(dummie)
  gc()
}

temp_filelist <- list.files(path = PATH_SAVE_CHANGE_PREC_TEMP, pattern = "*temp_",
                                   full.names = TRUE)

prec_data_mon <- lapply(temp_filelist, readRDS)
prec_data_mon <- rbindlist(prec_data_mon)

file.remove(temp_filelist)

## Save data
saveRDS(prec_data_mon, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_mon.rds"))
