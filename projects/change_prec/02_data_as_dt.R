# Transform data sets from brick to a single data table
source("source/change_prec.R")

## Data
load(paste0(PATH_SAVE_CHANGE_PREC, "prec_names_1990_2019.rda"))

## Analysis
foreach(data_count = 1:10) %do% {
  dummie <- tabular(PREC_NAMES_1990_2019[data_count])
  dummie_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_NAMES_1990_2019[data_count])
  dummie$dataset <- dummie_name
  saveRDS(dummie,
          paste0(PATH_SAVE_CHANGE_PREC, "xx_temp_", dummie_name, ".rds"))
  rm(dummie)
  gc()
}

temp_filelist <- list.files(path = PATH_SAVE_CHANGE_PREC, pattern = "*xx_temp_",
                                   full.names = TRUE)

prec_data <- lapply(temp_filelist, readRDS)
prec_data <- rbindlist(prec_data)

file.remove(temp_filelist)

## Save data
saveRDS(prec_data, paste0(PATH_SAVE_CHANGE_PREC, "prec_data.rds"))
