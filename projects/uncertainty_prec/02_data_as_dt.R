# Transform data sets from brick to a single data table (large memory requirements)
source("source/uncertainty_prec.R")

## Data
load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_names_2000_2019.rda"))

## Analysis
prec_data <- foreach(data_count = 1:length(PREC_NAMES_2000_2019), .combine = rbind) %do% {
  dummie <- tabular(PREC_NAMES_2000_2019[data_count])
  dummie_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_NAMES_2000_2019[data_count])
  dummie$dataset <- dummie_name
  return(dummie)
}

## Save data
saveRDS(prec_data, paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data.rds"))
