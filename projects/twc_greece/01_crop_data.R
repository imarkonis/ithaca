source('source/twc_greece.R')
library(pRecipe)

study_area <- c(GREECE_LON_MIN, GREECE_LON_MAX, GREECE_LAT_MIN, GREECE_LAT_MAX)
study_area_name <- 'greece'

# Datasets - Main
dataset_variable <- 'prec'

prec_dt <- foreach(dataset_count = 1:N_DATASETS_PREC, .combine = rbind) %do% {
  dataset_to_crop <- brick(PREC_FNAMES[dataset_count])
  dataset_brick_cropped <- subset_data(dataset_to_crop, 
                                       box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
  dataset_dt <- tabular(dataset_brick_cropped)
  dataset_dt[, variable := dataset_variable]
  dataset_dt[, dataset := PREC_NAMES_SHORT[dataset_count]]
}

dataset_variable <- 'evap'
evap_dt <- foreach(dataset_count = 1:N_DATASETS_EVAP, .combine = rbind) %do% {
  dataset_to_crop <- brick(EVAP_FNAMES[dataset_count])
  dataset_brick_cropped <- subset_data(dataset_to_crop, 
                                       box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
  dataset_dt <- tabular(dataset_brick_cropped)
  dataset_dt[, variable := dataset_variable]
  dataset_dt[, dataset := EVAP_NAMES_SHORT[dataset_count]]
}

data_all <- rbind(prec_dt, evap_dt)
saveRDS(data_all, paste0(PATH_OUTPUT_DATA, 'data_all.rds'))
