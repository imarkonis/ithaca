# Transform data sets from brick to a single data table
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))

prec_data <- prec_data[, .(prec = sum(prec)), .(lon, lat, dataset, year(date))]

saveRDS(prec_data, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs.rds"))
