#Determine number of data sets on each grid cell
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data.rds"))

## Analysis
lonlat <- prec_data[, .(n_datasets = .N), .(lon, lat)]

lonlat <- lonlat[, .(n_datasets = .N), .(lon, lat)]

prec_data <- prec_data[lonlat[n_datasets == 3600, .(lon, lat)], on = .(lon, lat)]

setnames(prec_data, "value", "prec")

rm(lonlat)
gc()

prec_data[, `:=`(ensemble = median(prec, na.rm = TRUE),
                 mad = mad(prec, na.rm = TRUE)), .(lon, lat, date)]

## Save data
saveRDS(prec_data, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))
