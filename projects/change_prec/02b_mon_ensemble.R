#Determine number of data sets on each grid cell
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_mon.rds"))

## Analysis
lonlat <- prec_data[, .(prec = mean(value)), .(lon, lat, dataset)]

lonlat <- lonlat[, .(n_datasets = .N), .(lon, lat)]

prec_data <- prec_data[lonlat[n_datasets == MIN_N_DATASETS, .(lon, lat)],
                       on = .(lon, lat)]

setnames(prec_data, "value", "prec")

rm(lonlat)
gc()

dummie <- prec_data[, .(prec = median(prec, na.rm = TRUE),
                        dataset = "ensemble"),
                    .(lon, lat, date)]

prec_data <- rbind(prec_data, dummie)

rm(dummie)
gc()
## Save data
saveRDS(prec_data, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))
