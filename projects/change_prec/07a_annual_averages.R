#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))

prec_ensemble <- unique(prec_data[, .(lon, lat, date, ensemble)])

prec_data <- prec_data[, .(prec = sum(prec, na.rm = TRUE)),
                       .(lon, lat, year(date), dataset)]

prec_data <- prec_data[, .(prec = mean(prec, na.rm = TRUE)),
                       .(lon, lat, dataset)]

prec_ensemble <- prec_ensemble[, .(ensemble = sum(ensemble, na.rm = TRUE)),
                               .(lon, lat, year(date))]

prec_ensemble <- prec_ensemble[, .(ensemble = mean(ensemble, na.rm = TRUE)),
                               .(lon, lat)]

prec_ensemble <- prec_ensemble[, .(lon, lat, prec = ensemble, dataset = "ensemble")]

prec_data <- rbind(prec_data, prec_ensemble)

saveRDS(prec_data, file = paste0(PATH_SAVE_CHANGE_PREC,
                                   "prec_data_annual_avg.rds"))
