#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))

prec_ensemble <- unique(prec_data[, .(lon, lat, date, ensemble)])

prec_data <- prec_data[, .(prec = sum(prec, na.rm = TRUE)),
                       .(lon, lat, year(date), dataset)]

prec_ensemble <- prec_ensemble[, .(ensemble = sum(ensemble, na.rm = TRUE)),
                               .(lon, lat, year(date))]

prec_ensemble <- prec_ensemble[, .(lon, lat, year, prec = ensemble, dataset = "ensemble")]

prec_data <- rbind(prec_data, prec_ensemble)

dummie_1 <- prec_data[year <= 2019, .(mean_1990_2019 = mean(prec, na.rm = TRUE)),
                      .(lon, lat, dataset)]

dummie_2 <- prec_data[year >= 1995, .(mean_1995_2024 = mean(prec, na.rm = TRUE)),
                      .(lon, lat, dataset)]

prec_data <- merge(dummie_1, dummie_2, by = c("lon", "lat", "dataset"))

saveRDS(prec_data, file = paste0(PATH_SAVE_CHANGE_PREC,
                                   "prec_data_annual_avg.rds"))
