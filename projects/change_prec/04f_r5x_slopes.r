#Scatter plot matrix
source("source/change_prec.R")

library("Rcpp")

sourceCpp("projects/change_prec/aux_theil_sen_mann_kendall.cpp")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r5x.rds"))

## Analysis
prec_data <- prec_data[year(date) <= 2024,
                       .(lon, lat, dataset, year = year(date), prec)]

dummie <- prec_data[, .(year = 1998:2024), .(lon, lat, dataset)]

prec_data <- prec_data[dummie, on = .(lon, lat, dataset, year)]

rm(dummie)

gc()

setorder(prec_data, lon, lat, dataset, year)

stopifnot(nrow(prec_data) %% 27 == 0)

n_groups <- nrow(prec_data) / 27

n_threads <- parallel::detectCores() - 1

dummie <- sen_mk_parallel(prec = prec_data$prec,
                          n_groups = n_groups,
                          n_threads = n_threads)

idx <- seq(1L, nrow(prec_data), by = 27L)

prec_trends <- data.table(lon = prec_data$lon[idx],
                          lat = prec_data$lat[idx],
                          dataset = prec_data$dataset[idx],
                          slope = dummie[, 1],
                          p_value = dummie[, 2])

## Save data
saveRDS(prec_trends,
        file = paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r5x_slopes.rds"))
