#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))

prec_ensemble <- unique(prec_data[, .(lon, lat, date, ensemble)])

prec_data <- prec_data[, .(date, prec, mon_mean = mean(prec, na.rm = TRUE)),
                       .(lon, lat, month(date), dataset)]

prec_ensemble <- prec_ensemble[, .(date, prec = ensemble, dataset = "ensemble",
                                   mon_mean = mean(ensemble, na.rm = TRUE)),
                               .(lon, lat, month(date))]

prec_data <- rbind(prec_data, prec_ensemble)

rm(prec_ensemble)
gc()
###
no_cores <- detectCores() - 1

if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)

registerDoParallel(cores = no_cores)
####
prec_data[, coord_id := .GRP, by = c("lon", "lat", "month", "dataset")]

COORD_IDX <- max(prec_data$coord_id)

dummie <- foreach (idx = 1:COORD_IDX, .combine = rbind) %dopar% {
  dummie_row <- prec_data[coord_id == idx]
  dummie_1990_2019 <- dummie_row[year(date) <= 2019]
  dummie_1995_2024 <- dummie_row[year(date) >= 1995]
  dummie_time_1990_2019 <- 1:nrow(dummie_1990_2019)
  dummie_time_1995_2024 <- 1:nrow(dummie_1995_2024)
  X_1990_2019 <- cbind(1, dummie_time_1990_2019)
  X_1995_2024 <- cbind(1, dummie_time_1995_2024)
  invXtX_1990_2019 <- solve(t(X_1990_2019) %*% X_1990_2019) %*% t(X_1990_2019)
  invXtX_1995_2024 <- solve(t(X_1995_2024) %*% X_1995_2024) %*% t(X_1995_2024)
  dummie_slope_1990_2019 <- (invXtX_1990_2019  %*% dummie_1990_2019$prec)[2]
  dummie_slope_1995_2024 <- (invXtX_1995_2024 %*% dummie_1995_2024$prec)[2]
  dummie_row <- unique(dummie_row[, .(lon, lat, month, dataset)])
  dummie_row$slope_1990_2019 <- dummie_slope_1990_2019
  dummie_row$slope_1995_2024 <- dummie_slope_1995_2024
  dummie_row$mean_1990_2019 <- mean(dummie_1990_2019$prec, na.rm = TRUE)
  dummie_row$mean_1995_2024 <- mean(dummie_1995_2024$prec, na.rm = TRUE)
  return(dummie_row)
}

saveRDS(dummie, file = paste0(PATH_SAVE_CHANGE_PREC,
                              "prec_data_monthly_slopes.rds"))
