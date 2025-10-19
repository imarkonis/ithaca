#Scatter plot matrix
source("source/change_prec.R")

registerDoParallel(cores = N_CORES)

## Data
done_ids <- list.files(PATH_SAVE_CHANGE_PREC_TEMP, full.names = TRUE)
done_ids <- sub(".*/([^_]+)_.*", "\\1", done_ids) %>% as.numeric() %>% unique()
gc()

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_prec_data_r1x.rds"))
gc()

## Analysis
prec_data[, coord_id := .GRP, by = c("lon", "lat")]

COORD_MAX <- max(prec_data$coord_id)

dummie_coords <- 1:COORD_MAX
dummie_coords <- setdiff(dummie_coords, done_ids)
gc()

prec_data <- prec_data[coord_id %in% dummie_coords]
gc()

DATASETS <- c("cpc-global", "ensemble", "era5-land", "gpcp-v1-3",
              "gpm-imerg-v7", "jra-3q", "merra-2", "mswep-v2-8", "ncep-doe")

foreach (coord_idx = 1:length(dummie_coords)) %dopar% {
  idx <- dummie_coords[coord_idx]
  dummie_point <- prec_data[coord_id == idx]
  dummie <- foreach(data_idx = 1:9, .combine = rbind) %do% {
    dummie_dataset <- DATASETS[data_idx]
    dummie_row <- dummie_point[dataset == dummie_dataset]
    if (nrow(dummie_row) > 1) {
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
      dummie_row <- unique(dummie_row[, .(lon, lat, dataset)])
      dummie_row$slope_1990_2019 <- dummie_slope_1990_2019
      dummie_row$slope_1995_2024 <- dummie_slope_1995_2024
      dummie_row$max_1990_2019 <- max(dummie_1990_2019$prec, na.rm = TRUE)
      dummie_row$max_1995_2024 <- max(dummie_1995_2024$prec, na.rm = TRUE)
      dummie_row$min_1990_2019 <- min(dummie_1990_2019$prec, na.rm = TRUE)
      dummie_row$min_1995_2024 <- min(dummie_1995_2024$prec, na.rm = TRUE)
      dummie_row$iqr_1990_2019 <- IQR(dummie_1990_2019$prec, na.rm = TRUE)
      dummie_row$iqr_1995_2024 <- IQR(dummie_1995_2024$prec, na.rm = TRUE)
      dummie_row$median_1990_2019 <- median(dummie_1990_2019$prec, na.rm = TRUE)
      dummie_row$median_1995_2024 <- median(dummie_1995_2024$prec, na.rm = TRUE)
      dummie_row$mean_1990_2019 <- mean(dummie_1990_2019$prec, na.rm = TRUE)
      dummie_row$mean_1995_2024 <- mean(dummie_1995_2024$prec, na.rm = TRUE)
      return(dummie_row)
    }
  }
  fwrite(dummie, paste0(PATH_SAVE_CHANGE_PREC_TEMP, idx, "_tmp.csv"))
  rm(dummie, dummie_point)
  gc()
}

## Save
temp_filelist <- list.files(PATH_SAVE_CHANGE_PREC_TEMP, full.names = TRUE,
                            pattern = "*_tmp.csv")

prec_data <- lapply(temp_filelist, fread)
prec_data <- rbindlist(prec_data)

## Save data
saveRDS(prec_data, file = paste0(PATH_SAVE_CHANGE_PREC,
                                 "prec_data_r1x_slopes.rds"))

## Clear temporary files
file.remove(temp_filelist)
