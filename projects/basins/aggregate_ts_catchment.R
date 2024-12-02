library(doParallel)
library(foreach)
library(data.table)
library(parallel)
library(raster)

aggregate_ts_catchment <- function(data, method = "mean") {
  data[, pair_id := .GRP, by = c("dataset", "pfaf_id")]
  PAIR_IDX <- max(data$pair_id)
  ts_data <- foreach(data_idx = 1:PAIR_IDX, .combine = rbind) %do% {
    dummie <- data[pair_id == data_idx]
    dummie_name <- unique(dummie$dataset)
    dummie_pfaf <- unique(dummie$pfaf_id)
    if (method == "mean") {
      dummie_dates <- unique(dummie$date)
      no_cores <- detectCores() - 1
      if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
      registerDoParallel(cores = no_cores)
      dummie <- foreach (idx = 1:length(dummie_dates), .combine = rbind) %dopar% {
        dummie_step <- dummie[date == dummie_dates[idx]]
        dummie_date <- unique(dummie_step$date)
        dummie_step <- dummie_step[, .(lon, lat, value)]
        dummie_step <- rasterFromXYZ(dummie_step)
        dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
        dummie_step <- dummie_area * dummie_step
        dummie_step <- cellStats(dummie_step, stat = "sum", na.rm = TRUE)
        dummie_step <- data.table("date" = dummie_date,
                                  "value" = dummie_step)
        dummie_step
      }
      dummie$dataset <- dummie_name
      dummie$pfaf_id <- dummie_pfaf
      dummie <- dummie[, .(date, dataset, pfaf_id, value)]
    } else if (method == "median") {
      dummie_dates <- unique(dummie$date)
      no_cores <- detectCores() - 1
      if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
      registerDoParallel(cores = no_cores)
      dummie <- foreach (idx = 1:length(dummie_dates), .combine = rbind) %dopar% {
        dummie_table <- dummie[date == dummie_dates[idx]]
        dummie_date <- unique(dummie_table$date)
        dummie_step <- dummie_table[, .(lon, lat, value)]
        dummie_step <- rasterFromXYZ(dummie_step)
        dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
        dummie_area <- as.data.frame(dummie_area, na.rm = TRUE, xy = TRUE)
        setnames(dummie_area, c("lon", "lat", "weights"))
        dummie_table <- merge(dummie_table, dummie_area)
        dummie_table <- dummie_table[order(value)][, quant := cumsum(weights)
                                                   ][, control := abs(0.5 - quant)]
        dummie_table <- dummie_table[control == min(control)]
        dummie_table <- dummie_table[, .(value = mean(value, na.rm = TRUE)), .(date, dataset, pfaf_id)]
        dummie_table
      }
    } else if (method == "sum") {
      dummie_dates <- unique(dummie$date)
      no_cores <- detectCores() - 1
      if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
      registerDoParallel(cores = no_cores)
      dummie <- foreach (idx = 1:length(dummie_dates), .combine = rbind) %dopar% {
        dummie_table <- dummie[date == dummie_dates[idx]]
        dummie_date <- unique(dummie_table$date)
        dummie_step <- dummie_table[, .(lon, lat, value)]
        dummie_step <- rasterFromXYZ(dummie_step)
        dummie_area <- area(dummie_step, na.rm = TRUE)
        dummie_area <- as.data.frame(dummie_area, na.rm = TRUE, xy = TRUE)
        setnames(dummie_area, c("lon", "lat", "area"))
        dummie_table <- merge(dummie_table, dummie_area)
        dummie_table[, volume := (value/1000000)*area]
        dummie_table <- dummie_table[, .(value = sum(volume, na.rm = TRUE)), .(date, dataset, pfaf_id)]
        dummie_table
      }
    }
    return(dummie)
  }
}