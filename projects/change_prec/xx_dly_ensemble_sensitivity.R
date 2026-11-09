# Transform data sets from brick to a single data table
source("source/change_prec.R")

## Data
load(paste0(PATH_SAVE_CHANGE_PREC, "prec_names_1995_2024.rda"))

DATASETS <- c("cpc-global", "era5-land", "gpcp-v1-3", "gpm-imerg-v7",
              "jra-3q", "merra-2", "mswep-v2-8", "ncep-doe")
## Analysis
foreach(dataset_count = 1:length(DATASETS)) %do%{
  dummie_data <- DATASETS[dataset_count]
  foreach(year_count = 1995:2024) %do% {
    dummie <- grep(year_count, PREC_NAMES_1995_2024_DLY, value = TRUE)
    dummie <- grep(dummie_data, dummie, value = TRUE, invert = TRUE)
    dummie <- lapply(dummie, function(x) {
      dummie_name <- sub(".*/([^_/]*)_.*", "\\1", x)
      x <- tabular(x)
      x$dataset <- dummie_name
      x$date <- as.Date(x$date)
      x
    })
    dummie <- rbindlist(dummie)
    dummie <- dummie[, .(value = median(value, na.rm = TRUE)),
                     .(lon, lat, date)]
    gc()
    dummie[, s5x := frollsum(value, 5, na.rm = TRUE, align = "left"),
           .(lon, lat)]
    dummie[, r5x := max(s5x, na.rm = TRUE), .(lon, lat)]
    
    dummie_r5x <- dummie[r5x == s5x, .(r5x, date), .(lon, lat)] %>%
      setorder(date)
    dummie_r5x <- dummie_r5x[, .SD[1], .(lon, lat, r5x)]
    
    setorder(dummie, -value)
    dummie_top5 <- dummie[value > 0, .SD[1:5], .(lon, lat)]
    dummie_top5[, doy := lubridate::yday(date)]
    dummie_top5[, d5x := max(doy) - min(doy), .(lon, lat)]
    dummie_top5 <- dummie_top5[complete.cases(dummie_top5), .(d5x, lon, lat)] %>%
      unique()
    
    dummie_5s <- merge(dummie_r5x, dummie_top5, by = c("lon", "lat")) %>%
      setnames("date", "date_r5x")
    
    dummie[value > 0, r1x := max(value, na.rm = TRUE), .(lon, lat)]
    dummie <- dummie[r1x == value, .(lon, lat, date_r1x = date, r1x)]
    
    dummie <- merge(dummie, dummie_5s, by = c("lon", "lat"), all.x = TRUE)
    dummie$dataset <- "ensemble"
    saveRDS(dummie,
            paste0(PATH_SAVE_CHANGE_PREC_TEMP, "temp_", year_count,".rds"))
    rm(dummie)
    gc()
  }
  
  ## Save data
  temp_filelist <- list.files(path = PATH_SAVE_CHANGE_PREC_TEMP,
                              pattern = "temp_*",
                              full.names = TRUE)
  
  dummie <- lapply(temp_filelist, readRDS)
  dummie <- lapply(dummie, function(x) {
    x <- x[, .(lon, lat, date = date_r1x, dataset, prec = r1x)]
    x
  })
  dummie <- rbindlist(dummie)
  
  saveRDS(dummie,
          paste0(PATH_SAVE_CHANGE_PREC, "prec_data_", dummie_data, "_r1x.rds"))
  rm(dummie)
  gc()
  
  dummie <- lapply(temp_filelist, readRDS)
  dummie <- lapply(dummie, function(x) {
    x <- x[complete.cases(x), .(lon, lat, dataset, date = year(date_r5x), d5x)]
    x
  })
  dummie <- rbindlist(dummie)
  
  saveRDS(dummie,
          paste0(PATH_SAVE_CHANGE_PREC, "prec_data_", dummie_data, "_d5x.rds"))
  rm(dummie)
  gc()
  
  dummie <- lapply(temp_filelist, readRDS)
  dummie <- lapply(dummie, function(x) {
    x <- x[complete.cases(x), .(lon, lat, date = date_r5x, dataset, prec = r5x)]
    x
  })
  dummie <- rbindlist(dummie)
  
  saveRDS(dummie,
          paste0(PATH_SAVE_CHANGE_PREC, "prec_data_", dummie_data, "_r5x.rds"))
  rm(dummie)
  gc()
  
  ## Clear temp files
  file.remove(temp_filelist)
}
