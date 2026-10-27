# Transform data sets from brick to a single data table
source("source/change_prec.R")

## Data
load(paste0(PATH_SAVE_CHANGE_PREC, "prec_names_1998_2025.rda"))

## Analysis
foreach(year_count = 1998:2025) %do% {
  dummie <- grep(year_count, PREC_NAMES_1998_2025_DLY, value = TRUE)
  dummie <- lapply(dummie, function(x) {
    x <- tabular(x)
    x$date <- as.IDate(x$date)
    x
  })
  dummie <- rbindlist(dummie)
  dummie <- dummie[, .(value = median(value, na.rm = TRUE)),
                   .(lon, lat, date)]
  
  gc()
  
  dummie[, value := round(value, 1)]
  dummie[, wet_days := sum(value >= 1), .(lon, lat)]
  dummie[wet_days >= 5, s5x := frollsum(value, 5, align = "left"), .(lon, lat)]
  dummie[wet_days >= 5, r5x := max(s5x, na.rm = TRUE), .(lon, lat)]
  
  dummie_r5x <- dummie[wet_days >= 5 & r5x == s5x, .(date, r5x), .(lon, lat)]
  
  dummie_idx <- dummie_r5x[, .I[which.min(date)], .(lon, lat)]$V1
  
  dummie_r5x <- dummie_r5x[dummie_idx]
  
  setorder(dummie, -value)
  
  dummie_idx <- dummie[wet_days >= 5, head(.I, 5L), by = .(lon, lat)]$V1
  
  dummie_top5 <- dummie[dummie_idx]
  dummie_top5 <- dummie_top5[, {
    doy <- as.integer(format(date, "%j"))
    
    yr <- as.integer(format(date[1L], "%Y"))
    days_in_year <- if (
      yr %% 400L == 0L || (yr %% 4L == 0L && yr %% 100L != 0L)
    ) 366L else 365L
    
    i <- c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L)
    j <- c(2L, 3L, 4L, 5L, 3L, 4L, 5L, 4L, 5L, 5L)
    
    d <- abs(doy[i] - doy[j])
    d <- pmin(d, days_in_year - d)
    
    .(d5x = median(d))
  }, by = .(lon, lat)] %>% unique()
  
  dummie_5s <- merge(dummie_top5, dummie_r5x, by = c("lon", "lat"), all = TRUE) %>%
    setnames("date", "date_r5x")
  
  dummie[wet_days >= 1, r1x := max(value, na.rm = TRUE), .(lon, lat)]
  
  dummie_idx <- dummie[wet_days >= 1, .I[which.max(value)], .(lon, lat)]$V1
  
  dummie <- dummie[dummie_idx, .(lon, lat, date_r1x = date, r1x, wet_days)]
  dummie <- merge(dummie, dummie_5s, by = c("lon", "lat"), all = TRUE)
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
  x <- x[, .(lon, lat, date = date_r1x, prec = r1x, dataset)]
  x <- x[complete.cases(x)]
  x
})
dummie <- rbindlist(dummie)

dummie_base <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))

dummie <- rbind(dummie_base, dummie)

saveRDS(dummie, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))
rm(dummie, dummie_base)
gc()

dummie <- lapply(temp_filelist, readRDS)
dummie <- lapply(dummie, function(x) {
  x <- x[, .(lon, lat, date = year(date_r1x), d5x, dataset)]
  x <- x[complete.cases(x)]
  x
})
dummie <- rbindlist(dummie)

dummie_base <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_d5x.rds"))

dummie <- rbind(dummie_base, dummie)

saveRDS(dummie, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_d5x.rds"))
rm(dummie, dummie_base)
gc()

dummie <- lapply(temp_filelist, readRDS)
dummie <- lapply(dummie, function(x) {
  x <- x[, .(lon, lat, date = date_r5x, prec = r5x, dataset)]
  x <- x[complete.cases(x)]
  x
})
dummie <- rbindlist(dummie)

dummie_base <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r5x.rds"))

dummie <- rbind(dummie_base, dummie)

saveRDS(dummie, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r5x.rds"))
rm(dummie, dummie_base)
gc()

dummie <- lapply(temp_filelist, readRDS)
dummie <- lapply(dummie, function(x) {
  x <- x[, .(lon, lat, date = year(date_r1x), wet_days, dataset)]
  x <- x[complete.cases(x)]
  x
})
dummie <- rbindlist(dummie)

dummie_base <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_wet_days.rds"))

dummie <- rbind(dummie_base, dummie)

saveRDS(dummie, paste0(PATH_SAVE_CHANGE_PREC, "prec_data_wet_days.rds"))
rm(dummie, dummie_base)
gc()

## Clear temp files
file.remove(temp_filelist)
