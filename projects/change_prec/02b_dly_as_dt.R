# Transform data sets from brick to a single data table
source("source/change_prec.R")

## Data
load(paste0(PATH_SAVE_CHANGE_PREC, "prec_names_1991_2024.rda"))

## Analysis
foreach(data_count = 1:length(PREC_NAMES_1991_2024_DLY)) %do% {
  dummie <- tabular(PREC_NAMES_1991_2024_DLY[data_count])
  dummie[, `:=`(s3x = frollsum(value, 3, na.rm = TRUE, align = "left"),
                s5x = frollsum(value, 5, na.rm = TRUE, align = "left")),
         .(lon, lat)]
  dummie[, `:=`(r1x = max(value, na.rm = TRUE),
                r3x = max(s3x, na.rm = TRUE),
                r5x = max(s5x, na.rm = TRUE)),
         .(lon, lat, year(date))]
  dummie <- dummie[r1x == value | r3x == s3x | r5x == s5x]
  dummie_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_NAMES_1991_2024_DLY[data_count])
  dummie$dataset <- dummie_name
  saveRDS(dummie,
          paste0(PATH_SAVE_CHANGE_PREC_TEMP, "temp_", data_count,".rds"))
  rm(dummie)
  gc()
}

## Save data
temp_filelist <- list.files(path = PATH_SAVE_CHANGE_PREC_TEMP,
                            pattern = "temp_*",
                            full.names = TRUE)

dummie <- lapply(temp_filelist, readRDS)
dummie <- lapply(dummie, function(x) {
  x <- x[r1x == value, .(lon, lat, date = as.Date(date), prec = r1x)]
  x
  })
dummie <- rbindlist(dummie)

saveRDS(dummie,
        paste0(PATH_SAVE_CHANGE_PREC, "prec_data_prec_data_r1x.rds"))
rm(dummie)
gc()

dummie <- lapply(temp_filelist, readRDS)
dummie <- lapply(dummie, function(x) {
  x <- x[r3x == s3x, .(lon, lat, date = as.Date(date), prec = r3x)]
  x
})
dummie <- rbindlist(dummie)

saveRDS(dummie,
        paste0(PATH_SAVE_CHANGE_PREC, "prec_data_prec_data_r3x.rds"))
rm(dummie)
gc()

dummie <- lapply(temp_filelist, readRDS)
dummie <- lapply(dummie, function(x) {
  x <- x[r5x == s5x, .(lon, lat, date = as.Date(date), prec = r5x)]
  x
})
dummie <- rbindlist(dummie)

saveRDS(dummie,
        paste0(PATH_SAVE_CHANGE_PREC, "prec_data_prec_data_r5x.rds"))
rm(dummie)
gc()

## Clear temp files
file.remove(temp_filelist)
