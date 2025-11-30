#Scatter plot matrix
source("source/change_prec.R")

library(openair)

registerDoParallel(cores = N_CORES)

## Data
done_ids <- list.files(PATH_SAVE_CHANGE_PREC_TEMP, full.names = TRUE)
done_ids <- sub(".*/([^_]+)_.*", "\\1", done_ids) %>% as.numeric() %>% unique()
gc()

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_r1x.rds"))

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
  dummie <- prec_data[coord_id == idx & year(date) >= 1995,
                            .(lon, lat, date = as.POSIXct(paste0(as.character(date),
                                                                 " 00:00:00")),
                              prec, dataset)]
  dummie[, n_row := .N, .(lon, lat, dataset)]
  dummie <- dummie[n_row > 5]
  dummie_2 <- dummie[, .(iqr = IQR(prec, na.rm = TRUE),
                         median = median(prec, na.rm = TRUE),
                         mean = mean(prec, na.rm = TRUE),
                         min = min(prec, na.rm = TRUE),
                         max = max(prec, na.rm = TRUE)), .(lon, lat, dataset)]
  dummie <- dummie[, TheilSen(.SD,
                              pollutant = "prec",
                              autocor = TRUE,
                              plot = FALSE,
                              silent = TRUE)$data$main.data[1, c(10, 12,
                                                                 16, 17)],
                   .(lon, lat, dataset)]
  dummie <- merge(dummie, dummie_2, by = c("lon", "lat", "dataset"))
  
  fwrite(dummie, paste0(PATH_SAVE_CHANGE_PREC_TEMP, idx, "_tmp.csv"))
  
  rm(dummie, dummie_2)
  gc()
}

## Save
temp_filelist <- list.files(PATH_SAVE_CHANGE_PREC_TEMP, full.names = TRUE,
                            pattern = "*_tmp.csv")

if (length(temp_filelist) == COORD_MAX) {
  prec_data <- lapply(temp_filelist, fread)
  prec_data <- rbindlist(prec_data)
  
  ## Save data
  saveRDS(prec_data, file = paste0(PATH_SAVE_CHANGE_PREC,
                                   "prec_data_r1x_slopes.rds"))
  
  ## Clear temporary files
  file.remove(temp_filelist)
}
