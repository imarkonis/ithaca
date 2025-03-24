#Scatter plot matrix
source("source/change_prec.R")

library(openair)

registerDoParallel(cores = 42)
## Data
done_ids <- list.files(PATH_SAVE_CHANGE_PREC_TABLES, full.names = TRUE)
done_ids <- sub(".*/([^_]+)_.*", "\\1", done_ids) %>% as.numeric()

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_roi.rds"))
prec_data[, coord_idx := .GRP, .(lon, lat)]
COORD_MAX <- max(prec_data$coord_idx)
foreach(idx = 1:COORD_MAX) %dopar% {
  if (!(idx %in% done_ids)) {
    dummie <- prec_data[coord_idx == idx, .(lon, lat,
                                            date = as.POSIXct(paste0(as.character(date),
                                                                     " 00:00:00")),
                                            prec, dataset)]
    dummie <- dummie[, TheilSen(.SD,
                                pollutant = "prec",
                                autocor = TRUE,
                                plot = FALSE,
                                silent = TRUE)$data$main.data[1, c(10, 12,
                                                                   16, 17)],
                     .(lon, lat, dataset)]
    dummie_2 <- prec_data[coord_idx == idx, .(lon, lat,
                                              date = as.POSIXct(paste0(as.character(date),
                                                                       " 00:00:00")),
                                              ensemble)] %>% unique()
    dummie_2 <- dummie_2[, TheilSen(.SD,
                                    pollutant = "ensemble",
                                    autocor = TRUE,
                                    plot = FALSE,
                                    silent = TRUE)$data$main.data[1, c(10, 12,
                                                                       16, 17)],
                         .(lon, lat)]
    dummie <- rbind(dummie, dummie_2[, dataset := "ensemble"])
    fwrite(dummie, paste0(PATH_SAVE_CHANGE_PREC_TABLES, idx, "_tmp.csv"))
    rm(dummie, dummie_2)
    gc()
  }
}
