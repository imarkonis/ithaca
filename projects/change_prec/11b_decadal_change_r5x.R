#Scatter plot matrix
source("source/change_prec.R")

registerDoParallel(cores = N_CORES)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                            "prec_data_r5x.rds"))
prec_data <- prec_data[year(date) >= 1995]
prec_data[, `:=`(doy = lubridate::yday(date), year = year(date))]
prec_data[, `:=`(delta_doy = (doy - data.table::shift(doy, type = "lag")),
                 delta_year = (year - data.table::shift(year, type = "lag")),
                 dummie_idx = seq_len(.N),
                 idx_years = .N,
                 n_years = max(year(date)) - min(year(date))),
          .(lon, lat, dataset)]
prec_data[delta_doy > 365.25/2, k := -365.25
][delta_doy < -365.25/2, k := 365.25]
prec_data[is.na(k) & dummie_idx > 1, k := 0]
prec_data[, beta_1 := (delta_doy + k)/delta_year, .(lon, lat, dataset)]

foreach(idx = 2:29) %do% {
  dummie_name <- paste0("beta_", idx)
  prec_data[, `:=`(delta_doy = (doy - data.table::shift(doy, n = idx, type = "lag")),
                   delta_year = (year - data.table::shift(year, n = idx, type = "lag"))),
            .(lon, lat, dataset)]
  prec_data[delta_doy > 365.25/2, k := -365.25
  ][delta_doy < -365.25/2, k := 365.25]
  prec_data[is.na(k) & dummie_idx > 1, k := 0]
  prec_data[, (dummie_name) := (delta_doy + k)/delta_year, .(lon, lat, dataset)]
  gc()
}

prec_data <- prec_data[n_years >= 9 & idx_years >= 4]
cols <- paste0("beta_", 1:29)
prec_trends <- prec_data[, .(slope = median(unlist(.SD), na.rm = TRUE)),
                         .(lon, lat, dataset), .SDcols = cols]

## Save data
saveRDS(prec_trends, file = paste0(PATH_SAVE_CHANGE_PREC,
                                   "prec_data_r5x_date_slopes.rds"))

###
#ggplot(dummie_data) + geom_raster(aes(x = lon, y = lat, fill = slope))
