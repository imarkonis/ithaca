#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r5x.rds"))

## Analysis
prec_data[, `:=`(doy = lubridate::yday(date), year = year(date))]
prec_data <- prec_data[year <= 2024]
prec_data[, n_years := .N, .(lon, lat, dataset)]

setorder(prec_data, lon, lat, dataset, year)

foreach(idx = 1:26) %do% {
  dummie_name <- paste0("beta_", idx)
  prec_data[, `:=`(delta_doy = (doy - data.table::shift(doy, n = idx, type = "lag")),
                   delta_year = (year - data.table::shift(year, n = idx, type = "lag"))),
            .(lon, lat, dataset)]
  prec_data[delta_doy > 365.25/2, k := -365.25
  ][delta_doy < -365.25/2, k := 365.25]
  prec_data[is.na(k), k := 0]
  prec_data[, (dummie_name) := (delta_doy + k)/delta_year, .(lon, lat, dataset)]
  prec_data[, `:=`(k = NULL, delta_doy = NULL, delta_year = NULL)]
  gc()
}

prec_data <- prec_data[n_years > 2]
cols <- paste0("beta_", 1:26)
prec_trends <- prec_data[, .(slope = median(unlist(.SD), na.rm = TRUE)),
                         .(lon, lat, dataset), .SDcols = cols]
prec_trends <- prec_trends[!is.na(slope)]

## Save data
saveRDS(prec_trends, file = paste0(PATH_SAVE_CHANGE_PREC,
                                   "prec_data_r5x_date_slopes.rds"))

###
#ggplot(dummie_data) + geom_raster(aes(x = lon, y = lat, fill = slope))
