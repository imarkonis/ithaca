source('source/twc_change.R')

#Precipitation & Evaporation (main dataset)
prec_evap <- readRDS(paste0(PATH_OUTPUT_RAW, 'prec_evap_raw.Rds'))

prec_evap[, period := ordered('pre_2001')]
prec_evap[date > END_PERIOD_1, period := ordered('aft_2001')]

prec_evap[, date := as.numeric(format(date, "%Y"))]
setnames(prec_evap, "date", "year")
setcolorder(prec_evap, c("lon", "lat", "year", "period", "dataset", "variable"))

levels(prec_evap$dataset) <- c("CPC", "EARTH", "ERA5L", "GPCC", "MERRA", "FLDAS", "GLEAM", "MERRA", "TERRA")

saveRDS(prec_evap, file = paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
