source('source/twc_change.R')

#Precipitation & Evaporation (main dataset)
prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))

prec_evap[, period := ordered('pre_2001')]
prec_evap[date > END_PERIOD_1, period := ordered('aft_2001')]

prec_evap[, date := as.numeric(format(date, "%Y"))]
setnames(prec_evap, "date", "year")
setcolorder(prec_evap, c("lon", "lat", "year", "period", "dataset", "evap", "prec"))

saveRDS(prec_evap, file = paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
