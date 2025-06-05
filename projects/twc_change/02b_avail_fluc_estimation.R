source('source/twc_change.R')
prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))

#Yearly
dt_wide <- dcast(prec_evap, lon + lat + dataset + year ~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(lon, lat, year, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(lon, lat, year, dataset_evap = dataset, evap)]
avail_flux <- merge(prec_dt, evap_dt, by = c("year", 'lon', 'lat'), allow.cartesian = TRUE)
avail_flux[, avail := prec - evap]  #Water availability
avail_flux[, flux := (prec + evap) / 2] #Water flux
avail_flux[, dataset_pair := factor(paste(dataset_prec, dataset_evap, sep = "-"))]
avail_flux[, dataset_prec := NULL][, dataset_evap := NULL][, prec := NULL][, evap := NULL]

saveRDS(avail_flux, file = paste0(PATH_OUTPUT, 'avail_flux_year.rds') )

#Period means
prec_evap_periods_mean <- prec_evap[, .(value = mean(value)), .(lon, lat, variable, period, dataset)]

dt_wide <- dcast(prec_evap_periods_mean, lon + lat + dataset + period ~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(lon, lat, period, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(lon, lat, period, dataset_evap = dataset, evap)]

avail_flux <- merge(prec_dt, evap_dt, by = c("lon", "lat", "period"), allow.cartesian = TRUE)
avail_flux[, avail := prec - evap]  
avail_flux[, flux := (prec + evap) / 2]
avail_flux[, dataset_pair := factor(paste(dataset_prec, dataset_evap, sep = "-"))]
avail_flux[, dataset_prec := NULL][, dataset_evap := NULL][, prec := NULL][, evap := NULL]

setcolorder(avail_flux, c("lon", "lat", "period", "dataset_pair", "avail", "flux"))

saveRDS(avail_flux, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_period_mean.Rds'))


