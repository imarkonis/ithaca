source('source/twc_change.R')
prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))

prec_evap[, avail := prec - evap]  #Water availability
prec_evap[, flux := (prec + evap) / 2] #Water flux

#Yearly
prec_evap_yearly <- copy(prec_evap)
prec_evap_yearly[, prec := NULL][, evap := NULL]

saveRDS(prec_evap_yearly, file = paste0(PATH_OUTPUT, 'avail_flux_year.Rds') )

#Period means
prec_evap_periods_mean <- prec_evap_yearly[, .(avail = mean(avail), flux = mean(flux)), .(lon, lat, period, dataset)]

saveRDS(prec_evap_periods_mean, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_periods.Rds'))


