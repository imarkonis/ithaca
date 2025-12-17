source('source/twc_change.R')
prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))

prec_evap[, avail := prec - evap]  #Water availability
prec_evap[, flux := (prec + evap) / 2] #Water flux

#Yearly
prec_evap_yearly <- copy(prec_evap)
prec_evap_yearly[, prec := NULL][, evap := NULL]

saveRDS(prec_evap_yearly, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_year.Rds') )

#Period means
prec_evap_periods_mean <- prec_evap_yearly[, .(avail = mean(avail), flux = mean(flux)), .(lon, lat, period, dataset)]

saveRDS(prec_evap_periods_mean, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_periods.Rds'))

#Period change
water_avail <- copy(prec_evap_periods_mean)
water_avail_wide <- dcast(water_avail, lon + lat + dataset ~ period, value.var = 'avail')
water_avail_wide[, avail_change := aft_2001 - bef_2001]

water_flux <- copy(prec_evap_periods_mean)
water_flux_wide <- dcast(water_flux, lon + lat + dataset ~ period, value.var = 'flux')
water_flux_wide[, flux_change := aft_2001 - bef_2001]

avail_flux_change <- merge(water_avail_wide[, .(lon, lat, dataset, avail_change)], 
                           water_flux_wide[, .(lon, lat, dataset, flux_change)], 
                           by = c("lon", "lat", "dataset"), allow.cartesian = TRUE)

saveRDS(avail_flux_change, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_change_grid.rds'))
