source('source/twc_change.R')

avail_flux <-  readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_period_mean.Rds'))
avail_flux_change <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_hybrid.rds'))

avail_flux_median <- avail_flux[, .(avail_median = median(avail), flux_median = median(flux)), .(lon, lat)]

dummy <- merge(avail_flux_change, avail_flux_median, by = c("lon", "lat"))
dummy[avail_median < 1, avail_median := 1]
avail_flux_change_ratio <- dummy[, .(avail_change_ratio = avail_change / avail_median, 
                                     flux_change_ratio = flux_change / flux_median), .(lon, lat)]

avail_flux_change_ratio[, ]
