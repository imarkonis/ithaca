source('source/twc_change.R')

avail_flux <-  readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_period_mean.Rds'))
avail_flux_change <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_hybrid.rds'))

avail_flux_median <- avail_flux[, .(avail_median = median(avail), flux_median = median(flux)), .(lon, lat)]

dummy <- merge(avail_flux_change, avail_flux_median, by = c("lon", "lat"))
dummy[avail_median < 1, avail_median := 1]
avail_flux_change_ratio <- dummy[, .(avail_change_ratio = avail_change / avail_median, 
                                     flux_change_ratio = flux_change / flux_median), .(lon, lat)]

threshold <- 3

avail_flux_change_ratio[, avail_hotspot := abs(avail_change_ratio) > threshold]
avail_flux_change_ratio[, flux_hotspot := abs(flux_change_ratio) > threshold]

ggplot(avail_flux_change_ratio, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = pmax(pmin(avail_change_ratio, 1), -1))) + # clamp to [-1,1]
  geom_point(data = avail_flux_change_ratio[avail_hotspot == TRUE], aes(x = lon, y = lat)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       name = "Availability Change Ratio", limits = c(-1,1)) +
  theme_minimal() +
  labs(title = "Water Availability Change Ratio Hotspots (Clamped to ±1)")


avail_flux[lon == -156.375 & lat == 20.625, ]
