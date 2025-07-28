source('source/twc_greece.R')
data_all <- readRDS(paste0(PATH_OUTPUT_DATA, 'data_all.rds'))

#Pre-processing -> different script
data_all[, period := ordered('pre_2001')]
data_all[date > END_PERIOD_1, period := ordered('aft_2001')]
data_all[, month := month(date)]
data_all[, region := factor("region A")]
data_all[lon < 22, region := factor("region B")]

#Analysis
data_period_means <- data_all[, .(value = mean(value)), .(lon, lat, variable, period, dataset)]

dt_wide <- dcast(data_period_means, lon + lat + dataset + period ~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(lon, lat, period, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(lon, lat, period, dataset_evap = dataset, evap)]
merged <- merge(prec_dt, evap_dt, by = c("lon", "lat", "period"), allow.cartesian = TRUE)
merged[, pe_diff := prec - evap]
merged[, pe_mean := (prec + evap) / 2]
merged[, dataset_pair := paste(dataset_prec, dataset_evap, sep = "-")]

water_avail <- merged[, .(water_avail = mean(pe_diff)), .(dataset_pair, period)]
water_avail_wide <- dcast(water_avail, dataset_pair ~ period )
water_avail_wide[, water_avail_change := aft_2001 - pre_2001]

water_flux <- merged[, .(water_flux = mean(pe_mean)), .(dataset_pair, period)]
water_flux_wide <- dcast(water_flux, dataset_pair ~ period )
water_flux_wide[, water_flux_change := aft_2001 - pre_2001]

water_avail_flux  <- merge(water_avail, water_flux, 
      by = c("dataset_pair", "period"), allow.cartesian = TRUE)
twc_change <- merge(water_avail_wide[, .(dataset_pair, water_avail_change)], 
                    water_flux_wide[, .(dataset_pair, water_flux_change)], 
                    by = c("dataset_pair"), allow.cartesian = TRUE)

#Plots -> different script
to_plot <- copy(twc_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[water_flux_change > 0 & water_avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[water_flux_change < 0 & water_avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[water_flux_change > 0 & water_avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[water_flux_change < 0 & water_avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, water_avail_flux)

levels(to_plot$period) <-  c("1981-2000", "2001-2020")
names(to_plot)[5] <- "Period"
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = dataset_pair, col = Conditions), 
            alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/day]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/day]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  geom_text(data = to_plot[Period == "1981-2000"], aes(y = water_flux, x = water_avail, label = dataset_pair), 
            vjust = -3.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))


#SAME AS ABOVE FOR REGIONS
data_period_means <- data_all[, .(value = mean(value)), .(lon, lat, variable, period, region, dataset)]

dt_wide <- dcast(data_period_means, lon + lat + dataset + period + region~ variable, value.var = "value")
prec_dt <- dt_wide[!is.na(prec), .(lon, lat, period, region, dataset_prec = dataset, prec)]
evap_dt <- dt_wide[!is.na(evap), .(lon, lat, period, region, dataset_evap = dataset, evap)]
merged <- merge(prec_dt, evap_dt, by = c("lon", "lat", "period", "region"), allow.cartesian = TRUE)
merged[, pe_diff := prec - evap]
merged[, pe_mean := (prec + evap) / 2]
merged[, dataset_pair := paste(dataset_prec, dataset_evap, sep = "-")]

water_avail <- merged[, .(water_avail = mean(pe_diff)), .(dataset_pair, period, region)]
water_avail_wide <- dcast(water_avail, dataset_pair + region ~ period )
water_avail_wide[, water_avail_change := aft_2001 - pre_2001]

water_flux <- merged[, .(water_flux = mean(pe_mean)), .(dataset_pair, period, region)]
water_flux_wide <- dcast(water_flux, dataset_pair + region ~ period )
water_flux_wide[, water_flux_change := aft_2001 - pre_2001]

water_avail_flux  <- merge(water_avail, water_flux, 
      by = c("dataset_pair", "period", "region"), allow.cartesian = TRUE)
twc_change <- merge(water_avail_wide[, .(dataset_pair, water_avail_change, region)], 
                    water_flux_wide[, .(dataset_pair, water_flux_change, region)], 
                    by = c("dataset_pair", "region"), allow.cartesian = TRUE)

#Plots -> different script
to_plot <- copy(twc_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[water_flux_change > 0 & water_avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[water_flux_change < 0 & water_avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[water_flux_change > 0 & water_avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[water_flux_change < 0 & water_avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, water_avail_flux)

levels(to_plot$period) <-  c("1981-2000", "2001-2020")
names(to_plot)[6] <- "Period"
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = dataset_pair, col = Conditions), 
            alpha = 0.5) +
  facet_wrap(~region) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/day]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/day]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  geom_text(data = to_plot[Period == "1981-2000"], aes(y = water_flux, x = water_avail, label = dataset_pair), 
            vjust = -3.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))


