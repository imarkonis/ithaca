source('source/exeves.R')
library(pRecipe)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

names(prec)[3] <- "prec"
exeves_prec <- merge(exeves, prec, by = c('grid_id', 'date'), all.x = TRUE)
names(exeves_prec)[5] <- 'evap'

exeves_prec_sums <- unique(exeves_prec[, .(evap = sum(evap), 
                                           prec = sum(prec), 
                                           diff_pe = sum(prec) - sum(evap)), 
                                       by = .(grid_id, period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[7] <- "Period"
to_plot[grid_id == 100 & Period == "Up to 2001", Conditions := factor('Wetter - Deccelerated')] # needed for plotting purposes

gg_all <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux / 20, x = diff_pe / 20, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux / 20, x = diff_pe / 20, group = grid_id, col = Conditions), alpha = 0.5) +
  #geom_rect(aes(xmin = 250, xmax = 280, ymin = 710, ymax = 790), fill = NA, col = '#a4c8a4', linewidth = 0.25) +
  #geom_rect(aes(xmin = 180, xmax = 215, ymin = 640, ymax = 670), fill = NA, col = 'orange', linewidth = 0.25) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = c( 'darkgreen', 'darkred', 'darkorange', 'steelblue3')) +
  scale_shape_manual(values = c(22, 21)) +
  xlab("Atmospheric water residual over land (mm)") +
  ylab("Mean land-atmosphere water exchange (mm)") +
  theme_linedraw()

exeves_prec_sums <- unique(exeves_prec[is.na(event_id), .(evap = sum(evap), 
                                           prec = sum(prec), 
                                           diff_pe = sum(prec) - sum(evap)), 
                                       by = .(grid_id, period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[7] <- "Period"
exeves_prec_sums[]

gg_not_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux / 20, x = diff_pe / 20, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux / 20, x = diff_pe / 20, group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = c('steelblue3' ,  'darkorange', 'darkred','darkgreen')) +
  scale_shape_manual(values = c(22, 21)) +
  xlab("Atmospheric water residual (mm)") +
  ylab("Mean land-atmosphere exchange (mm)") +
  theme_linedraw()

exeves_prec_sums <- unique(exeves_prec[!is.na(event_id), .(evap = sum(evap), 
                                           prec = sum(prec), 
                                           diff_pe = sum(prec) - sum(evap)), 
                                       by = .(grid_id, period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_pe, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
exeves_prec_sums[, mean_flux := (prec + evap) / 2]

to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[7] <- "Period"
exeves_prec_sums[]

gg_event <- ggplot(to_plot) +
  geom_point(aes(y = mean_flux / 20, x = diff_pe / 20, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = mean_flux / 20, x = diff_pe / 20 , group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = c( 'darkgreen', 'darkred', 'darkorange', 'steelblue3')) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P-E))) +
  ylab(expression(atop((P+E)/2))) +
  theme_linedraw()

ggarrange(gg_all, gg_not_event, gg_event,
          ncol = 1, nrow = 3,
          labels = c("A", "B", "C"),
          legend = 'right', common.legend = TRUE) 
          






to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[6] <- "Period"
exeves_prec_sums[]

ggplot(to_plot) +
  geom_point(aes(y = prec / 20, x = evap / 20, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = prec / 20, x = evap / 20, group = grid_id, col = Conditions), alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = c( 'darkgreen', 'darkred', 'darkorange', 'steelblue3')) +
  scale_shape_manual(values = c(22, 21)) +
  xlab("Evaporatation") +
  ylab("Precipitation") +
  scale_x_continuous(expand = c(0, 0), limits = c(400, 750), breaks = seq(0, 1400, 100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(500, 1300), breaks = seq(0, 1700, 100)) +
  theme_linedraw()

exeves_prec_sums <- unique(exeves_prec[!is.na(event_id), .(evap = sum(evap), prec = sum(prec)), by = .(grid_id, period)])
exeves_prec_sums <- exeves_prec_sums[, .(prec, evap, diff_prec = diff(prec), diff_evap = diff(evap), period), .(grid_id)]
exeves_prec_sums[, diff_diff_pe := diff_prec - diff_evap]
exeves_prec_sums[, sum_diff_pe := diff_prec + diff_evap]
 
to_plot <- copy(exeves_prec_sums)
to_plot[, Conditions := factor("Uknown")]
to_plot[sum_diff_pe > 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[sum_diff_pe > 0 & diff_diff_pe < 0, Conditions := factor('Drier - Accelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[sum_diff_pe < 0 & diff_diff_pe < 0, Conditions := factor('Drier - Deccelerated')]
levels(to_plot$period) <-  c("Up to 2001", "After 2001")
names(to_plot)[6] <- "Period"
exeves_prec_sums[]

ggplot(to_plot) +
  geom_point(aes(y = prec / 20, x = evap / 20, fill = Period, shape = Period), colour = "transparent", size = 2) +
  geom_line(aes(y = prec / 20, x = evap / 20, group = grid_id, col = Conditions), alpha = 0.5) +
  geom_abline(slope = 1, linetype = 3, size = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = c( 'darkgreen', 'darkred', 'darkorange', 'steelblue3')) +
  scale_shape_manual(values = c(22, 21)) +
  xlab("Evaporatation") +
  ylab("Precipitation") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 400), breaks = seq(0, 400, 100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0, 700, 100)) +
  theme_linedraw()

# Exeves & precipitation
total_prec_period <- exeves_prec[, .(value = sum(prec)), .(grid_id,  period)]
total_prec_period[, diff_value := diff(value), by = .(grid_id)]
total_prec_period[, diff_value_ratio := diff_value/value, by = .(grid_id, period)]

total_prec_period_monthly <- exeves_prec[, .(value = sum(prec)), .(grid_id, month(date), year(date), period)]
ggplot(total_prec_period_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal() +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

exeves_prec[event_qr_id > 0, sum(prec), period]
exeves_prec[event_id > 0, sum(prec), period]
exeves_prec[event_qr_id > 0, mean(prec), period]
exeves_prec[event_id > 0, mean(prec), period]

exeves_prec[extreme_prec == TRUE, sum(prec), period]
exeves_prec[extreme_prec == TRUE & event_qr_id > 0, mean(prec), period]
exeves_prec[extreme_prec == TRUE & event_id > 0, mean(prec), period]

exeves_prec_monthly <- exeves_prec[extreme_prec == TRUE & event_id > 0, .(value = sum(prec)), .(grid_id, month(date), period)]
ggplot(exeves_prec_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal() 

#Aridity

exeves_aridity <- exeves_prec[, .(value = sum(prec - value)), .(lon, lat, period)]

ggplot(exeves_aridity[value > -10000 & value < 10000]) +
  geom_tile(aes(lon, lat, fill = value)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0.3) +
  facet_wrap(~period) +
  theme_minimal()

exeves_prec_fraction <- total_exeves_prec_period[, .(lon, lat, event_prec = value, period)]
exeves_prec_fraction <- exeves_prec_fraction[total_prec_period[, .(lon, lat, total_prec = value, period)], 
                                             on = .(lon, lat, period)]
exeves_prec_fraction[, value := event_prec/total_prec]


ggplot(exeves_prec_fraction) +
  geom_tile(aes(lon, lat, fill = value)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0.2) +
  facet_wrap(~period) +
  theme_minimal() 
  
ggplot(exeves_prec_fraction[lon > 20 & lat < 60]) +
  geom_density(aes(x = value, col = period)) +
  theme_minimal() 
  
ggplot(exeves_prec_fraction[lon < 20 & lat < 60]) +
  geom_density(aes(x = value, col = period)) +
  theme_minimal() 

rm(exeves_prec); gc()


lon_test <- 12.125 
lat_test <- 50.875 

test <- evap[lon == lon_test & lat == lat_test]
test_prec <- prec[lon == lon_test & lat == lat_test]
names(test_prec)[4] <- "prec"

test <- test[test_prec, on = .(lon, lat, date, period)]
test[, evap_cum := cumsum(value)]
test[, prec_cum := cumsum(prec)]
test[, evap_cum_yr := cumsum(value), year(date)]
test[, prec_cum_yr := cumsum(prec), year(date)]

ggplot(test) +
  geom_line(aes(date, pentad_mean)) +
  theme_minimal()

ggplot() +
  geom_line(data = test, aes(date, prec_cum - evap_cum)) +
  theme_minimal()

ggplot(data = test[year(date) > 2013]) +
  geom_hline(yintercept = 0) +
  geom_point(data = test[year(date) > 2013 & !is.na(event_id)], 
             aes(date, value), col = 'purple', size = 3) +
  geom_line(aes(date, value), col = "red") +
  geom_line(aes(date, (prec/10)), col = 'blue') +
  facet_wrap(~year(date), scales = "free_x") +
  geom_point(data = test[year(date) > 2013& !is.na(extreme_id)], 
             aes(date, value), col = 'red') +
  theme_minimal()

ggplot(data = test[year(date) == 2000 & month(date) < 9]) +
  geom_hline(yintercept = 0) +
  geom_point(data = test[year(date)  == 2000 & extreme_prec == TRUE & month(date) < 9], 
             aes(date, 0), col = 'black', size = 5, shape = 15) +
  geom_point(data = test[year(date)  == 2000 & !is.na(event_id) & month(date) < 9], 
             aes(date, 0), col = 'purple', size = 3) +
  geom_point(data = test[year(date)  == 2000 & !is.na(event_id) & month(date) < 9], 
             aes(date, 0), col = 'purple', size = 3) +
  geom_line(aes(date, value), col = "red") +
  geom_line(aes(date, (prec/10)), col = 'blue') +
  geom_point(data = test[year(date)  == 2000 & !is.na(extreme_id) & month(date) < 9], 
             aes(date, 0), col = 'red') +
  theme_minimal()
