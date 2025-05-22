source('source/avail_flux_change.R')
avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_period_mean.Rds'))

water_avail <- avail_flux[, .(water_avail = mean(avail)), .(dataset_pair, period)]
water_avail_wide <- dcast(water_avail, dataset_pair ~ period )
water_avail_wide[, avail_change := aft_2001 - pre_2001]

water_flux <- avail_flux[, .(water_flux = mean(flux)), .(dataset_pair, period)]
water_flux_wide <- dcast(water_flux, dataset_pair ~ period )
water_flux_wide[, flux_change := aft_2001 - pre_2001]

water_avail_flux  <- merge(water_avail, water_flux, 
      by = c("dataset_pair", "period"), allow.cartesian = TRUE)
avail_flux_change <- merge(water_avail_wide[, .(dataset_pair, avail_change)], 
                    water_flux_wide[, .(dataset_pair, flux_change)], 
                    by = c("dataset_pair"), allow.cartesian = TRUE)

save(water_avail_flux, avail_flux_change, file = paste0(PATH_OUTPUT, 'avail_flux_change_global.Rdata'))

#Plots
to_plot <- copy(avail_flux_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

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
  xlab(expression(atop(P - E~"[mm/yr]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/yr]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  geom_text(data = to_plot[Period == "1981-2000"], aes(y = water_flux, x = water_avail, label = dataset_pair), 
            vjust = -1.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))


#SAME AS ABOVE STD
water_avail_wide[, aft_2001 := (aft_2001 - pre_2001) / pre_2001]
water_avail_wide[, pre_2001 := 0]
water_avail_wide[, avail_change := aft_2001]
water_avail <- melt(water_avail_wide, id.vars = c('dataset_pair', 'avail_change'), 
                    variable.name = 'period', value.name = 'water_avail')

water_flux_wide[, aft_2001 := (aft_2001 - pre_2001) / pre_2001]
water_flux_wide[, pre_2001 := 0]
water_flux_wide[, flux_change := aft_2001]
water_flux <- melt(water_flux_wide, id.vars = c('dataset_pair', 'flux_change'), 
                   variable.name = 'period', value.name = 'water_flux')

avail_flux_change  <- merge(water_avail, water_flux, 
                     by = c("dataset_pair", "period"), allow.cartesian = TRUE)

to_plot <- copy(avail_flux_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

levels(to_plot$period) <-  c("1981-2000", "2001-2020")
names(to_plot)[2] <- "Period"
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = dataset_pair, col = Conditions), 
            alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E))) +
  ylab(expression(atop((P + E) / 2))) +
  geom_text(data = to_plot[Period == "2001-2020"], aes(y = water_flux, x = water_avail, label = dataset_pair), 
            vjust = -1.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

