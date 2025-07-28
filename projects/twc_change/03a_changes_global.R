source('source/twc_change.R')

avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_periods.Rds'))

water_avail <- avail_flux[, .(water_avail = mean(avail)), .(dataset, period)]
water_avail_wide <- dcast(water_avail, dataset ~ period )
water_avail_wide[, avail_change := aft_2001 - pre_2001]

water_flux <- avail_flux[, .(water_flux = mean(flux)), .(dataset, period)]
water_flux_wide <- dcast(water_flux, dataset ~ period )
water_flux_wide[, flux_change := aft_2001 - pre_2001]

avail_flux_global  <- merge(water_avail, water_flux, 
      by = c("dataset", "period"), allow.cartesian = TRUE)
avail_flux_global_change <- merge(water_avail_wide[, .(dataset, avail_change)], 
                    water_flux_wide[, .(dataset, flux_change)], 
                    by = c("dataset"), allow.cartesian = TRUE)

save(avail_flux_global, avail_flux_global_change, file = paste0(PATH_OUTPUT_DATA, 'avail_flux_global_change.Rdata'))

#Plots
to_plot <- copy(avail_flux_global_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, avail_flux_global)

levels(to_plot$period) <-  c("1981-2000", "2001-2020")
names(to_plot)[5] <- "Period"
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = dataset, col = Conditions), 
            alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = PALETTES$water_cycle_change) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/yr]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/yr]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  geom_text(data = to_plot[Period == "1981-2000"], aes(y = water_flux, x = water_avail, label = dataset), 
            vjust = -1.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

#SAME AS ABOVE STD
water_avail_wide[, aft_2001 := (aft_2001 - pre_2001) / pre_2001]
water_avail_wide[, pre_2001 := 0]
water_avail_wide[, avail_change := aft_2001]
water_avail <- melt(water_avail_wide, id.vars = c('dataset', 'avail_change'), 
                    variable.name = 'period', value.name = 'water_avail')

water_flux_wide[, aft_2001 := (aft_2001 - pre_2001) / pre_2001]
water_flux_wide[, pre_2001 := 0]
water_flux_wide[, flux_change := aft_2001]
water_flux <- melt(water_flux_wide, id.vars = c('dataset', 'flux_change'), 
                   variable.name = 'period', value.name = 'water_flux')

avail_flux_change  <- merge(water_avail, water_flux, 
                     by = c("dataset", "period"), allow.cartesian = TRUE)

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
  geom_line(aes(y = water_flux, x = water_avail, group = dataset, col = Conditions), 
            alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = PALETTES$water_cycle_change) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E))) +
  ylab(expression(atop((P + E) / 2))) +
  geom_text(data = to_plot[Period == "2001-2020"], aes(y = water_flux, x = water_avail, label = dataset), 
            vjust = -1.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

#SAME AS STD BUT FOR CLIMATOLOGIES
masks <- pRecipe::pRecipe_masks()

avail_flux_kg <- merge(masks[KG_class_1 != 'Ocean', .(lon, lat, KG_class_1)], avail_flux, by = c("lon", "lat"))

water_avail_kg <- avail_flux_kg[, .(water_avail = mean(avail)), .(dataset, period, KG_class_1)]
water_avail_kg_wide <- dcast(water_avail_kg, dataset + KG_class_1 ~ period )
water_avail_kg_wide[, avail_change := aft_2001 - pre_2001]

water_flux_kg <- avail_flux_kg[, .(water_flux = mean(flux)), .(dataset, period, KG_class_1)]
water_flux_kg_wide <- dcast(water_flux_kg, dataset + KG_class_1 ~ period )
water_flux_kg_wide[, flux_change := aft_2001 - pre_2001]

avail_flux_kg  <- merge(water_avail_kg, water_flux_kg, 
                            by = c("dataset", "period", "KG_class_1"), allow.cartesian = TRUE)
avail_flux_kg_change <- merge(water_avail_kg_wide[, .(dataset, avail_change, KG_class_1)], 
                                  water_flux_kg_wide[, .(dataset, flux_change, KG_class_1)], 
                                  by = c("dataset", "KG_class_1"), allow.cartesian = TRUE)

water_avail_kg_wide[, aft_2001 := (aft_2001 - pre_2001) / pre_2001]
water_avail_kg_wide[, pre_2001 := 0]
water_avail_kg_wide[, avail_change := aft_2001]
water_avail_kg <- melt(water_avail_kg_wide, id.vars = c('dataset', 'KG_class_1', 'avail_change'), 
                    variable.name = 'period', value.name = 'water_avail')

water_flux_kg_wide[, aft_2001 := (aft_2001 - pre_2001) / pre_2001]
water_flux_kg_wide[, pre_2001 := 0]
water_flux_kg_wide[, flux_change := aft_2001]
water_flux_kg <- melt(water_flux_kg_wide, id.vars = c('dataset', 'KG_class_1', 'flux_change'), 
                   variable.name = 'period', value.name = 'water_flux')

water_flux_kg_change  <- merge(water_avail_kg, water_flux_kg, 
                            by = c("dataset", 'KG_class_1', "period"), allow.cartesian = TRUE)

to_plot <- copy(water_flux_kg_change)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

levels(to_plot$period) <-  c("1981-2000", "2001-2020")
names(to_plot)[2] <- "Climate"
names(to_plot)[3] <- "Period"
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "grey40", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = dataset, col = Conditions), 
            alpha = 0.5) +
  scale_color_manual(values = PALETTES$water_cycle_change) +
  facet_wrap(~Climate, scales = 'free') +
  xlab(expression(atop(P - E))) +
  ylab(expression(atop((P + E) / 2))) +
  geom_text(data = to_plot[Period == "2001-2020"], aes(y = water_flux, x = water_avail, label = dataset), 
            vjust = -1.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))
