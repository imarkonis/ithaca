source('source/twc_change.R')

avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_periods.Rds'))
prob_ensemble <- readRDS(file.path(PATH_OUTPUT_DATA, 'prob_ensemble.Rds'))

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


avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_periods.Rds'))
dummy <- merge(prob_ensemble, avail_flux, by = c('lon', 'lat', 'dataset'), allow.cartesian = T) 

water_avail <- dummy[, .(water_avail = mean(avail)), .(member, period)]
water_avail_wide <- dcast(water_avail, member ~ period )
water_avail_wide[, avail_change := aft_2001 - pre_2001]

water_flux <- to_plot[, .(water_flux = mean(flux)), .(member, period)]
water_flux_wide <- dcast(water_flux, member ~ period )
water_flux_wide[, flux_change := aft_2001 - pre_2001]

avail_flux_global  <- merge(water_avail, water_flux, 
                            by = c("member", "period"), allow.cartesian = TRUE)
avail_flux_global_change <- merge(water_avail_wide[, .(member, avail_change)], 
                                  water_flux_wide[, .(member, flux_change)], 
                                  by = c("member"), allow.cartesian = TRUE)

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

to_plot <- to_plot[complete.cases(to_plot)]

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = member, col = Conditions), 
            alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = PALETTES$water_cycle_change) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/yr]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/yr]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))




# Step 1: Join sampled datasets back to avail_flux and flux data
grid_flux <- merge(prob_ensemble, avail_flux, by = c('lon', 'lat', 'dataset'), allow.cartesian = TRUE)
grid_flux <- grid_flux[complete.cases(grid_flux)]

# Reshape to compute change per grid cell
avail_wide <- dcast(grid_flux, lon + lat + member ~ period, value.var = "avail")
flux_wide  <- dcast(grid_flux, lon + lat + member ~ period, value.var = "flux")

avail_wide[, avail_change := aft_2001  - pre_2001 ]
flux_wide[, flux_change := aft_2001  - pre_2001 ]

# Merge changes
grid_changes <- merge(avail_wide[, .(lon, lat, member, avail_change)],
                      flux_wide[, .(lon, lat, member, flux_change)],
                      by = c("lon", "lat", "member"))

# Step 2: Assign condition labels per grid cell
grid_changes[, Condition := "Unknown"]
grid_changes[flux_change > 0 & avail_change > 0, Condition := "Wetter - Accelerated"]
grid_changes[flux_change < 0 & avail_change > 0, Condition := "Wetter - Deccelerated"]
grid_changes[flux_change > 0 & avail_change < 0, Condition := "Drier - Accelerated"]
grid_changes[flux_change < 0 & avail_change < 0, Condition := "Drier - Deccelerated"]
grid_changes <- grid_changes[Condition != "Unknown"]

# Count frequency of each condition per grid cell
grid_condition_freq <- grid_changes[, .N, by = .(lon, lat, Condition)]
grid_condition_freq[, freq := N / 100]  # assuming 100 members

ggplot(grid_condition_freq, aes(x = lon, y = lat, color = freq)) +
  geom_point(size = 0.7) +
  scale_color_viridis_c(name = "Probability", option = "C") +
  facet_wrap(~Condition) +
  theme_minimal() +
  labs(title = "Spatial Frequency of Water Cycle Change Regimes",
       x = "Longitude", y = "Latitude")



regime_freq <- grid_changes[, .N, by = .(lon, lat, Condition)]
regime_freq[, freq := N / 100]  # assuming 100 members

dominant_regime <- regime_freq[, .SD[which.max(freq)], by = .(lon, lat)]
setnames(dominant_regime, c("Condition", "freq"), c("Dominant_Condition", "Confidence"))

regime_entropy <- regime_freq[, .(Entropy = -sum(freq * log2(freq + 1e-10))), by = .(lon, lat)]
uncertainty_summary <- merge(dominant_regime, regime_entropy, by = c("lon", "lat"))


ggplot(uncertainty_summary, aes(x = lon, y = lat, color = Confidence)) +
  geom_point(size = 0.7) +
  scale_color_viridis_c(name = "Agreement") +
  theme_minimal() +
  labs(title = "Agreement on Dominant Water Cycle Regime",
       x = "Longitude", y = "Latitude")

ggplot(uncertainty_summary, aes(x = lon, y = lat, color = Entropy)) +
  geom_point(size = 0.7) +
  scale_color_viridis_c(name = "Entropy") +
  theme_minimal() +
  labs(title = "Uncertainty (Entropy) of Water Cycle Change Regimes",
       x = "Longitude", y = "Latitude")



