source('source/twc_change.R')
masks <- pRecipe::pRecipe_masks()
load(paste0(PATH_OUTPUT, 'avail_flux_change_global.Rdata'))
soil_moisture <- readRDS(paste0(PATH_OUTPUT_RAW, '/other/esa-cci_yearly.Rds'))

water_avail_flux <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_grid.rds'))
avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))

soil_moisture[, period := ordered('pre_2001')]
soil_moisture[date > END_PERIOD_1, period := ordered('aft_2001')]
soil_moisture <- merge(soil_moisture, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)]) 
setnames(soil_moisture, 'ipcc_short_region', 'region')

#Global comparison
soil_moisture[, mean(value), period] #0.2% increase between two periods

#IPCC comparison
sm_grid_mean <- soil_moisture[, .(value = mean(value)), .(lon, lat, region, period)]
sm_grid_mean_wide <- dcast(sm_grid_mean, lon + lat +region ~ period, value.var = "value")
sm_grid_mean_wide[, sm_diff := aft_2001 - pre_2001]
avail_flux_change <- merge(avail_flux_change, sm_grid_mean_wide[, .(lon, lat, region, sm_diff)], by = c('lon', 'lat'), allow.cartesian=TRUE)

avail_flux_change[, agreement := factor("Uknown")]
avail_flux_change[avail_change > 0 & sm_diff > 0,  agreement := factor("yes")]
avail_flux_change[avail_change < 0 & sm_diff < 0,  agreement := factor("yes")]
avail_flux_change[avail_change > 0 & sm_diff < 0,  agreement := factor("no")]
avail_flux_change[avail_change < 0 & sm_diff > 0,  agreement := factor("no")]

to_plot <- copy(avail_flux_change)
to_plot[, n_grids := .N, .(region, dataset_pair)]
to_plot <- to_plot[agreement != "Uknown" & n_grids >= 1000]

ggplot(to_plot) +
  geom_bar(aes(region, fill = agreement), position = "fill") +
  facet_wrap(~dataset_pair) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(to_plot) +
  geom_bar(aes(dataset_pair, fill = agreement), position = "fill") +
  facet_wrap(~region) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

avail_flux_change_agreement <- avail_flux_change[agreement != "Uknown", .N, by = .(agreement, dataset_pair, region)][
  , agree_ratio := N / sum(N), by = .(dataset_pair, region)]

highest_agreement <- avail_flux_change_agreement[agreement == 'yes', .SD[which.max(agree_ratio)], by = region][, agreement := NULL]
highest_agreement[N > 100]

save(avail_flux_change_agreement, highest_agreement, file = paste0(PATH_OUTPUT, 'avail_flux_change_agreement_ipcc.Rdata'))

water_avail_flux <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_ipcc.rds'))
avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_ipcc.rds'))

#Highest agreement dataset pair per region

to_plot <- merge(highest_agreement[, .(dataset_pair, region)], avail_flux_change, by = c("dataset_pair", 'region'))
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, water_avail_flux)

levels(to_plot$period) <-  c("1981-2000", "2001-2020")
names(to_plot)[6] <- "Period"
axis_decimal <- function(x) sprintf("%.1f", x)

ggplot(to_plot) +
  geom_point(aes(y = water_flux, x = water_avail, fill = Period, shape = Period), 
             colour = "transparent", size = 2) +
  geom_line(aes(y = water_flux, x = water_avail, group = region, col = Conditions), 
            alpha = 0.5) +
  scale_fill_manual(values = c('grey60', 'grey20')) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  scale_shape_manual(values = c(22, 21)) +
  xlab(expression(atop(P - E~"[mm/year]"))) +
  ylab(expression(atop((P + E) / 2~" [mm/year]"))) +
  scale_y_continuous(labels = axis_decimal) + 
  geom_text(data = to_plot[Period == "1981-2000"], aes(y = water_flux, x = water_avail, label = region), 
            vjust = -3.5, size = 3, check_overlap = TRUE) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))

avail_flux_change <- readRDS(file = paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))
avail_flux_change <- merge(avail_flux_change, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)], by = c("lon", "lat")) 
colnames(avail_flux_change)[6] <- 'region'


#Map of highest agreement dataset pair per region

to_plot <- merge(highest_agreement[, .(dataset_pair, region)], avail_flux_change, by = c("dataset_pair", 'region'))
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

ggplot(to_plot) +
  geom_point(aes(x = lon, y = lat, col = Conditions)) +
  scale_color_manual(values = WATER_CYCLE_CHANGE_PALETTE) +
  theme_minimal()

