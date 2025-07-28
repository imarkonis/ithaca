source('source/twc_change.R')
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))

dataset_ranks[, all_ranks := 
                prec_mean_rank + prec_sd_rank + prec_rank_slope +
                evap_mean_rank + evap_sd_rank + evap_rank_slope]

# Remove datasets failing pe_ratio_check
filtered_data <- dataset_ranks[pe_ratio_check == TRUE]

# Apply penalties for other failed checks
filtered_data[, all_ranks_adjusted := all_ranks]
filtered_data[prec_check_significance == FALSE | prec_check_non_significance == FALSE,
              all_ranks_adjusted := all_ranks_adjusted * 1.5]
filtered_data[evap_check_significance == FALSE | evap_check_non_significance == FALSE,
              all_ranks_adjusted := all_ranks_adjusted * 1.5]

# Calculate probabilistic weights
lambda      <- 0.05 #to minimize the probability of low scores

filtered_data[, weight_scaled := exp(-lambda * all_ranks_adjusted)]
filtered_data[, prob_weight := weight_scaled / sum(weight_scaled, na.rm = T), by = .(lon, lat)]

saveRDS(filtered_data, file.path(PATH_OUTPUT_DATA, 'dataset_weights.Rds'))



n_members <- 100

sample_ensemble <- function(values, weights, n = 100) {
  sample(values, size = n, replace = TRUE, prob = weights)
}

ensemble_samples <- filtered_data[, {
  sampled <- sample_ensemble(dataset, prob_weight, n_members)
  .(member = 1:n_members, dataset = sampled)
}, by = .(lon, lat)]


avail_flux <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'avail_flux_periods.Rds'))
to_plot <- merge(ensemble_samples, avail_flux, by = c('lon', 'lat', 'dataset'), allow.cartesian = T) 

water_avail <- to_plot[, .(water_avail = mean(avail)), .(member, period)]
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


