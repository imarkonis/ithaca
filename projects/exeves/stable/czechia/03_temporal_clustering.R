install.packages('HKprocess')

source('source/exeves.R')
library(stats)
#library(HKprocess)

axis_decimal <- function(x) sprintf("%.1f", x)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_grid.rds'))
max_lag <- 7
n_grids <- exeves[, max(grid_id)]

names(evap)[3] <- "evap"
exeves[, value := NULL]
exeves_all <- merge(evap, exeves, all.x = TRUE, by = c("grid_id", "date"))

dummy <- exeves[, .(grid_id, std_value)]
exeves_acf <- dummy[, sapply(.SD, function(x) acf(x, lag.max = max_lag, plot = FALSE)$acf), grid_id]
exeves_acf[, lag := rep(1:(1 + max_lag), n_grids)]
acf_table <- dcast(exeves_acf, grid_id~lag, value.var = 'V1')
acf_lag_means <- apply(acf_table, 2, mean)[-1]

#mleHK(exeves[grid_id == 150, .(std_value)]$std_value) #should run for all grid cells but it is too slow

evap_acf <- data.table(lag = 0:max_lag, acf_lag_means, 
           q05 = apply(acf_table, 2, quantile, 0.05)[-1], 
           q95 = apply(acf_table, 2, quantile, 0.95)[-1])

## Adding CAMELE
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '_camele.rds'))
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_grid_camele.rds'))
n_grids <- exeves[, max(grid_id)]

names(evap)[3] <- "evap"
exeves[, value := NULL]
exeves_all_camele <- merge(evap, exeves, all.x = TRUE, by = c("grid_id", "date"))

dummy <- exeves[, .(grid_id, std_value)]
exeves_acf <- dummy[, sapply(.SD, function(x) acf(x, lag.max = max_lag, plot = FALSE)$acf), grid_id]
exeves_acf[, lag := rep(1:(1 + max_lag), n_grids)]
acf_table <- dcast(exeves_acf, grid_id~lag, value.var = 'V1')
acf_lag_means <- apply(acf_table, 2, mean)[-1]

#mleHK(exeves[grid_id == 150, .(std_value)]$std_value) #should run for all grid cells but it is too slow

evap_acf_camele <- data.table(lag = 0:max_lag, acf_lag_means, 
                       q05 = apply(acf_table, 2, quantile, 0.05)[-1], 
                       q95 = apply(acf_table, 2, quantile, 0.95)[-1])

## theoretical AR model
ar_model_ensemble <- replicate(1000, arima.sim(model = list(order = c(1, 0, 0), ar = acf_lag_means[2]), n = 15340))
ensemble_acf <- apply(ar_model_ensemble, 2, acf, max_lag, plot = FALSE)
ensemble_acf_lag_means <- sapply(ensemble_acf, '[[', 1)
evap_acf[, ar_mean := rowMeans(ensemble_acf_lag_means)]
evap_acf[, ar_q95 := apply(ensemble_acf_lag_means, 1, function(x) quantile(x, 0.95))]
evap_acf[, ar_q05 := apply(ensemble_acf_lag_means, 1, function(x) quantile(x, 0.05))]

gg_acf <- ggplot(evap_acf, aes(x = lag)) +
  geom_hline(yintercept = 0, col = 'grey50') + 
  geom_point(aes(y = acf_lag_means), col = colset_subdued_prof[4]) + 
  geom_line(aes(y = acf_lag_means), col = colset_subdued_prof[4], linewidth = 0.5) +
  geom_line(aes(y = q05), col = colset_subdued_prof[4], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = q95), col = colset_subdued_prof[4], linetype = 3, linewidth = 0.5) +
  geom_point(data = evap_acf_camele, aes(y = acf_lag_means), col = colset_subdued_prof[3]) + 
  geom_line(data = evap_acf_camele, aes(y = acf_lag_means), col = colset_subdued_prof[3], linewidth = 0.5) +
  geom_line(data = evap_acf_camele, aes(y = q05), col = colset_subdued_prof[3], linetype = 3, linewidth = 0.5) +
  geom_line(data = evap_acf_camele, aes(y = q95), col = colset_subdued_prof[3], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = ar_mean), col = colset_subdued_prof[1], linewidth = 0.5) +
  geom_point(aes(y = ar_mean), col = colset_subdued_prof[1]) + 
  geom_line(aes(y = ar_q05), col = colset_subdued_prof[1], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = ar_q95), col = colset_subdued_prof[1], linetype = 3, linewidth = 0.5) +
  xlab('Lag (day)') +
  ylab('Auto-correlation coef.') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max_lag), breaks = seq(0, max_lag, 1)) +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

sample_grid_cell <- exeves_all[grid_id == 100]
sample_year <- 2003

warm_season_start <- as.Date(paste0(sample_year, '-04-01'))
warm_season_end <- as.Date(paste0(sample_year, '-10-01'))
cold_season_start <- copy(warm_season_end)
cold_season_end <-  as.Date(paste0(sample_year + 1, '-04-01'))

definition_names <- data.frame(
  x = warm_season_start + lubridate::days(8),
  y = c(0.6, 0.4, 0.2, 0),
  text = c("Q80/Q95", "Mean/Q95", "Mean/Q95*", "Q80")
)

gg_sample_warm <- ggplot(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end]) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(event_80_95_id)],
             aes(date, 0.6), col = '#a9cce0', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(event_id)],
             aes(date, 0.4), col = '#7cb47c', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(event_qr_id)],
             aes(date, 0.2), col = '#fcc47c', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(event_80_id)],
             aes(date, 0), col = '#c07878', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[3], size = 4, shape = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(event_80_95_id)],
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[4]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  geom_text(data = definition_names, aes(x, y, label = text), cex = 2.5) +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

gg_sample_cold <- ggplot(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end]) +
  geom_point(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end & !is.na(event_80_95_id)],
             aes(date, -0.05), col = '#a9cce0', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end & !is.na(event_id)],
             aes(date, -0.15), col = '#7cb47c', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end & !is.na(event_qr_id)],
             aes(date, -0.25), col = '#fcc47c', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end & !is.na(event_80_id)],
             aes(date, -0.35), col = '#c07878', size = 2, shape = 15) +
  geom_point(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end & !is.na(extreme_qr_id)],
             aes(date, evap), col = colset_subdued_prof[3], size = 4, shape = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_point(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end & !is.na(event_80_95_id)],
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = sample_grid_cell[date >= cold_season_start & date <= cold_season_end & !is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[4]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

ggarrange(gg_acf, gg_sample_warm, gg_sample_cold,
          ncol = 1, labels = c("A", "B", "C"))

ggsave(paste0(PATH_OUTPUT_FIGURES, "clustering.png"), width = 9, height = 12)
