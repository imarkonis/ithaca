source('source/exeves.R')
library(stats)
library(HKprocess)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_grid.rds'))
max_lag <- 10
n_grids <- exeves[, max(grid_id)]

names(evap)[3] <- "evap"
exeves[, value := NULL]
exeves_all <- merge(evap, exeves, all.x = TRUE, by = c("grid_id", "date"))

dummy <- exeves[, .(grid_id, std_value)]
exeves_acf <- dummy[, sapply(.SD, function(x) acf(x, lag.max = max_lag, plot = FALSE)$acf), grid_id]
exeves_acf[, lag := rep(1:(1 + max_lag), n_grids)]
acf_table <- dcast(exeves_acf, grid_id~lag, value.var = 'V1')
acf_lag_means <- apply(acf_table, 2, mean)[-1]

apply(acf_table, 2, quantile, 0.95)
apply(acf_table, 2, quantile, 0.05)

mleHK(exeves[grid_id == 150, .(std_value)]$std_value)

evap_acf <- data.table(lag = 0:max_lag, acf_lag_means, 
           q05 = apply(acf_table, 2, quantile, 0.05)[-1], 
           q95 = apply(acf_table, 2, quantile, 0.95)[-1])

ar_model_ensemble <- replicate(1000, arima.sim(model = list(order = c(1, 0, 0), ar = acf_lag_means[2]), n = 15340))
ensemble_acf <- apply(ar_model_ensemble, 2, acf, max_lag, plot = FALSE)
ensemble_acf_lag_means <- sapply(ensemble_acf, '[[', 1)
evap_acf[, ar_mean := rowMeans(ensemble_acf_lag_means)]
evap_acf[, ar_q95 := apply(ensemble_acf_lag_means, 1, function(x) quantile(x, 0.95))]
evap_acf[, ar_q05 := apply(ensemble_acf_lag_means, 1, function(x) quantile(x, 0.05))]

gg_acf <- ggplot(evap_acf, aes(x = lag)) +
  geom_hline(yintercept = 0, col = 'grey50') + 
  geom_point(aes(y = acf_lag_means), col = colset_subdued_prof[2]) + 
  geom_line(aes(y = acf_lag_means), col = colset_subdued_prof[2], linewidth = 0.5) +
  geom_line(aes(y = q05), col = colset_subdued_prof[2], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = q95), col = colset_subdued_prof[2], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = ar_mean), col = colset_subdued_prof[1], linewidth = 0.5) +
  geom_point(aes(y = ar_mean), col = colset_subdued_prof[1]) + 
  geom_line(aes(y = ar_q05), col = colset_subdued_prof[1], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = ar_q95), col = colset_subdued_prof[1], linetype = 3, linewidth = 0.5) +
  xlab('Lag (day)') +
  ylab('Auto-correlation coef.') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max_lag), breaks = seq(0, max_lag, 1)) +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

test <- exeves_all[grid_id == 100]
test_year <- 2003

warm_season_start <- paste0(test_year, '-04-01')
warm_season_end <- paste0(test_year, '-10-01')
cold_season_start <- copy(warm_season_end)
cold_season_end <-  paste0(test_year + 1, '-04-01')

axis_decimal <- function(x) sprintf("%.1f", x)

gg_sample_warm <- ggplot(data = test[date >= warm_season_start & date <= warm_season_end]) +
  geom_point(data = test[date >= warm_season_start & date <= warm_season_end & !is.na(event_qr_id)],
             aes(date, evap), col = colset_subdued_prof[3], size = 5, shape = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_point(data = test[date >= warm_season_start & date <= warm_season_end & !is.na(event_id)],
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = test[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[4]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

gg_sample_cold <- ggplot(data = test[date >= cold_season_start & date <= cold_season_end]) +
  geom_point(data = test[date >= cold_season_start & date <= cold_season_end & !is.na(event_qr_id)],
             aes(date, evap), col = colset_subdued_prof[3], size = 5, shape = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_point(data = test[date >= cold_season_start & date <= cold_season_end & !is.na(event_id)],
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = test[date >= cold_season_start & date <= cold_season_end & !is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[4]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

ggarrange(gg_acf, gg_sample_warm, gg_sample_cold,
          ncol = 1, labels = c("A", "B", "C"))

ggsave(paste0(PATH_OUTPUT_FIGURES, "clustering.png"), width = 9, height = 12)
