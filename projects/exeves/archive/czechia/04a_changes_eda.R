source('source/exeves.R')

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

# Stationary
## Severity & Intensity
total_evap_period <- exeves[, .(value = sum(value)), .(grid_id, period)]
total_evap_period[, mean(value), period]$V1 

exeves[!is.na(extreme_id), sum(value, na.rm = T), period]$V1 / 
  exeves[!is.na(event_id), sum(value, na.rm = T), period]$V1 

event_period <- exeves[!is.na(event_id), .(value = sum(value)), .(grid_id, period)]
event_period[, mean(value), period]$V1 / total_evap_period[, mean(value), period]$V1 

total_evap_period_monthly <- exeves[, .(value = sum(value)), .(grid_id, month(date), period)]
ggplot(total_evap_period_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

event_period_monthly <- exeves[!is.na(event_id), .(value = mean(value)), .(grid_id, month(date), period)]
ggplot(event_period_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

event_period_monthly <- exeves[!is.na(event_id), .(value = sum(value)), .(grid_id, month(date), period)]
ggplot(event_period_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

event_period_monthly_std <- exeves[!is.na(event_id), .(value = mean(std_value)), .(grid_id, month(date), period)]
ggplot(event_period_monthly_std) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

exeves_season <- unique(exeves[, .(value = sum(value)), .(year(date), season)])
exeves_season_exeves <- unique(exeves[!is.na(event_id), .(value = sum(value)), .(year(date), season)])
ggplot(exeves_season) +
  geom_line(aes(x = year, y = value)) +
  geom_smooth(aes(x = year, y = value), se = 0, method = 'gam') +
  geom_line(data = exeves_season_exeves, aes(x = year, y = value), col = 'darkred') +
  geom_smooth(data = exeves_season_exeves, aes(x = year, y = value), se = 0, method = 'gam', col = 'darkred') +
  facet_wrap(~season, scales = 'free') +
  theme_minimal()

## Frequency
exeves[, start_month := month(min(date)), .(grid_id, event_id)]
unique_exeves <- exeves[!is.na(event_id), .(grid_id, event_id, start_month, period)]
unique_exeves <- unique_exeves[, .(event_id = unique(event_id)), .(grid_id, start_month, period)]
event_frequency_period <- unique_exeves[, .(value = .N), .(grid_id, start_month, period)]

ggplot(event_frequency_period) +
  geom_boxplot(aes(x = factor(start_month), y = value, fill = period)) +
  theme_minimal()

extreme_frequency_period <- exeves[!is.na(extreme_id), .(grid_id, extreme_id, start_month, period)]
extreme_frequency_period <- extreme_frequency_period[, unique(extreme_id), .(grid_id, start_month, period)]
extreme_frequency_period <- extreme_frequency_period[, .(value = .N), .(grid_id, start_month, period)]

ggplot(extreme_frequency_period) +
  geom_boxplot(aes(x = factor(start_month), y = value, fill = period)) +
  theme_minimal()

## Duration
event_duration_period <- exeves[!is.na(event_id), .(value = .N), .(event_id, grid_id, start_month, period)]
event_duration_period <- event_duration_period[, .(value = mean(value)), .(grid_id, start_month, period)]

ggplot(event_duration_period) +
  geom_boxplot(aes(x = factor(start_month), y = value, fill = period)) +
  theme_minimal()

## ExEvEs and Extremes
exeves[, event_day := seq_len(.N), by = .(event_id, grid_id)] 
exeves[!is.na(event_id), extreme_day := seq_len(.N), by = .(event_id, grid_id)]
exeves[, event_duration := .N, by = .(event_id, grid_id)]
exeves[!is.na(extreme_id), extremes_per_event := .N, by = .(event_id, grid_id)]

evap_max <- exeves[exeves[, .I[value == max(value)], .(event_id, grid_id)]$V1]
evap_max[extreme_day == 1, first_extreme_day := event_day]
evap_max[, day_vs_duration := event_day/event_duration]
evap_max[, first_day_vs_duration := first_extreme_day/event_duration]

to_plot_monthly <- evap_max[!is.na(event_id) & event_duration < 25, .(value = event_day), .(period, month(date))]
ggplot(to_plot_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

evap_max[!is.na(event_id) & event_duration < 25, mean(extremes_per_event, na.rm = T), .(period, month(date))]

to_plot_monthly <- evap_max[!is.na(event_id) & event_duration < 25, .(value = extremes_per_event), .(period, month(date))]
ggplot(to_plot_monthly) +
  geom_boxplot(aes(x = factor(month), y = value, fill = period)) +
  theme_minimal()

aa <- evap_max[!is.na(event_id), .(mean(event_day, na.rm = T)), .(period, event_duration, month(date))]
ggplot(aa[event_duration < 10]) +
  geom_line(aes(x = event_duration, y = V1, col = period)) +
  facet_wrap(~month) +
  theme_minimal()

# Non-stationary
## Repeat as above for event_qr_id and extreme_qr_id
