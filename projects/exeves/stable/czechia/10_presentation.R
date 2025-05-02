ggplot(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end]) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

ggplot(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end]) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_line(data = pentads_to_plot,
            aes(date, pentad_mean), col = 'dark red') +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

ggplot(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end]) +
  geom_line(aes(date, std_value), col = colset_subdued_prof[3]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

pentads[, pentad_mean := mean(value), by = .(pentad, grid_id)]
pentads_to_plot <- pentads[sample_grid_cell[date >= warm_season_start & date <= warm_season_end], on = .(grid_id, date)]

ggplot(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end]) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_id)],
             aes(date, std_value), col = colset_subdued_prof[3], size = 4, shape = 0) +
  geom_line(aes(date, std_value), col = colset_subdued_prof[3]) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(event_80_95_id)],
             aes(date, std_value), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_id)],
             aes(date, std_value), col = colset_subdued_prof[4]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))


ggplot(data = sample_grid_cell[date >= warm_season_start & date <= warm_season_end]) +
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
