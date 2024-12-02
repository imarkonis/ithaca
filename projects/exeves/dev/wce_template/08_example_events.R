source('source/exeves.R')
region <- 'czechia'

axis_decimal <- function(x) sprintf("%.1f", x)

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))

names(evap)[3] <- "evap"
names(prec)[3] <- "prec"
names(lwrad)[3:4] <- c("lwrad", "std_lwrad")
names(swrad)[3:4] <- c("swrad", "std_swrad")
exeves[, value := NULL]

exeves_all <- merge(evap, exeves, all.x = TRUE, by = c("grid_id", "date"))
exeves_all <- merge(exeves_all, prec, by = c("grid_id", "date"))
exeves_all <- merge(exeves_all, lwrad, all.x = TRUE, by = c("grid_id", "date"))
exeves_all <- merge(exeves_all, swrad, all.x = TRUE, by = c("grid_id", "date"))

exeves_all[, event_day := seq_len(.N), by = .(event_80_95_id, grid_id)]
exeves_all[event_80_95_id > 0, extreme_day := seq_len(.N), by = .(event_80_95_id, grid_id)]
exeves_all[, event_duration := .N, by = .(event_80_95_id, grid_id)]
exeves_all[extreme_id > 0, extremes_per_event := .N, by = .(event_80_95_id, grid_id)]

sample_grid_cell <- exeves_all[grid_id == 159 & date >= '2011-12-25' & date <= '2012-1-30']
gg_jan <- ggplot(data = sample_grid_cell) +
  geom_hline(yintercept = 0) +
  geom_line(aes(date, std_value), col = colset_subdued_prof[3]) +
  geom_point(data = sample_grid_cell[!is.na(event_80_95_id)],
             aes(date, std_value), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = sample_grid_cell[!is.na(extreme_id)],
             aes(date, std_value), col = colset_subdued_prof[3], size = 5, shape = 0) +
  geom_line(aes(date, (prec/10)), col = colset_subdued_prof[2]) +
  geom_line(aes(date, std_lwrad), col = 'darkgreen', alpha = 0.6, linetype = 5) +
  geom_line(aes(date, std_swrad), col = 'darkred', alpha = 0.6, linetype = 5) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 week", minor_breaks = NULL, date_labels = "%d %b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Value (z-score)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

sample_grid_cell <- exeves_all[grid_id == 122 & date >= '2006-07-01' & date <= '2006-7-25']
gg_jun <- ggplot(data = sample_grid_cell) +
  geom_hline(yintercept = 0) +
  geom_line(aes(date, std_value), col = colset_subdued_prof[3]) +
  geom_point(data = sample_grid_cell[!is.na(event_80_95_id)],
             aes(date, std_value), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = sample_grid_cell[!is.na(extreme_id)],
             aes(date, std_value), col = colset_subdued_prof[3], size = 5, shape = 0) +
  geom_line(aes(date, (prec/10)), col = colset_subdued_prof[2]) +
  geom_line(aes(date, std_lwrad), col = 'darkgreen', alpha = 0.6, linetype = 5) +
  geom_line(aes(date, std_swrad), col = 'darkred', alpha = 0.6, linetype = 5) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 week", minor_breaks = NULL, date_labels = "%d %b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Value (z-score)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

sample_grid_cell <- exeves_all[grid_id == 122 & date >= '2018-04-16' & date <= '2018-6-07']
gg_2018 <- ggplot(data = sample_grid_cell) +
  geom_hline(yintercept = 0) +
  geom_line(aes(date, std_value), col = colset_subdued_prof[3]) +
  geom_point(data = sample_grid_cell[!is.na(event_80_95_id)],
             aes(date, std_value), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = sample_grid_cell[!is.na(extreme_id)],
             aes(date, std_value), col = colset_subdued_prof[3], size = 5, shape = 0) +
  geom_line(aes(date, (prec/10)), col = colset_subdued_prof[2]) +
  geom_line(aes(date, std_lwrad), col = 'darkgreen', alpha = 0.6, linetype = 5) +
  geom_line(aes(date, std_swrad), col = 'darkred', alpha = 0.6, linetype = 5) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 week", minor_breaks = NULL, date_labels = "%d %b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Value (z-score)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

ggarrange(gg_jan, gg_jun, gg_2018,
          ncol = 1, labels = c("A", "B", "C"))
ggsave(paste0(PATH_OUTPUT_FIGURES, "exeve_examples.png"), width = 7, height = 12)

# Absolute values
to_plot()
ggplot(data = sample_grid_cell) +
  geom_hline(yintercept = 0) +
  geom_line(aes(date, evap), col = colset_subdued_prof[3]) +
  geom_line(aes(date, prec), col = colset_subdued_prof[2]) +
  geom_point(data = sample_grid_cell[!is.na(event_80_95_id)],
             aes(date, evap), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = sample_grid_cell[!is.na(extreme_id)],
             aes(date, evap), col = colset_subdued_prof[3], size = 5, shape = 0) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 week", minor_breaks = NULL, date_labels = "%d %b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Value (z-score)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))
