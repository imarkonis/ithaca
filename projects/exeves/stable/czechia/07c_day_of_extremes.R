source('source/exeves.R')

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

names(prec)[3] <- "prec"
exeves_prec <- merge(exeves[!is.na(event_80_95_id)], prec, by = c('grid_id', 'date'), all.x = TRUE)
names(exeves_prec)[5] <- 'evap'

exeves_prec[!is.na(extreme_id), extreme_evap := TRUE]

#Day of precipitation
exeves_prec[, event_day := seq_len(.N), by = .(event_80_95_id, grid_id)]
exeves_prec[, extreme_day := seq_len(.N), by = .(extreme_id, grid_id)]
exeves_prec[, event_duration := .N, by = .(event_80_95_id, grid_id)]
exeves_prec[, extremes_per_event := .N, by = .(extreme_id, event_80_95_id, grid_id)]
exeves_prec[is.na(extreme_id), extreme_day := NA]
exeves_prec[is.na(extreme_id), extremes_per_event := NA]
exeves_prec[extreme_day == 1, event_day_of_extreme := event_day]
exeves_prec[, cumsum_evap := cumsum(evap), .(grid_id, event_80_95_id)]
exeves_prec[, cumsum_prec := cumsum(prec), .(grid_id, event_80_95_id)]
exeves_prec[, cumsum_diff := cumsum_prec - cumsum_evap, .(grid_id, event_80_95_id)]

prec_max <- exeves_prec[exeves_prec[, prec == max(prec), .(event_80_95_id, grid_id)]$V1]

prec_max <- prec_max[, .(date, grid_id, event_80_95_id, prec_max = prec)]
exeves_prec <- merge(exeves_prec, prec_max, by = c('grid_id', 'event_80_95_id', 'date'), all.x = TRUE)
exeves_prec[!is.na(prec_max), event_day_of_prec_max := event_day]

exeves_prec[, event_day_of_first_extreme := min(event_day_of_extreme, na.rm = T) , .(grid_id, event_80_95_id)]
exeves_prec[, event_day_of_first_prec_max:= min(event_day_of_prec_max, na.rm = T) , .(grid_id, event_80_95_id)]

to_plot <- unique(exeves_prec[, .(grid_id, event_80_95_id, event_duration,
                                  event_day_of_first_extreme, 
                                  event_day_of_first_prec_max, 
                                  month = month(date), 
                                  period)])

gg_extreme_day <- ggplot(to_plot[event_duration == 6]) + 
  geom_vline(xintercept = 1, col = 'black') +
  geom_hline(yintercept = 0, col = 'black') +
  geom_density(aes(x = event_day_of_first_prec_max, group = event_duration), stat = "count", 
               col = SUBDUED_PROF_PALETTE[2], linetype = 2) +
  geom_density(aes(x = event_day_of_first_extreme, group = event_duration), stat = "count", 
               col = SUBDUED_PROF_PALETTE[4]) +
  facet_wrap(~ month, scales = 'free_y', ncol = 4) +
  xlab("Day of event") + 
  ylab("Number of days") + 
  theme_linedraw() +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        strip.background = element_rect(fill = 'grey30'))

to_plot <- exeves_prec[, .(Evaporation = mean(evap), Precipitation = mean(prec)), .(event_duration, event_day, month = month(date))]
to_plot <- melt(to_plot, id.vars = c("event_duration", "event_day", "month"))
colnames(to_plot)[4] <- "Variable"

gg_mean_day <- ggplot(to_plot[event_duration == 6]) + 
  geom_vline(xintercept = 1, col = 'black') +
  geom_hline(yintercept = 0, col = 'black') +
  geom_line(aes(x = event_day, y = value, col = Variable, linetype = Variable)) +
  facet_wrap(~ month, scales = 'free_y', ncol = 4) +
  xlab("Day of event") + 
  ylab("Mean (mm/day)") +
  scale_color_manual(values = SUBDUED_PROF_PALETTE[c(4, 2)]) + 
  theme_linedraw() +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        strip.background = element_rect(fill = 'grey30'))

exeves_prec[, prec_day := factor("wet")]
exeves_prec[prec < 1, prec_day := factor("dry")]

all_prec_days <- exeves_prec[, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day, month(date))]
colnames(all_prec_days)[4] <- "value"
all_prec_days$conditions <- "All days"

exeves_prec_days <- exeves_prec[!is.na(event_80_95_id), .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day, month(date))]
colnames(exeves_prec_days)[4] <- "value"
exeves_prec_days$conditions <- "ExEvEs"

non_exeves_prec_days <- exeves_prec[is.na(event_80_95_id), .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day, month(date))]
colnames(non_exeves_prec_days)[4] <- "value"
non_exeves_prec_days$conditions <- "Non-ExEvEs"

prec_days <- rbind(all_prec_days, exeves_prec_days, non_exeves_prec_days)
prec_days <- prec_days[, c(1, 3:2, 5:4)]

levels(prec_days$prec_day) <- c("Wet", "Dry")
levels(prec_days$period) <- c("Up to 2001", "After 2001")
colnames(prec_days)[3] <- "Day class"

gg_wet_ratio <- ggplot(prec_days[conditions == 'ExEvEs']) +
  geom_col(aes(x = period, y = value, fill = `Day class`),
           position = position_fill(), width = 0.7) +
  facet_wrap(~month)+
  xlab("Period") + 
  ylab("Ratio") + 
  scale_fill_manual(values = c('#a9cce0', '#fcc47c')) + 
  theme_linedraw() +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        strip.background = element_rect(fill = 'grey30'))

ggarrange(gg_extreme_day, gg_mean_day, gg_wet_ratio, 
          ncol = 1, labels = c("A", "B", "C"),
          legend = 'right', common.legend = TRUE)

ggsave(paste0(PATH_OUTPUT_FIGURES, "wet_days.png"), width = 9, height = 12)


