source('source/exeves.R')

region <- 'random_locations'
exeves_drivers <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))

### Onset/termination day
onset_date <- exeves_drivers[event_day == 1 & conditions == 'ExEvE', .(date = date), KG_class_2]
termination_date <- exeves_drivers[event_day == event_duration, .(date = date + 1), KG_class_2]
termination_date <- termination_date[date < '2023-01-01']

exeves_drivers <- exeves_drivers[, .(KG_class_2, date, conditions,
                                     swrad, lwrad, temp, sensible, prec)]

non_exeves_values <- melt(exeves_drivers[conditions == 'non-ExEvE'], id.vars = c('KG_class_2', 'date', 'conditions')) 
non_exeves_means <- non_exeves_values[, mean(value), .(month = month(date), conditions, variable, KG_class_2)]

onset_values <- merge(onset_date, 
                      exeves_drivers, 
                      by = c('KG_class_2', 'date'), all.x = TRUE)
onset_values[, month := month(date)][, date := NULL]
onset_values <- melt(onset_values, id.vars = c('KG_class_2', 'month', 'conditions')) 
onset_means <- onset_values[, mean(value), .(month, conditions, variable, KG_class_2)]
levels(onset_means$conditions)[1] <- "Onset"

termination_values <- merge(termination_date, 
                            exeves_drivers, 
                            by = c('KG_class_2', 'date'), all.x = TRUE)
termination_values[, month := month(date)][, date := NULL]
termination_values <- melt(termination_values, id.vars = c('KG_class_2', 'month', 'conditions')) 
termination_means <- termination_values[, mean(value), .(month, conditions, variable, KG_class_2)]
levels(termination_means$conditions)[2] <- "Termination"

onset_termination <- rbind(onset_means, termination_means, non_exeves_means)
names(onset_termination)[5] <- "value"
onset_termination$conditions <- factor(onset_termination$conditions, 
                                       levels = c("Onset", "Termination", "non-ExEvE"))

onset_termination$variable <- factor(onset_termination$variable,
                                     levels = levels(onset_termination$variable),
                                     labels = c("'SW radiation W/'*m^2*''",
                                                "'LW radiation W/'*m^2*''",
                                                "'Temperature (°C)'",
                                                "'Sensible Heat W/'*m^2*''",
                                                "'Precipitation (mm/day)'"))

kg_classes_shown <- levels(onset_termination$KG_class_2)[c(2, 4, 8, 10)]

ggplot(onset_termination[KG_class_2 %in% kg_classes_shown]) +
  geom_line(aes(y = value, x = factor(month), col = conditions, group = conditions))+
  geom_point(aes(y = value, x = factor(month), col = conditions, group = conditions))+
  facet_wrap(KG_class_2~variable, scales = "free", ncol = 5, nrow = 4, 
             strip.position = "left", 
             labeller = label_parsed)  +
  labs(colour = "Conditions") +
  xlab("Month") +
  ylab("") +
  scale_color_manual(values = c(SUBDUED_PROF_PALETTE[c(2, 1)], "grey30")) +
  theme_linedraw() + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(colour = 'black'))



