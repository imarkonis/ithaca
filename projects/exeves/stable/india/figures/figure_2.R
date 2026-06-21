### check code from line 145 for plot updated as needed by Akbar ##########
source("~/ithaca_sp/projects/exeves/stable/india/00_initialize.R")
source('source/exeves_ind.R')

region <- 'india'

exeves_drivers <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))

### Onset/termination day
onset_date <- exeves_drivers[event_day == 1 & conditions == 'ExEvE', .(date = date), grid_id]
termination_date <- exeves_drivers[event_day == event_duration, .(date = date + 1), grid_id]
termination_date <- termination_date[date < '2023-01-01']

exeves_drivers <- exeves_drivers[, .(grid_id, date, conditions,
                                     evap, swrad, lwrad, prec, sensible, temp, KG_class_2)]

non_exeves_values <- melt(exeves_drivers[conditions == 'non-ExEvE'], id.vars = c('grid_id', 'date', 'conditions', 'KG_class_2')) 
non_exeves_means <- non_exeves_values[, mean(value), .(month = month(date), conditions, variable, KG_class_2)]

onset_values <- merge(onset_date,
                       exeves_drivers,
                       by = c('grid_id', 'date'), all.x = TRUE)
onset_values[, month := month(date)][, date := NULL]
onset_values <- melt(onset_values, id.vars = c('grid_id', 'month', 'conditions', 'KG_class_2'))
onset_means <- onset_values[, mean(value), .(month, conditions, variable, KG_class_2)]
levels(onset_means$conditions)[1] <- "Onset"

termination_values <- merge(termination_date, 
                       exeves_drivers, 
                       by = c('grid_id', 'date'), all.x = TRUE)
termination_values[, month := month(date)][, date := NULL]
termination_values <- melt(termination_values, id.vars = c('grid_id', 'month', 'conditions', 'KG_class_2')) 
termination_means <- termination_values[, mean(value), .(month, conditions, variable, KG_class_2)]
levels(termination_means$conditions)[2] <- "Termination"

# ### added this line
all_levels <- c("Onset", "Termination", "non-ExEvE")

onset_means$conditions <- factor(onset_means$conditions, levels = all_levels)
termination_means$conditions <- factor(termination_means$conditions, levels = all_levels)
non_exeves_means$conditions <- factor(non_exeves_means$conditions, levels = all_levels)

# ### added this line
# onset_means <- onset_values[, mean(value), .(month, conditions, variable, KG_class_2)]

onset_termination <- rbind(onset_means, termination_means, non_exeves_means)
names(onset_termination)[5] <- "value"
onset_termination$conditions <- factor(onset_termination$conditions,
                                       levels = c("Onset", "Termination", "non-ExEvE"))

onset_termination$variable <- factor(onset_termination$variable,
                                     levels = levels(onset_termination$variable),
                                     labels = c( "'Evaporation (mm/day)'",
                                                 "'SW radiation W/'*m^2*''",
                                                 "'LW radiation W/'*m^2*''",
                                                 "'Precipitation (mm/day)'",
                                                 "'Sensible Heat W/'*m^2*''",
                                                 "'Temperature (°C)'"))
levels(onset_termination$variable)

climate_labels <- c("W" = "Desert", "w" = "Winter dry", "S" = "Steppe",
                    "s" = "Summer dry", "m" = "Monsoonal", "f" = "Fully humid")

onset_termination <- onset_termination[!is.na(onset_termination$conditions), ]
onset_termination <- onset_termination[!(is.na(KG_class_2) | KG_class_2 == ""), ]

ggplot(onset_termination) +
  geom_line(aes(y = value, x = factor(month), col = conditions, group = conditions)) +
  geom_point(aes(y = value, x = factor(month), col = conditions, group = conditions)) +
  geom_line(data = onset_termination[conditions != "non-ExEvE"], aes(y = value, x = factor(month), group = month), 
            col = SUBDUED_PROF_PALETTE[2], lty = 3) +
  # facet_wrap(~variable, scales = "free",
  #            strip.position = "left",
  #            labeller = label_parsed) +
  # facet_grid(KG_class_2 ~ variable, 
  #            scales = "free_y", 
  #            labeller = labeller(KG_class_2 = climate_labels), 
  #            switch = "y")+
  facet_wrap(~ KG_class_2 + variable, scales = "free_y",
             labeller = labeller(KG_class_2 = climate_labels)) +
  labs(colour = "Conditions") +
  xlab("Month") +
  ylab("") +
  scale_color_manual(values = c(SUBDUED_PROF_PALETTE[c(2, 1)], "grey50")) +
  theme_linedraw() +
  theme(#panel.grid.minor = element_line(colour = "grey60", size = 0.03),
        #panel.grid.major = element_line(colour = "grey60", size = 0.02),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(colour = 'black'),
        strip.text.y.left = element_text(angle = 0))


ggsave(paste0(PATH_OUTPUT_FIGURES, "onset_termination_ind_gleam_clim2.1.pdf"), width = 16, height = 14)


###------------------------ by climate types --------
climate_labels <- c("W" = "Desert", "w" = "Winter dry", "S" = "Steppe",
                    "s" = "Summer dry", "m" = "Monsoonal", "f" = "Fully humid")

onset_termination <- onset_termination %>%
  mutate(KG_label = recode(KG_class_2, !!!climate_labels))

onset_termination$conditions <- factor(onset_termination$conditions,
                                       levels = c("Onset", "Termination", "non-ExEvE"))

onset_termination$KG_class_2 <- as.factor(onset_termination$KG_class_2)

climate_types <- unique(onset_termination$KG_label)

pdf(file = paste0(PATH_OUTPUT_FIGURES, "onset_termination_ind_gleam_clim2.pdf"), width = 9, height = 6)

for (clim in climate_types) {
  subset_data <- onset_termination[KG_label == clim]
  
  if (nrow(subset_data) == 0) next
  
  p <- ggplot(subset_data) +
    geom_line(aes(y = value, x = factor(month), col = conditions, group = conditions)) +
    geom_point(aes(y = value, x = factor(month), col = conditions, group = conditions)) +
    geom_line(data = subset_data[conditions != "non-ExEvE"], 
              aes(y = value, x = factor(month), group = month), 
              col = SUBDUED_PROF_PALETTE[2], lty = 3) +
    facet_wrap(~variable, scales = "free", 
               strip.position = "left", 
               labeller = label_parsed) +
    labs(title = paste("Climate Type:", clim),
         colour = "Conditions", x = "Month", y = "") +
    scale_color_manual(values = c(SUBDUED_PROF_PALETTE[c(2, 1)], "grey50")) +
    theme_linedraw() + 
    theme(panel.grid.minor = element_line(colour = "grey60"),
          panel.grid.major = element_line(colour = "grey60"),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(colour = 'black'))
  
  print(p)
}

dev.off()


######## updated as needed by Akbar ##########

climate_labels <- c("W" = "Desert", "w" = "Winter dry", "S" = "Steppe",
                    "s" = "Summer dry", "m" = "Monsoonal", "f" = "Fully humid")

onset_termination <- onset_termination %>%
  mutate(KG_label = recode(KG_class_2, !!!climate_labels))

onset_termination$conditions <- factor(onset_termination$conditions,
                                       levels = c("Onset", "Termination", "non-ExEvE"))

onset_termination$variable <- gsub("[\\*']", "", as.character(onset_termination$variable))

var_labels <- c(
  "Evaporation (mm/day)" = "Evaporation~(mm/day)",
  "SW radiation W/m^2" = "SW~radiation~(W/m^2)",
  "LW radiation W/m^2" = "LW~radiation~(W/m^2)",
  "Precipitation (mm/day)" = "Precipitation~(mm/day)",
  "Sensible Heat W/m^2" = "Sensible~heat~(W/m^2)",
  "Temperature (°C)" = "Temperature~(degree*C)"
)

pdf(paste0(PATH_OUTPUT_FIGURES, "onset_termination_ind_gleam_clim2.pdf"),
    width = 15.5, height = 12)

p <- ggplot(onset_termination) +
  
  geom_line(aes(x = month, y = value, colour = conditions, group = conditions)) +
  geom_point(aes(x = month, y = value, colour = conditions, group = conditions)) +
  
  geom_line(data = subset(onset_termination, conditions != "non-ExEvE"),
            aes(x = month, y = value, group = month),
            linetype = 3,
            colour = SUBDUED_PROF_PALETTE[2]) +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(variable ~ KG_label, scales = "free_y", 
             labeller = labeller(variable = as_labeller(var_labels, label_parsed))) +
  
  labs(x = "Month",
       y = "",
       colour = " ") +
  scale_color_manual(values = c(SUBDUED_PROF_PALETTE[c(2,1)], "grey50")) +
  # theme_linedraw() +
  theme_linedraw(base_size = 14.5) +
  theme(
    # panel.grid.major = element_line(colour = "grey90"),
    # panel.grid.minor = element_line(colour = "grey95"),
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black"),
    legend.position = "bottom",   # single shared legend
    axis.title.x = element_text(margin = margin(t = 5)),
    legend.margin = margin(t = -5),
    legend.box.margin = margin(t = -5)
  )

print(p)
dev.off()

graphics.off()
