source('source/exeves_ind.R')
library(data.table)

region <- 'india'

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_climate_', region, '.rds')) # with climate type

exeves_events <- exeves[event_id == TRUE]

#old
# event_evap_intensity_climate <- exeves_events[
#   !is.na(event_id),
#   .(value = round(mean(evap), 2)),
#   by = .(KG_class_2, lon, lat, period)
# ]

# ### intensity
# event_evap_intensity_climate[, diff_value := diff(value), by = .(KG_class_2, lon, lat)]
# event_evap_intensity_climate[, ratio := 1 + diff_value / value, by = .(KG_class_2, lon, lat, period)]
# event_evap_intensity_climate[, variable := "Intensity (E)"]
# 
# summary_by_climate <- event_evap_intensity_climate[
#   , .(mean_ratio = mean(ratio, na.rm = TRUE),
#       mean_diff = mean(diff_value, na.rm = TRUE)),
#   by = KG_class_2
# ]
# 
# summary_by_climate_clean <- summary_by_climate %>%
#   mutate(KG_class_2 = factor(KG_class_2, levels = summary_by_climate$KG_class_2[order(mean_diff)]))

#new
event_evap_intensity_climate <- exeves[
  !is.na(event_id),
  .(value = mean(evap)),
  by = .(KG_class_2, lon, lat, period)
]

setorder(event_evap_intensity_climate, KG_class_2, lon, lat, period)


# event_evap_intensity_climate[
#   , diff_value := data.table::shift(value) - value,
#   by = .(KG_class_2, lon, lat)
# ]

event_evap_intensity_climate[
  , `:=`(
    before = value[period == "up_to_2001"],
    after  = value[period == "after_2001"]
  ),
  by = .(KG_class_2, lon, lat)
]

# event_evap_intensity_climate[
#   , ratio := value / data.table::shift(value),
#   by = .(KG_class_2, lon, lat)
# ]

event_evap_intensity_climate[
  , diff_value := before - after
]

event_evap_intensity_climate[
  , ratio := after / before
]

summary_by_climate <- event_evap_intensity_climate[
  !is.na(ratio),
  .(mean_ratio = mean(ratio),
    mean_diff = mean(diff_value)),
  by = KG_class_2
]
###
summary_by_climate_clean <- summary_by_climate %>%
  mutate(KG_class_2 = factor(KG_class_2, levels = summary_by_climate$KG_class_2[order(mean_diff)]))

summary_by_climate_clean$KG_class_2 <- factor(
  summary_by_climate_clean$KG_class_2,
  levels = c("f", "m", "w", "s", "W", "S")
)

climate_labels <- c("W" = "Desert", "w" = "Winter dry", "S" = "Steppe", "s" = "Summer dry", "m" = "Monsoonal", "f" = "Fully humid", "NA"  = "Unkwn")

climate_colors <- c("W" = "#8c510a", "S" = "#d8b365", "w" = "#c7eae5", "s" = "#f6e8c3", "m" = "#5ab4ac", "f" = "#01665e", "NA"  = "grey80")

# 
# ## Changes in exeves by Climate type ----dashed line-represent mean_ratio (sec.axis)
# ggplot(summary_by_climate_clean, aes(x = KG_class_2)) +
#   geom_col(aes(y = mean_diff, fill = KG_class_2), width = 0.5) +
#   geom_point(aes(y = mean_ratio), size = 4, color = "#645F5F") +
#   geom_line(aes(y = mean_ratio, group = 1), color = "#645F5F", linetype = "dashed") +
#   scale_x_discrete(labels = climate_labels) +
#   scale_y_continuous(name = "Mean diff. (abs)", sec.axis = sec_axis(~ ., name = "Mean ratio (rel.)")) +
#   scale_fill_manual(values = climate_colors, name = "Water Availability") +
#   labs(x = "Climate type (Köppen–Geiger)",) +
#   theme_minimal(base_size = 12) +
#   theme(legend.position = "none")+
#   theme(
#     # plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
#         panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
#         panel.grid.minor = element_line(colour = scales::alpha("black", 0.05)))


###
scale_factor <- max(summary_by_climate_clean$mean_diff) /
  max(summary_by_climate_clean$mean_ratio)

ggplot(summary_by_climate_clean, aes(x = KG_class_2)) +
  geom_col(aes(y = mean_diff, fill = KG_class_2), width = 0.5) +
  geom_line(aes(y = mean_ratio * scale_factor, group = 1),
            color = "#085499", linetype = "dashed", linewidth = 0.68) +
  geom_point(aes(y = mean_ratio * scale_factor),
             size = 2.3, color = "#085499") +
  scale_y_continuous(
    name = "Mean diff. (abs.)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Mean ratio (rel.)")
  ) +
  scale_x_discrete(labels = climate_labels) +
  scale_fill_manual(values = climate_colors, name = "Water Availability") +
  labs(x = "Climate type") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(colour = scales::alpha("black", 0.08)),
    panel.grid.minor = element_line(colour = scales::alpha("black", 0.05))
  )+
theme(
  axis.title.y.right = element_text(color = "#085499"),
  axis.text.y.right  = element_text(color = "#085499"),
  axis.title.x = element_text(color = "black"),
  axis.title.y = element_text(color = "black"),
  axis.text.x = element_text(color = "black"),
  axis.text.y = element_text(color = "black")
)

ggsave(paste0(PATH_OUTPUT_FIGURES, "exeves_climate_type_ind_gleam4_int2.pdf"), width = 8, height = 6)

### Frequency #####
event_frequency_period <- exeves[!is.na(event_id), .(value = .N), by = .(lon, lat, period, KG_class_2)]
setorder(event_frequency_period, lon, lat, period)
event_frequency_period[, diff_value := diff(value), by = .(lon, lat)]
event_frequency_period[, ratio := 1 + diff_value / value, by = .(lon, lat, period)]
event_frequency_period$variable <- factor("ExEvEs Frequency")

summary_by_climate <- event_frequency_period[
  , .(mean_ratio = mean(ratio, na.rm = TRUE),
      mean_diff = mean(diff_value, na.rm = TRUE)),
  by = KG_class_2
]

summary_by_climate_clean <- summary_by_climate %>%
  mutate(KG_class_2 = factor(KG_class_2, levels = summary_by_climate$KG_class_2[order(mean_diff)]))


climate_labels <- c("W" = "Desert", "w" = "Winter dry", "S" = "Steppe", "s" = "Summer dry", "m" = "Monsoonal", "f" = "Fully humid", "NA"  = "Unkwn")

climate_colors <- c("W" = "#8c510a", "S" = "#d8b365", "w" = "#c7eae5", "s" = "#f6e8c3", "m" = "#5ab4ac", "f" = "#01665e", "NA"  = "grey80")

## Changes in exeves by Climate type ----dashed line-represent mean_ratio (sec.axis)
ggplot(summary_by_climate_clean, aes(x = KG_class_2)) +
  geom_col(aes(y = mean_diff, fill = KG_class_2), width = 0.5) +
  geom_point(aes(y = mean_ratio), size = 4, color = "#645F5F") +
  geom_line(aes(y = mean_ratio, group = 1), color = "#645F5F", linetype = "dashed") +
  scale_x_discrete(labels = climate_labels) +
  scale_y_continuous(name = "Mean diff. (abs)", sec.axis = sec_axis(~ ., name = "Mean ratio (rel.)")) +
  scale_fill_manual(values = climate_colors, name = "Water Availability") +
  labs(x = "Climate type (Köppen–Geiger)",) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(paste0(PATH_OUTPUT_FIGURES, "exeves_climate_type_ind_gleam4_fq.pdf"), width = 8, height = 9)


## Severity 
event_evap_severity_period <- exeves[!is.na(event_id), .(value = sum(evap)), by = .(lon, lat, period, KG_class_2)]
setorder(event_evap_severity_period, lon, lat, period)
event_evap_severity_period[, diff_value := diff(value), by = .(lon, lat)]
event_evap_severity_period[, ratio := 1 + diff_value / value, by = .(lon, lat)]
event_evap_severity_period[, variable := "Evaporation (ExEvEs)"]

summary_by_climate <- event_evap_severity_period[
  , .(mean_ratio = mean(ratio, na.rm = TRUE),
      mean_diff = mean(diff_value, na.rm = TRUE)),
  by = KG_class_2
]

summary_by_climate_clean <- summary_by_climate %>%
  mutate(KG_class_2 = factor(KG_class_2, levels = summary_by_climate$KG_class_2[order(mean_diff)]))


climate_labels <- c("W" = "Desert", "w" = "Winter dry", "S" = "Steppe", "s" = "Summer dry", "m" = "Monsoonal", "f" = "Fully humid", "NA"  = "Unkwn")

climate_colors <- c("W" = "#8c510a", "S" = "#d8b365", "w" = "#c7eae5", "s" = "#f6e8c3", "m" = "#5ab4ac", "f" = "#01665e", "NA"  = "grey80")

ggplot(summary_by_climate_clean, aes(x = KG_class_2)) +
  geom_col(aes(y = mean_diff, fill = KG_class_2), width = 0.5) +
  geom_point(aes(y = mean_ratio), size = 4, color = "#645F5F") +
  geom_line(aes(y = mean_ratio, group = 1), color = "#645F5F", linetype = "dashed") +
  scale_x_discrete(labels = climate_labels) +
  scale_y_continuous(name = "Mean diff. (abs)", sec.axis = sec_axis(~ ., name = "Mean ratio (rel.)")) +
  scale_fill_manual(values = climate_colors, name = "Water Availability") +
  labs(x = "Climate type (Köppen–Geiger)",) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(paste0(PATH_OUTPUT_FIGURES, "exeves_climate_type_ind_gleam4_sev.pdf"), width = 8, height = 9)
