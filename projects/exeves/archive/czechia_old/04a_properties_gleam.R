source('source/exeves.R')
library(pRecipe)

region <- 'czechia'

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

event_comparison <- data.table(definition = factor(c("Mean/Q90", "Mean/Q90*", "Q75/Q90", "Q90", "Q90*", "Q80")))
event_comparison$frequency <- round(c(
  mean(exeves[!is.na(event_id), unique(event_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_qr_id), unique(event_qr_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_75_id), unique(event_75_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(extreme_id), unique(extreme_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(extreme_qr_id), unique(extreme_qr_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_80_id), unique(event_80_id), .(grid_id)][, .N, grid_id]$N)
) / PERIOD_LENGTH, 0) #Events per year

event_comparison$duration_mean <- round(c(
  mean(exeves[!is.na(event_id), .N, .(grid_id, event_id)]$N),
  mean(exeves[!is.na(event_qr_id), .N, .(grid_id, event_qr_id)]$N),
  mean(exeves[!is.na(event_75_id), .N, .(grid_id, event_75_id)]$N),
  mean(exeves[!is.na(extreme_id), .N, .(grid_id, extreme_id)]$N),
  mean(exeves[!is.na(extreme_qr_id), .N, .(grid_id, extreme_qr_id)]$N),
  mean(exeves[!is.na(event_80_id), .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison$duration_max <- c(
  max(exeves[!is.na(event_id), .N, .(grid_id, event_id)]$N),
  max(exeves[!is.na(event_qr_id), .N, .(grid_id, event_qr_id)]$N),
  max(exeves[!is.na(event_75_id), .N, .(grid_id, event_75_id)]$N),
  max(exeves[!is.na(extreme_id), .N, .(grid_id, extreme_id)]$N),
  max(exeves[!is.na(extreme_qr_id), .N, .(grid_id, extreme_qr_id)]$N),
  max(exeves[!is.na(event_80_id), .N, .(grid_id, event_80_id)]$N)
)

event_comparison$extreme_n <- round(c(  #### Extremes per event
  mean(exeves[!is.na(extreme_id), .N, .(grid_id, event_id)]$N),
  mean(exeves[!is.na(extreme_qr_id), .N, .(grid_id, event_qr_id)]$N),
  mean(exeves[!is.na(extreme_id), .N, .(grid_id, event_75_id)]$N),
  mean(exeves[!is.na(extreme_id), .N, .(grid_id, extreme_id)]$N),
  mean(exeves[!is.na(extreme_qr_id), .N, .(grid_id, extreme_qr_id)]$N),
  mean(exeves[!is.na(extreme_id), .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison$extreme_n_max <- round(c(  #### Extremes per event
  max(exeves[!is.na(extreme_id), .N, .(grid_id, event_id)]$N),
  max(exeves[!is.na(extreme_qr_id), .N, .(grid_id, event_qr_id)]$N),
  max(exeves[!is.na(extreme_id), .N, .(grid_id, event_75_id)]$N),
  max(exeves[!is.na(extreme_id), .N, .(grid_id, extreme_id)]$N),
  max(exeves[!is.na(extreme_qr_id), .N, .(grid_id, extreme_qr_id)]$N),
  max(exeves[!is.na(extreme_id), .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison$intensity_mean <- round(c(
  mean(exeves[!is.na(event_id), mean(value), .(grid_id, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id), mean(value), .(grid_id, event_qr_id)]$V1),
  mean(exeves[!is.na(event_75_id), mean(value), .(grid_id, event_75_id)]$V1),
  mean(exeves[!is.na(extreme_id), mean(value), .(grid_id, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id), mean(value), .(grid_id, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id), mean(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison$intensity_max <- round(c(
  max(exeves[!is.na(event_id), mean(value), .(grid_id, event_id)]$V1),
  max(exeves[!is.na(event_qr_id), mean(value), .(grid_id, event_qr_id)]$V1),
  max(exeves[!is.na(event_75_id), mean(value), .(grid_id, event_75_id)]$V1),
  max(exeves[!is.na(extreme_id), mean(value), .(grid_id, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id), mean(value), .(grid_id, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id), mean(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison$severity_mean <- round(c(
  mean(exeves[!is.na(event_id), sum(value), .(grid_id, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id), sum(value), .(grid_id, event_qr_id)]$V1),
  mean(exeves[!is.na(event_75_id), sum(value), .(grid_id, event_75_id)]$V1),
  mean(exeves[!is.na(extreme_id), sum(value), .(grid_id, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id), sum(value), .(grid_id, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id), sum(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison$severity_max <- round(c(
  max(exeves[!is.na(event_id), sum(value), .(grid_id, event_id)]$V1),
  max(exeves[!is.na(event_qr_id), sum(value), .(grid_id, event_qr_id)]$V1),
  max(exeves[!is.na(event_75_id), sum(value), .(grid_id, event_75_id)]$V1),
  max(exeves[!is.na(extreme_id), sum(value), .(grid_id, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id), sum(value), .(grid_id, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id), sum(value), .(grid_id, event_80_id)]$V1)
), 1)

write.csv(event_comparison,  paste0(PATH_OUTPUT_TABLES, region, '_definition_properties.csv'))

##Compare change
#up_to_2001
event_comparison_1 <- data.table(definition = factor(c("Mean/Q90", "Mean/Q90*", "Q75/Q90", "Q90", "Q90*", "Q80")))
event_comparison_1$frequency <- round(c(
  mean(exeves[!is.na(event_id) & period == 'up_to_2001', unique(event_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_qr_id) & period == 'up_to_2001', unique(event_qr_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_75_id) & period == 'up_to_2001', unique(event_75_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', unique(extreme_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', unique(extreme_qr_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_80_id) & period == 'up_to_2001', unique(event_80_id), .(grid_id)][, .N, grid_id]$N)
) / (0.5 * PERIOD_LENGTH), 0) #Events per year

event_comparison_1$duration_mean <- round(c(
  mean(exeves[!is.na(event_id) & period == 'up_to_2001', .N, .(grid_id, event_id)]$N),
  mean(exeves[!is.na(event_qr_id) & period == 'up_to_2001', .N, .(grid_id, event_qr_id)]$N),
  mean(exeves[!is.na(event_75_id) & period == 'up_to_2001', .N, .(grid_id, event_75_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, extreme_id)]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', .N, .(grid_id, extreme_qr_id)]$N),
  mean(exeves[!is.na(event_80_id) & period == 'up_to_2001', .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison_1$duration_max <- c(
  max(exeves[!is.na(event_id) & period == 'up_to_2001', .N, .(grid_id, event_id)]$N),
  max(exeves[!is.na(event_qr_id) & period == 'up_to_2001', .N, .(grid_id, event_qr_id)]$N),
  max(exeves[!is.na(event_75_id) & period == 'up_to_2001', .N, .(grid_id, event_75_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, extreme_id)]$N),
  max(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', .N, .(grid_id, extreme_qr_id)]$N),
  max(exeves[!is.na(event_80_id) & period == 'up_to_2001', .N, .(grid_id, event_80_id)]$N)
)

event_comparison_1$extreme_n <- round(c(  #### Extremes per event
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, event_id)]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', .N, .(grid_id, event_qr_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, event_75_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, extreme_id)]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', .N, .(grid_id, extreme_qr_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison_1$extreme_n_max <- round(c(  #### Extremes per event
  max(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, event_id)]$N),
  max(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', .N, .(grid_id, event_qr_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, event_75_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, extreme_id)]$N),
  max(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', .N, .(grid_id, extreme_qr_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'up_to_2001', .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison_1$intensity_mean <- round(c(
  mean(exeves[!is.na(event_id) & period == 'up_to_2001', mean(value), .(grid_id, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id) & period == 'up_to_2001', mean(value), .(grid_id, event_qr_id)]$V1),
  mean(exeves[!is.na(event_75_id) & period == 'up_to_2001', mean(value), .(grid_id, event_75_id)]$V1),
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', mean(value), .(grid_id, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', mean(value), .(grid_id, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id) & period == 'up_to_2001', mean(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison_1$intensity_max <- round(c(
  max(exeves[!is.na(event_id) & period == 'up_to_2001', mean(value), .(grid_id, event_id)]$V1),
  max(exeves[!is.na(event_qr_id) & period == 'up_to_2001', mean(value), .(grid_id, event_qr_id)]$V1),
  max(exeves[!is.na(event_75_id) & period == 'up_to_2001', mean(value), .(grid_id, event_75_id)]$V1),
  max(exeves[!is.na(extreme_id) & period == 'up_to_2001', mean(value), .(grid_id, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', mean(value), .(grid_id, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id) & period == 'up_to_2001', mean(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison_1$severity_mean <- round(c(
  mean(exeves[!is.na(event_id) & period == 'up_to_2001', sum(value), .(grid_id, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id) & period == 'up_to_2001', sum(value), .(grid_id, event_qr_id)]$V1),
  mean(exeves[!is.na(event_75_id) & period == 'up_to_2001', sum(value), .(grid_id, event_75_id)]$V1),
  mean(exeves[!is.na(extreme_id) & period == 'up_to_2001', sum(value), .(grid_id, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', sum(value), .(grid_id, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id) & period == 'up_to_2001', sum(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison_1$severity_max <- round(c(
  max(exeves[!is.na(event_id) & period == 'up_to_2001', sum(value), .(grid_id, event_id)]$V1),
  max(exeves[!is.na(event_qr_id) & period == 'up_to_2001', sum(value), .(grid_id, event_qr_id)]$V1),
  max(exeves[!is.na(event_75_id) & period == 'up_to_2001', sum(value), .(grid_id, event_75_id)]$V1),
  max(exeves[!is.na(extreme_id) & period == 'up_to_2001', sum(value), .(grid_id, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id) & period == 'up_to_2001', sum(value), .(grid_id, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id) & period == 'up_to_2001', sum(value), .(grid_id, event_80_id)]$V1)
), 1)

#after_2001
event_comparison_2 <- data.table(definition = factor(c("Mean/Q90", "Mean/Q90*", "Q75/Q90", "Q90", "Q90*", "Q80")))
event_comparison_2$frequency <- round(c(
  mean(exeves[!is.na(event_id) & period == 'after_2001', unique(event_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_qr_id) & period == 'after_2001', unique(event_qr_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_75_id) & period == 'after_2001', unique(event_75_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', unique(extreme_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'after_2001', unique(extreme_qr_id), .(grid_id)][, .N, grid_id]$N),
  mean(exeves[!is.na(event_80_id) & period == 'after_2001', unique(event_80_id), .(grid_id)][, .N, grid_id]$N)
) / (0.5 * PERIOD_LENGTH), 0) #Events per year

event_comparison_2$duration_mean <- round(c(
  mean(exeves[!is.na(event_id) & period == 'after_2001', .N, .(grid_id, event_id)]$N),
  mean(exeves[!is.na(event_qr_id) & period == 'after_2001', .N, .(grid_id, event_qr_id)]$N),
  mean(exeves[!is.na(event_75_id) & period == 'after_2001', .N, .(grid_id, event_75_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, extreme_id)]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'after_2001', .N, .(grid_id, extreme_qr_id)]$N),
  mean(exeves[!is.na(event_80_id) & period == 'after_2001', .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison_2$duration_max <- c(
  max(exeves[!is.na(event_id) & period == 'after_2001', .N, .(grid_id, event_id)]$N),
  max(exeves[!is.na(event_qr_id) & period == 'after_2001', .N, .(grid_id, event_qr_id)]$N),
  max(exeves[!is.na(event_75_id) & period == 'after_2001', .N, .(grid_id, event_75_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, extreme_id)]$N),
  max(exeves[!is.na(extreme_qr_id) & period == 'after_2001', .N, .(grid_id, extreme_qr_id)]$N),
  max(exeves[!is.na(event_80_id) & period == 'after_2001', .N, .(grid_id, event_80_id)]$N)
)

event_comparison_2$extreme_n <- round(c(  #### Extremes per event
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, event_id)]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'after_2001', .N, .(grid_id, event_qr_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, event_75_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, extreme_id)]$N),
  mean(exeves[!is.na(extreme_qr_id) & period == 'after_2001', .N, .(grid_id, extreme_qr_id)]$N),
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison_2$extreme_n_max <- round(c(  #### Extremes per event
  max(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, event_id)]$N),
  max(exeves[!is.na(extreme_qr_id) & period == 'after_2001', .N, .(grid_id, event_qr_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, event_75_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, extreme_id)]$N),
  max(exeves[!is.na(extreme_qr_id) & period == 'after_2001', .N, .(grid_id, extreme_qr_id)]$N),
  max(exeves[!is.na(extreme_id) & period == 'after_2001', .N, .(grid_id, event_80_id)]$N)
), 1)

event_comparison_2$intensity_mean <- round(c(
  mean(exeves[!is.na(event_id) & period == 'after_2001', mean(value), .(grid_id, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id) & period == 'after_2001', mean(value), .(grid_id, event_qr_id)]$V1),
  mean(exeves[!is.na(event_75_id) & period == 'after_2001', mean(value), .(grid_id, event_75_id)]$V1),
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', mean(value), .(grid_id, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id) & period == 'after_2001', mean(value), .(grid_id, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id) & period == 'after_2001', mean(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison_2$intensity_max <- round(c(
  max(exeves[!is.na(event_id) & period == 'after_2001', mean(value), .(grid_id, event_id)]$V1),
  max(exeves[!is.na(event_qr_id) & period == 'after_2001', mean(value), .(grid_id, event_qr_id)]$V1),
  max(exeves[!is.na(event_75_id) & period == 'after_2001', mean(value), .(grid_id, event_75_id)]$V1),
  max(exeves[!is.na(extreme_id) & period == 'after_2001', mean(value), .(grid_id, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id) & period == 'after_2001', mean(value), .(grid_id, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id) & period == 'after_2001', mean(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison_2$severity_mean <- round(c(
  mean(exeves[!is.na(event_id) & period == 'after_2001', sum(value), .(grid_id, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id) & period == 'after_2001', sum(value), .(grid_id, event_qr_id)]$V1),
  mean(exeves[!is.na(event_75_id) & period == 'after_2001', sum(value), .(grid_id, event_75_id)]$V1),
  mean(exeves[!is.na(extreme_id) & period == 'after_2001', sum(value), .(grid_id, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id) & period == 'after_2001', sum(value), .(grid_id, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id) & period == 'after_2001', sum(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison_2$severity_max <- round(c(
  max(exeves[!is.na(event_id) & period == 'after_2001', sum(value), .(grid_id, event_id)]$V1),
  max(exeves[!is.na(event_qr_id) & period == 'after_2001', sum(value), .(grid_id, event_qr_id)]$V1),
  max(exeves[!is.na(event_75_id) & period == 'after_2001', sum(value), .(grid_id, event_75_id)]$V1),
  max(exeves[!is.na(extreme_id) & period == 'after_2001', sum(value), .(grid_id, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id) & period == 'after_2001', sum(value), .(grid_id, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id) & period == 'after_2001', sum(value), .(grid_id, event_80_id)]$V1)
), 1)

event_comparison_change <- round(event_comparison_2/event_comparison_1, 2)
write.csv(event_comparison_change,  paste0(PATH_OUTPUT_TABLES, region, '_definition_change.csv'))
