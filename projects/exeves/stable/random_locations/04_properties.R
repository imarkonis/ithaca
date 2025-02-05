source('source/exeves.R')

region <- 'random_locations'

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

exeve_definitions <- factor(c("Q80/Q95", "Mean/Q95", "Mean/Q95*", "Q95", "Q95*", "Q80"), 
                            levels = c("Q80/Q95", "Mean/Q95", "Mean/Q95*", "Q95", "Q95*", "Q80"))

event_frequency <- rbind(
  cbind(exeves[!is.na(event_80_95_id), unique(event_80_95_id), .(KG_class_2)]
        [,.N, KG_class_2], definition = exeve_definitions[1]),
  cbind(exeves[!is.na(event_id), unique(event_id), .(KG_class_2)]
        [,.N, KG_class_2], definition = exeve_definitions[2]),
  cbind(exeves[!is.na(event_qr_id), unique(event_qr_id), .(KG_class_2)]
        [,.N, KG_class_2], definition = exeve_definitions[3]),
  cbind(exeves[!is.na(extreme_id), unique(extreme_id), .(KG_class_2)]
        [,.N, KG_class_2], definition = exeve_definitions[4]),
  cbind(exeves[!is.na(extreme_qr_id), unique(extreme_qr_id), .(KG_class_2)]
        [,.N, KG_class_2], definition = exeve_definitions[5]),
  cbind(exeves[!is.na(event_80_id), unique(event_80_id), .(KG_class_2)]
        [,.N, KG_class_2], definition = exeve_definitions[6]))

colnames(event_frequency)[2] <- 'value'
event_frequency[, value := round(value / PERIOD_LENGTH)]
event_frequency_table <- dcast(event_frequency,  KG_class_2 ~ definition)
write.csv(event_frequency_table,  paste0(PATH_OUTPUT_TABLES, region, '_frequency.csv'))

event_duration_mean <- rbind(
  cbind(exeves[!is.na(event_80_95_id), .N, .(event_80_95_id, KG_class_2)][, round(mean(N), 1), KG_class_2],
        definition = exeve_definitions[1]),
  cbind(exeves[!is.na(event_id), .N, .(event_id, KG_class_2)][, round(mean(N), 1), KG_class_2],
        definition = exeve_definitions[2]),
  cbind(exeves[!is.na(event_qr_id), .N, .(event_qr_id, KG_class_2)][, round(mean(N), 1), KG_class_2],
        definition = exeve_definitions[3]),
  cbind(exeves[!is.na(extreme_id), .N, .(extreme_id, KG_class_2)][, round(mean(N), 1), KG_class_2],
        definition = exeve_definitions[4]),
  cbind(exeves[!is.na(extreme_qr_id), .N, .(extreme_qr_id, KG_class_2)][, round(mean(N), 1), KG_class_2],
        definition = exeve_definitions[5]),
  cbind(exeves[!is.na(event_80_id), .N, .(event_80_id, KG_class_2)][, round(mean(N), 1), KG_class_2],
        definition = exeve_definitions[6]))

colnames(event_duration_mean)[2] <- 'value'
event_duration_mean_table <- dcast(event_duration_mean,  KG_class_2 ~ definition)
write.csv(event_duration_mean_table,  paste0(PATH_OUTPUT_TABLES, region, '_duration.csv'))

event_duration_max <- rbind(
  cbind(exeves[!is.na(event_80_95_id), .N, .(event_80_95_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[1]),
  cbind(exeves[!is.na(event_id), .N, .(event_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[2]),
  cbind(exeves[!is.na(event_qr_id), .N, .(event_qr_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[3]),
  cbind(exeves[!is.na(extreme_id), .N, .(extreme_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[4]),
  cbind(exeves[!is.na(extreme_qr_id), .N, .(extreme_qr_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[5]),
  cbind(exeves[!is.na(event_80_id), .N, .(event_80_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[6]))

colnames(event_duration_max)[2] <- 'value'
event_duration_max_table <- dcast(event_duration_max,  KG_class_2 ~ definition)

event_extreme_n <- rbind(
  cbind(exeves[!is.na(extreme_id), .N, .(event_80_95_id, KG_class_2)][, round(mean(N)), KG_class_2],
        definition = exeve_definitions[1]),
  cbind(exeves[!is.na(extreme_id), .N, .(event_id, KG_class_2)][, round(mean(N)), KG_class_2],
        definition = exeve_definitions[2]),
  cbind(exeves[!is.na(extreme_qr_id), .N, .(event_qr_id, KG_class_2)][, round(mean(N)), KG_class_2],
        definition = exeve_definitions[3]),
  cbind(exeves[!is.na(extreme_id), .N, .(extreme_id, KG_class_2)][, round(mean(N)), KG_class_2],
        definition = exeve_definitions[4]),
  cbind(exeves[!is.na(extreme_qr_id), .N, .(extreme_qr_id, KG_class_2)][, round(mean(N)), KG_class_2],
        definition = exeve_definitions[5]),
  cbind(exeves[!is.na(event_80_id), .N, .(event_80_id, KG_class_2)][, round(mean(N)), KG_class_2],
        definition = exeve_definitions[6]))

colnames(event_extreme_n)[2] <- 'value'
event_extreme_n_table <- dcast(event_extreme_n,  KG_class_2 ~ definition)

event_extreme_n_max <- rbind(
  cbind(exeves[!is.na(extreme_id), .N, .(event_80_95_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[1]),
  cbind(exeves[!is.na(extreme_id), .N, .(event_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[2]),
  cbind(exeves[!is.na(extreme_qr_id), .N, .(event_qr_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[3]),
  cbind(exeves[!is.na(extreme_id), .N, .(extreme_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[4]),
  cbind(exeves[!is.na(extreme_qr_id), .N, .(extreme_qr_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[5]),
  cbind(exeves[!is.na(event_80_id), .N, .(event_80_id, KG_class_2)][, round(max(N)), KG_class_2],
        definition = exeve_definitions[6]))

colnames(event_extreme_n_max)[2] <- 'value'
event_extreme_n_max_table <- dcast(event_extreme_n_max,  KG_class_2 ~ definition)

####################################################### NOT EDITED BELOW

event_comparison$intensity_mean <- round(c(
  mean(exeves[!is.na(event_id), mean(value), .(KG_class_2, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id), mean(value), .(KG_class_2, event_qr_id)]$V1),
  mean(exeves[!is.na(event_80_95_id), mean(value), .(KG_class_2, event_80_95_id)]$V1),
  mean(exeves[!is.na(extreme_id), mean(value), .(KG_class_2, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id), mean(value), .(KG_class_2, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id), mean(value), .(KG_class_2, event_80_id)]$V1)
), 1)

event_comparison$intensity_max <- round(c(
  max(exeves[!is.na(event_id), mean(value), .(KG_class_2, event_id)]$V1),
  max(exeves[!is.na(event_qr_id), mean(value), .(KG_class_2, event_qr_id)]$V1),
  max(exeves[!is.na(event_80_95_id), mean(value), .(KG_class_2, event_80_95_id)]$V1),
  max(exeves[!is.na(extreme_id), mean(value), .(KG_class_2, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id), mean(value), .(KG_class_2, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id), mean(value), .(KG_class_2, event_80_id)]$V1)
), 1)

event_comparison$severity_mean <- round(c(
  mean(exeves[!is.na(event_id), sum(value), .(KG_class_2, event_id)]$V1),
  mean(exeves[!is.na(event_qr_id), sum(value), .(KG_class_2, event_qr_id)]$V1),
  mean(exeves[!is.na(event_80_95_id), sum(value), .(KG_class_2, event_80_95_id)]$V1),
  mean(exeves[!is.na(extreme_id), sum(value), .(KG_class_2, extreme_id)]$V1),
  mean(exeves[!is.na(extreme_qr_id), sum(value), .(KG_class_2, extreme_qr_id)]$V1),
  mean(exeves[!is.na(event_80_id), sum(value), .(KG_class_2, event_80_id)]$V1)
), 1)

event_comparison$severity_max <- round(c(
  max(exeves[!is.na(event_id), sum(value), .(KG_class_2, event_id)]$V1),
  max(exeves[!is.na(event_qr_id), sum(value), .(KG_class_2, event_qr_id)]$V1),
  max(exeves[!is.na(event_80_95_id), sum(value), .(KG_class_2, event_80_95_id)]$V1),
  max(exeves[!is.na(extreme_id), sum(value), .(KG_class_2, extreme_id)]$V1),
  max(exeves[!is.na(extreme_qr_id), sum(value), .(KG_class_2, extreme_qr_id)]$V1),
  max(exeves[!is.na(event_80_id), sum(value), .(KG_class_2, event_80_id)]$V1)
), 1)

write.csv(event_comparison,  paste0(PATH_OUTPUT_TABLES, region, '_definition_properties.csv'))
