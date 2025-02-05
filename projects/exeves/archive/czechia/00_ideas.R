exeves_prec_events <- unique(exeves_prec[!is.na(event_id), .(evap = sum(evap), prec = sum(prec)), by = .(event_id, grid_id, month(date), period)])
exeves_prec_events[, diff_pe := prec - evap]
exeves_prec_events[, sum_pe := prec + evap]
exeves_prec_events[, type := ordered('wet')]
exeves_prec_events[diff_pe < 0, type := ordered('dry')]



exeves_prec_events[, .N, .(type, period)]

dummy <- exeves_prec_events[, .N, .(type, period, month)]
dummy <- dummy[order(month, period, type)]
dummy <- dcast(dummy, period + month ~ type)
dummy[, dry_wet := dry / wet]

dummy[, mean(dry_wet), month]

exeves_prec_events[, sum(diff_pe), .(period)]
dummy <- exeves_prec_events[, .(pe_diff = sum(diff_pe)), .(period, month)]
dummy <- dummy[order(month, period)]
dummy[, dry_wet := dry / wet]



ggplot(exeves_prec_events) +
  geom_boxplot(aes(x = factor(month), y = diff_pe, fill = period)) +
  theme_minimal() 
