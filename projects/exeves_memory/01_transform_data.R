source("source/exeves_memory.R")

fnames_dat <- list.files(PATH_INPUT_RAW, full.names = TRUE, pattern = ".dat")

all_data <- rbindlist(lapply(fnames_dat, function(file) {
  dt <- fread(file)
  dt[, SITE_ID := tools::file_path_sans_ext(basename(file))]  
  return(dt)
}))

evap_data <- all_data[, .(site_id = SITE_ID, date, latent = LE_CORR, temperature = TA_F, QC)]
evap_data$evap <- evap_data$latent / (2.501 - (2.361 * 0.001) * evap_data$temperature)
evap_data[evap < 0, evap := 0]


#testing
test <- evap_data[site_id == unique(evap_data$site_id)[3]]
plot(test$date, test$latent)

pentads <- copy(test)
pentads[, pentad := ceiling((yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5 )]
pentads[, std_evap := (evap - mean(evap, na.rm = T)) / sd(evap, na.rm = T), by = .(pentad, site_id)]
pentads[, pentad_std_q_high := quantile(std_evap, EXTREMES_THRES, na.rm = TRUE), by = site_id]
pentads[, pentad_std_q_low := quantile(std_evap, LOW_THRES, na.rm = TRUE), by = site_id]
pentads[, evap := NULL]

exeves <- merge(test, pentads[, .(site_id, date, std_evap,  
                                  pentad_std_q_low, pentad_std_q_high)], 
                all.x = TRUE, by = c("site_id", "date"))
exeves[, evap_event := FALSE]
exeves[, value_above_low_thres := FALSE]
exeves[, extreme := FALSE]
exeves[std_evap > 0, value_above_low_thres := TRUE]
exeves[std_evap > pentad_std_q_high, extreme := TRUE]
exeves[, above_low_thres_id := rleid(value_above_low_thres)]
exeves[, extreme_id := rleid(extreme), .(site_id)]

exeves[extreme == TRUE, evap_event := TRUE, .(site_id, above_low_thres_id)] 
above_low_thres_ids_with_extreme <- exeves[extreme == TRUE, above_low_thres_id]
exeves[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves[, event_id := rleid(evap_event), .(site_id)]
exeves[evap_event != TRUE, event_id := NA]
exeves[extreme != TRUE, extreme_id := NA]

to_plot <- exeves[date <= "2006-12-01" & date >= "2006-03-01" ]
ggplot(data = to_plot) +
  geom_line(aes(date, evap)) +
  geom_point(data = to_plot[!is.na(extreme_id)],
             aes(date, evap), col = my_palette[1], size = 4, shape = 0) +
  geom_point(data = to_plot[!is.na(event_id)],
             aes(date, evap), col = my_palette[2], size = 3, alpha = 0.5) +
  geom_point(data = to_plot[!is.na(event_id)], aes(date, 0.6), col = '#a9cce0', size = 2, shape = 15) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))

tidy_data <- melt(
  all_data,
  id.vars = c("TIMESTAMP", "SITE_ID"),  
  variable.name = "variable",           
  value.name = "value"                   
)

setnames(tidy_data, old = c("TIMESTAMP", "SITE_ID", "variable", "value"),
         new= c("timestamp", "site_id", "variable", "value"))
