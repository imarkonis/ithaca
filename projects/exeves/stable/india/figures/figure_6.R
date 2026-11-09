source("~/ithaca_sp/projects/exeves/stable/india/00_initialize.R")
source('source/exeves_ind.R')

# Function: log-scale and 0–1 normalize ratio column by group
log_scale_ratio <- function(dt, ratio_col = "ratio",
                            by = c("period", "KG_class_2")) {
  stopifnot(is.data.table(dt))
  if (!ratio_col %in% names(dt)) stop("Column not found: ", ratio_col)
  
  dt_copy <- copy(dt)
  
  # Shift to make all positive
  min_val <- min(dt_copy[[ratio_col]], na.rm = TRUE)
  shift_val <- ifelse(min_val <= 0, abs(min_val) + 0.01, 0)
  dt_copy[, paste0(ratio_col, "_pos") := get(ratio_col) + shift_val]
  
  # Log-transform safely
  log_col <- paste0(ratio_col, "_log")
  dt_copy[, (log_col) := log(get(paste0(ratio_col, "_pos")))]
  
  # Group-wise scaling (avoid NaN when all same values)
  dt_copy[, paste0(ratio_col, "_log_scaled") :=
            {
              vals <- get(log_col)
              rng <- max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
              if (rng == 0 | is.na(rng)) rep(0.5, .N)  # handle constant group
              else (vals - min(vals, na.rm = TRUE)) / rng
            },
          by = by]
  
  return(dt_copy)
}


region <- 'india'
# evap_grid <- readRDS(paste0(PATH_OUTPUT_DATA, 'grid_', region, '.rds'))
evap_grid <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap_grid', '.rds'))
# exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_climate_', region, '.rds')) # with climate type
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

# Pre-processing
rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_rad <- merge(exeves, rad, by = c('grid_id', 'date'))
exeves <- merge(exeves_rad,
                prec[, .(grid_id, date, prec = value)], by = c('grid_id', 'date'))

# exeves <- evap_grid[exeves, on = 'grid_id'][, grid_id := NULL]
exeves <- evap_grid[exeves, on = c('grid_id', 'date')][, grid_id := NULL]

# rm(evap_grid); gc()

# Analysis
## Severity: All values
evap_severity_period <- exeves[, .(value = sum(value)), .(lon, lat, period, KG_class_2)] #added climate
evap_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
evap_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
evap_severity_period$variable <- "E: All days"

prec_severity_period <- exeves[, .(value = sum(prec)), .(lon, lat, period, KG_class_2)]
prec_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
prec_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
prec_severity_period$variable <- "P: All days"

swrad_severity_period <- exeves[, .(value = sum(swrad)), .(lon, lat, period, KG_class_2)]
swrad_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
swrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
swrad_severity_period$variable <- "SW: All days"

lwrad_severity_period <- exeves[, .(value = sum(lwrad)), .(lon, lat, period, KG_class_2)]
lwrad_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
lwrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
lwrad_severity_period$variable <- "LW: All days"

event_evap_severity_period <- exeves[!is.na(event_id), .(value = sum(value)), by = .(lon, lat, period, KG_class_2)] #added climate
setorder(event_evap_severity_period, lon, lat, period)

event_evap_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
# event_evap_severity_period[, diff_value := c(NA, diff(value)), by = .(lon, lat, KG_class_2)]
event_evap_severity_period[, ratio := 1 + diff_value / value, by = .(lon, lat, KG_class_2)]
event_evap_severity_period[, variable := "Evaporation (ExEvEs)"]

event_prec_severity_period <- exeves[!is.na(event_id), .(value = sum(prec)), by = .(lon, lat, period, KG_class_2)] #added by = and also elsewhere
setorder(event_prec_severity_period, lon, lat, period)
event_prec_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
event_prec_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
event_prec_severity_period$variable <- "Precipitation (ExEvEs)"

##### Short-wave radiation
event_swrad_severity_period <- exeves[!is.na(event_id), .(value = sum(swrad)), by = .(lon, lat, period, KG_class_2)]
setorder(event_swrad_severity_period, lon, lat, period)
event_swrad_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
event_swrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
event_swrad_severity_period$variable <- "SW Radiation (ExEvEs)"

##### long-wave radiation
event_lwrad_severity_period <- exeves[!is.na(event_id), .(value = sum(lwrad)), by = .(lon, lat, period, KG_class_2)]
setorder(event_lwrad_severity_period, lon, lat, period)
event_lwrad_severity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
event_lwrad_severity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
event_lwrad_severity_period$variable <- "LW Radiation (ExEvEs)"

event_evap_intensity_period <- exeves[!is.na(event_id), .(value = round(mean(value), 2)), by = .(lon, lat, period, KG_class_2)]
setorder(event_evap_intensity_period, lon, lat, period)
event_evap_intensity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
event_evap_intensity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
event_evap_intensity_period$variable <- "Intensity (E)"

event_prec_intensity_period <- exeves[!is.na(event_id), .(value = round(mean(prec), 2)), by = .(lon, lat, period, KG_class_2)]
setorder(event_prec_intensity_period, lon, lat, period)
event_prec_intensity_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
event_prec_intensity_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
event_prec_intensity_period$variable <- "Intensity (P)"


## Frequency
# event_frequency_period <- exeves[!is.na(event_id), .(value = .N), .(lon, lat, period, KG_class_2)]
# event_frequency_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
# event_frequency_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
# event_frequency_period$variable <- factor("ExEvEs Frequency")

event_frequency_period <- exeves[!is.na(event_id), .(value = .N), by = .(lon, lat, period, KG_class_2)]
setorder(event_frequency_period, lon, lat, period)
event_frequency_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
event_frequency_period[, ratio := 1 + diff_value / value, by = .(lon, lat, period, KG_class_2)]
event_frequency_period$variable <- factor("ExEvEs Frequency")


## Duration
event_duration_period <- exeves[!is.na(event_id), .(value = .N), by = .(event_id, lon, lat, period, KG_class_2)]
event_duration_period <- event_duration_period[, .(value = mean(value)), by = .(lon, lat, period, KG_class_2)]
setorder(event_duration_period, lon, lat, period)
event_duration_period[, diff_value := diff(value), by = .(lon, lat, KG_class_2)]
event_duration_period[, ratio := 1 + diff_value/value, by = .(lon, lat, period, KG_class_2)]
event_duration_period$variable <- factor("ExEvEs Duration")


exeve_properties_change <- rbind(event_evap_severity_period, event_prec_severity_period, 
                                 event_swrad_severity_period, event_lwrad_severity_period, 
                                 event_evap_intensity_period, event_prec_intensity_period, event_frequency_period, event_duration_period,
                                 fill=TRUE) #added  fill=TRUE

levels(exeve_properties_change$period)


saveRDS(exeve_properties_change, file = paste0(PATH_OUTPUT, 'spatial_changes_ind_gleam4.rds'))

# Validation
# India outline
# borders <- st_read("~/shared/data/geodata/maps/admin/india/india_outline.shp")
borders <- st_read("~/shared/data/geodata/maps/admin/india/india_st.shp")
st_crs(borders) <- 4326


climate_labels <- c("W" = "Desert", "w" = "Winter dry", "S" = "Steppe", "s" = "Summer dry", "m" = "Monsoonal", "f" = "Fully humid")


#### Evap severity ####
event_evap_severity_period_log <- log_scale_ratio(event_evap_severity_period, "ratio")

summary(event_evap_severity_period_log)


data_severity_up_to_2001 <- event_evap_severity_period_log[period == "up_to_2001"]
ratio_min <- min(data_severity_up_to_2001$ratio_log_scaled, na.rm = TRUE)
ratio_max <- max(data_severity_up_to_2001$ratio_log_scaled, na.rm = TRUE)
ratio_min <- floor(ratio_min * 10) / 10   # round down to nearest 0.1
ratio_max <- ceiling(ratio_max * 10) / 10 # round up to nearest 0.1

labels = function(x) {
  # Reverse scaling (0–1 → log values)
  unscaled_log <- x * (max(data_severity_up_to_2001$ratio_log, na.rm = TRUE) -
                         min(data_severity_up_to_2001$ratio_log, na.rm = TRUE)) +
    min(data_severity_up_to_2001$ratio_log, na.rm = TRUE)
  # Reverse log + shift → original ratio
  shift_val <- ifelse(min(data_severity_up_to_2001$ratio, na.rm = TRUE) <= 0,
                      abs(min(data_severity_up_to_2001$ratio, na.rm = TRUE)) + 0.01, 0)
  round(exp(unscaled_log) - shift_val, 2)
}

### Severity ####
ggplot(data_severity_up_to_2001) +
  geom_tile(aes(lon, lat, fill = ratio_log_scaled)) +
  geom_sf(data = borders, alpha = 0.1, col = '#484848', lwd = 0.2) +
  # scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 1) +
  scale_fill_gradientn(
    name = "Severity ratio",
    colours = c("#2848E1", "#A1D3D3", "#EDA919", "#EE1F15"),
    values = scales::rescale(c(min(data_severity_up_to_2001$ratio_log_scaled, na.rm = TRUE),
                               max(data_severity_up_to_2001$ratio_log_scaled, na.rm = TRUE))),
    # values = c(0, 1),
    # limits = c(0.5, 2.5),  # Adjust this based on your desired color scale range
    # breaks = seq(0.5, 2.5, by = 0.25),
    # labels = seq(0.5, 2.5, by = 0.25),
    labels = labels,
    limits = c(ratio_min, ratio_max),
    breaks = seq(ratio_min, ratio_max, length.out = 6),  # automatic breaks
    # labels = function(x) round(exp(x) - 0.01, 2),  # shift back for legend
    guide = guide_colourbar(nbin = 9,
                            display = "rectangles",  # correct for ggplot2 ≥ 3.5
                            frame.colour = "black",
                            ticks.colour = NA, direction = "vertical", barwidth = 1.2,
                            barheight = 8.5, order = 2))+
  theme_minimal()+
  labs(x = NULL, y = NULL) +
  facet_wrap(~ KG_class_2, ncol=3, labeller = labeller(KG_class_2 = climate_labels))

# ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_up_to_2001_gleam4_ind.pdf"), width = 9, height = 6.5)
ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_up_to_2001_gleam4_climate_seve_ind.pdf"), width = 9, height = 6.5)

data_severity_after_2001 <- event_evap_severity_period_log[period == "after_2001"]
ratio_min2 <- min(data_severity_after_2001$ratio_log_scaled, na.rm = TRUE)
ratio_max2 <- max(data_severity_after_2001$ratio_log_scaled, na.rm = TRUE)
ratio_min2 <- floor(ratio_min2 * 10) / 10
ratio_max2 <- ceiling(ratio_max2 * 10) / 10

labels2 = function(x) {
  # Reverse scaling (0–1 → log values)
  unscaled_log <- x * (max(data_severity_after_2001$ratio_log, na.rm = TRUE) -
                         min(data_severity_after_2001$ratio_log, na.rm = TRUE)) +
    min(data_severity_after_2001$ratio_log, na.rm = TRUE)
  # Reverse log + shift → original ratio
  shift_val <- ifelse(min(data_severity_after_2001$ratio, na.rm = TRUE) <= 0,
                      abs(min(data_severity_after_2001$ratio, na.rm = TRUE)) + 0.01, 0)
  round(exp(unscaled_log) - shift_val, 2)
}

# ggplot(event_evap_severity_period[period == "after_2001"]) +
ggplot(data_severity_after_2001) +
  geom_tile(aes(lon, lat, fill = ratio_log_scaled)) +
  geom_sf(data = borders, alpha = 0.1, col = '#484848', lwd = 0.2) +
  # scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 1) +
  scale_fill_gradientn(
    name = "Severity ratio",
    colours = c("#2848E1", "#A1D3D3", "#EDA919", "#EE1F15"),
    values = scales::rescale(c(
      min(data_severity_after_2001$ratio_log_scaled, na.rm = T),
      max(data_severity_after_2001$ratio_log_scaled, na.rm = T))),
    # limits = c(0.5, 2.5),  # Adjust this based on your desired color scale range
    # breaks = seq(0.5, 2.5, by = 0.25),
    # labels = seq(0.5, 2.5, by = 0.25),
    # labels = seq(ratio_min, ratio_max, length.out = 6),
    limits = c(ratio_min2, ratio_max2),
    breaks = seq(ratio_min2, ratio_max2, length.out = 6),  # automatic breaks
    labels = labels2,  # shift back for legend
    guide = guide_colourbar(nbin = 9,
                            display = "rectangles",  # correct for ggplot2 ≥ 3.5
                            frame.colour = "black",
                            ticks.colour = NA, direction = "vertical", barwidth = 1.2,
                            barheight = 8.5, order = 2))+
  theme_minimal()+
  labs(x = NULL, y = NULL) +
  facet_wrap(~ KG_class_2, ncol=3, labeller = labeller(KG_class_2 = climate_labels))

ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_after_2001_gleam4_climate_seve_ind.pdf"), width = 9, height = 6.5)

### Frequency ####
event_frequency_period_log <- log_scale_ratio(event_frequency_period, "ratio")
data_freq_up_to_2001 <- event_frequency_period_log[period == "up_to_2001"]

ratio_min3 <- min(data_freq_up_to_2001$ratio_log_scaled, na.rm = TRUE)
ratio_max3 <- max(data_freq_up_to_2001$ratio_log_scaled, na.rm = TRUE)
ratio_min3 <- floor(ratio_min3 * 10) / 10
ratio_max3 <- ceiling(ratio_max3 * 10) / 10

labels3 = function(x) {
  # Reverse scaling (0–1 → log values)
  unscaled_log <- x * (max(data_freq_up_to_2001$ratio_log, na.rm = TRUE) -
                         min(data_freq_up_to_2001$ratio_log, na.rm = TRUE)) +
    min(data_freq_up_to_2001$ratio_log, na.rm = TRUE)
  # Reverse log + shift → original ratio
  shift_val <- ifelse(min(data_freq_up_to_2001$ratio, na.rm = TRUE) <= 0,
                      abs(min(data_freq_up_to_2001$ratio, na.rm = TRUE)) + 0.01, 0)
  round(exp(unscaled_log) - shift_val, 2)
}

# ggplot(event_frequency_period[period == "up_to_2001"]) +
ggplot(data_freq_up_to_2001) +
  geom_tile(aes(lon, lat, fill = ratio_log_scaled)) +
  geom_sf(data = borders, alpha = 0.1, col = '#484848', lwd = 0.2) +
  # scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 1) +
  scale_fill_gradientn(
    name = "Frequency",
    colours = c("#2848E1", "#A1D3D3", "#EDA919", "#EE1F15"),
    values = scales::rescale(c(
      min(data_freq_up_to_2001$ratio_log_scaled, na.rm = F),
      max(data_freq_up_to_2001$ratio_log_scaled, na.rm = F))),
    # limits = c(0.5, 2.5),  # Adjust this based on your desired color scale range
    # breaks = seq(0.5, 2.5, by = 0.25),
    # labels = seq(0.5, 2.5, by = 0.25),
    # labels = seq(ratio_min, ratio_max, length.out = 6),
    labels = labels3,
    limits = c(ratio_min3, ratio_max3),
    breaks = seq(ratio_min3, ratio_max3, length.out = 6),  # automatic breaks
    # labels = function(x) round(exp(x) - 0.01, 2),  # shift back for legend
    guide = guide_colourbar(nbin = 9,
                            display = "rectangles",  # correct for ggplot2 ≥ 3.5
                            frame.colour = "black",
                            ticks.colour = NA, direction = "vertical", barwidth = 1.2,
                            barheight = 8.5, order = 2))+
  theme_minimal()+
  labs(x = NULL, y = NULL) +
  facet_wrap(~ KG_class_2, ncol=3, labeller = labeller(KG_class_2 = climate_labels))
# ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_up_to_2001_gleam4_ind.pdf"), width = 9, height = 6.5)
ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_up_to_2001_gleam4_climate_fq_ind.pdf"), width = 9, height = 6.5)


data_freq_after_2001 <- event_frequency_period_log[period == "after_2001"]
ratio_min4 <- min(data_freq_after_2001$ratio_log_scaled, na.rm = TRUE)
ratio_max4 <- max(data_freq_after_2001$ratio_log_scaled, na.rm = TRUE)
ratio_min4 <- floor(ratio_min4 * 10) / 10
ratio_max4 <- ceiling(ratio_max4 * 10) / 10

labels4 = function(x) {
  # Reverse scaling (0–1 → log values)
  unscaled_log <- x * (max(data_freq_after_2001$ratio_log, na.rm = TRUE) -
                         min(data_freq_after_2001$ratio_log, na.rm = TRUE)) +
    min(data_freq_after_2001$ratio_log, na.rm = TRUE)
  # Reverse log + shift → original ratio
  shift_val <- ifelse(min(data_freq_after_2001$ratio, na.rm = TRUE) <= 0,
                      abs(min(data_freq_after_2001$ratio, na.rm = TRUE)) + 0.01, 0)
  round(exp(unscaled_log) - shift_val, 2)
}

# ggplot(event_frequency_period[period == "after_2001"]) +
ggplot(data_freq_after_2001) +
  geom_tile(aes(lon, lat, fill = ratio_log_scaled)) +
  geom_sf(data = borders, alpha = 0.1, col = '#484848', lwd = 0.2) +
  # scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 1) +
  scale_fill_gradientn(name = "Frequency",
                       colours = c("#2848E1", "#A1D3D3", "#EDA919", "#EE1F15"),
                       # values = scales::rescale(c(min(data_freq_after_2001$ratio_log_scaled, na.rm = F),
                       #                            max(data_freq_after_2001$ratio_log_scaled, na.rm = F))),
                       # limits = c(0.5, 2.5),  # Adjust this based on your desired color scale range
                       # breaks = seq(0.5, 2.5, by = 0.25),
                       # labels = seq(0.5, 2.5, by = 0.25),
                       # labels = seq(ratio_min, ratio_max, length.out = 6),
                       labels = labels4,
                       limits = c(ratio_min4, ratio_max4),
                       breaks = seq(ratio_min4, ratio_max4, length.out = 6),  # automatic breaks
                       # labels = function(x) round(exp(x) - 0.01, 2),  # shift back for legend
                       guide = guide_colourbar(nbin = 9,
                                               display = "rectangles",  # correct for ggplot2 ≥ 3.5
                                               frame.colour = "black",
                                               ticks.colour = NA, direction = "vertical", barwidth = 1.2,
                                               barheight = 8.5, order = 2)) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ KG_class_2, ncol=3, labeller = labeller(KG_class_2 = climate_labels))

ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_after_2001_gleam4_climate_fq_ind.pdf"), width = 9, height = 6.5)


### "Evaporation Event Intensity (E)" ###
event_evap_intensity_period_log <- log_scale_ratio(event_evap_intensity_period, "ratio")
data_evap_up_to_2001 <- event_evap_intensity_period_log[period == "up_to_2001"]
ratio_min5 <- min(data_evap_up_to_2001$ratio_log_scaled, na.rm = TRUE)
ratio_max5 <- max(data_evap_up_to_2001$ratio_log_scaled, na.rm = TRUE)
ratio_min5 <- floor(ratio_min5 * 10) / 10
ratio_max5 <- ceiling(ratio_max5 * 10) / 10

labels5 = function(x) {
  # Reverse scaling (0–1 → log values)
  unscaled_log <- x * (max(data_evap_up_to_2001$ratio_log, na.rm = TRUE) -
                         min(data_evap_up_to_2001$ratio_log, na.rm = TRUE)) +
    min(data_evap_up_to_2001$ratio_log, na.rm = TRUE)
  # Reverse log + shift → original ratio
  shift_val <- ifelse(min(data_evap_up_to_2001$ratio, na.rm = TRUE) <= 0,
                      abs(min(data_evap_up_to_2001$ratio, na.rm = TRUE)) + 0.01, 0)
  round(exp(unscaled_log) - shift_val, 2)
}


# ggplot(event_evap_intensity_period[period == "up_to_2001"]) +
ggplot(data_evap_up_to_2001) +
  geom_tile(aes(lon, lat, fill = ratio_log_scaled)) +
  geom_sf(data = borders, alpha = 0.1, col = '#484848', lwd = 0.2) +
  # scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 1) +
  scale_fill_gradientn(name = "Evaporation\n intensity",
                       colours = c("#2848E1", "#A1D3D3", "#EDA919", "#EE1F15"),
                       values = scales::rescale(c(min(data_evap_up_to_2001$ratio_log_scaled, na.rm = T),
                                                  max(data_evap_up_to_2001$ratio_log_scaled, na.rm = T))),
                       # limits = c(0.5, 2.5),  # Adjust this based on your desired color scale range
                       # breaks = seq(0.5, 2.5, by = 0.25),
                       # labels = seq(0.5, 2.5, by = 0.25),
                       # labels = seq(ratio_min, ratio_max, length.out = 6),
                       labels = labels5,
                       limits = c(ratio_min5, ratio_max5),
                       breaks = seq(ratio_min5, ratio_max5, length.out = 6),  # automatic breaks
                       # labels = function(x) round(exp(x) - 0.01, 2),  # shift back for legend
                       guide = guide_colourbar(nbin = 9,
                                               display = "rectangles",  # correct for ggplot2 ≥ 3.5
                                               frame.colour = "black",
                                               ticks.colour = NA, direction = "vertical", barwidth = 1.2,
                                               barheight = 8.5, order = 2)) +
  theme_minimal()+
  labs(x = NULL, y = NULL) +
  facet_wrap(~ KG_class_2, ncol=3, labeller = labeller(KG_class_2 = climate_labels))

# ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_up_to_2001_gleam4_ind.pdf"), width = 9, height = 6.5)
ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_up_to_2001_gleam4_climate_int_ind.pdf"), width = 9, height = 6.5)


data_evap_after_2001 <- event_evap_intensity_period_log[period == "after_2001"]
ratio_min6 <- min(data_evap_after_2001$ratio_log_scaled, na.rm = TRUE)
ratio_max6 <- max(data_evap_after_2001$ratio_log_scaled, na.rm = TRUE)
ratio_min6 <- floor(ratio_min6 * 10) / 10
ratio_max6 <- ceiling(ratio_max6 * 10) / 10

labels6 = function(x) {
  # Reverse scaling (0–1 → log values)
  unscaled_log <- x * (max(data_evap_after_2001$ratio_log, na.rm = TRUE) -
                         min(data_evap_after_2001$ratio_log, na.rm = TRUE)) +
    min(data_evap_after_2001$ratio_log, na.rm = TRUE)
  # Reverse log + shift → original ratio
  shift_val <- ifelse(min(data_evap_after_2001$ratio, na.rm = TRUE) <= 0,
                      abs(min(data_evap_after_2001$ratio, na.rm = TRUE)) + 0.01, 0)
  round(exp(unscaled_log) - shift_val, 2)
}


# ggplot(event_evap_intensity_period[period == "after_2001"]) +
ggplot(data_evap_after_2001) +
  geom_tile(aes(lon, lat, fill = ratio_log_scaled)) +
  geom_sf(data = borders, alpha = 0.1, col = '#484848', lwd = 0.2) +
  # scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 1) +
  scale_fill_gradientn(name = "Evaporation\n intensity",
                       colours = c("#2848E1", "#A1D3D3", "#EDA919", "#EE1F15"),
                       values = scales::rescale(c(min(data_evap_after_2001$ratio_log_scaled, na.rm = T),
                                                  max(data_evap_after_2001$ratio_log_scaled, na.rm = T))),
                       # limits = c(0.5, 2.5),  # Adjust this based on your desired color scale range
                       # breaks = seq(0.5, 2.5, by = 0.25),
                       # labels = seq(0.5, 2.5, by = 0.25),
                       # labels = seq(ratio_min, ratio_max, length.out = 6),
                       labels = labels5,
                       limits = c(ratio_min6, ratio_max6),
                       breaks = seq(ratio_min6, ratio_max6, length.out = 6),  # automatic breaks
                       # labels = function(x) round(exp(x) - 0.01, 2),  # shift back for legend
                       guide = guide_colourbar(nbin = 9,
                                               display = "rectangles",  # correct for ggplot2 ≥ 3.5
                                               frame.colour = "black",
                                               ticks.colour = NA, direction = "vertical", barwidth = 1.2,
                                               barheight = 8.5, order = 2)) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ KG_class_2, ncol=3, labeller = labeller(KG_class_2 = climate_labels))

ggsave(paste0(PATH_OUTPUT_FIGURES, "event_evap_after_2001_gleam4_climate_int_ind.pdf"), width = 9, height = 6.5)

