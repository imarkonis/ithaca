# Calculate dataset concurrence index for each grid ----
# Calculate uncertainty for each grid ----
# Calculate count of grid directions for each grid ----

source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input data generated in 01_c
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope.rds"))  
evap_trend <- evap_trend[dataset_count >= 12]

## Analysis 
### DCI ----
evap_trend_dci <- evap_trend[,.(DCI_theil_sen = sum(theil_sen_slope/abs(theil_sen_slope)*significant_theil_sen, na.rm = TRUE)/.N),
.(lon,lat)]


### probability groups ----

evap_trend_pos <- evap_trend[trend_direction == "positive significant", .(N_pos_theil_sen = .N), .(lon,lat)]
evap_trend_neg <- evap_trend[trend_direction == "negative significant", .(N_neg_theil_sen = .N), .(lon,lat)]
evap_trend_ins_pos <- evap_trend[trend_direction == "positive", .(N_ins_pos_theil_sen = .N), .(lon,lat)]
evap_trend_ins_neg <- evap_trend[trend_direction == "negative", .(N_ins_neg_theil_sen = .N), .(lon,lat)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_ins_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_ins_neg, by = c("lon", "lat"), all = TRUE)

evap_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_pos_theil_sen), N_ins_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_neg_theil_sen), N_ins_neg_theil_sen:= 0]

evap_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
evap_trend_summary[, threshold_likely := 0.33*count,.(lon,lat)]
evap_trend_summary[, threshold_probable := 1,.(lon,lat)]

evap_trend_summary[, trend:= "no trend",]
evap_trend_summary[N_pos_theil_sen > threshold_likely & N_neg_theil_sen == 0, trend:= "positive likely",]
evap_trend_summary[N_pos_theil_sen >= threshold_probable & N_pos_theil_sen <= threshold_likely & N_neg_theil_sen == 0, trend:= "positive probable",]

evap_trend_summary[N_neg_theil_sen > threshold_likely  & N_pos_theil_sen == 0, trend:= "negative likely",]
evap_trend_summary[N_neg_theil_sen >= threshold_probable & N_neg_theil_sen <= threshold_likely & N_pos_theil_sen == 0, trend:= "negative probable",]
evap_trend_summary[N_neg_theil_sen > 0 & N_pos_theil_sen > 0, trend:= "uncertain",]

grid_cell_area <- unique(evap_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_summary<- grid_cell_area[evap_trend_summary, on = .(lon, lat)]

land_area <- evap_trend_summary[,sum(area)]
uncertain_area <- evap_trend_summary[ trend == "uncertain", sum(area)]
notrend_area <- evap_trend_summary[trend == "no trend", sum(area)]

uncertain_area/land_area
notrend_area/land_area

### Merge data ----
evap_trend_lon_lat <- merge(evap_trend_summary, evap_trend_dci)

## Save data ----
saveRDS(evap_trend_lon_lat, paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))

