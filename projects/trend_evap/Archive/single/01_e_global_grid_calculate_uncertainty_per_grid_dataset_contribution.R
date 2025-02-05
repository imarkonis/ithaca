# Dataset contribution: Increase in uncertain area fraction for each grid per dataset----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/evap_trend/01_c
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope.rds"))  
evap_trend <- evap_trend[dataset_count >= 12]

### probability groups with dataset left out----
datasets <- unique(evap_trend$dataset)

evap_trend_pos <- evap_trend[dataset != datasets[1] & trend_direction == "positive significant", 
                        .(N_pos_theil_sen = .N,
                          dataset_leftout = datasets[1]), .(lon,lat)]

evap_trend_neg  <- evap_trend[dataset != datasets[1] & trend_direction == "negative significant", 
                        .(N_neg_theil_sen = .N,
                          dataset_leftout = datasets[1]), .(lon,lat)]

evap_trend_ins_pos <- evap_trend[dataset != datasets[1] & trend_direction == "positive", 
                            .(N_ins_pos_theil_sen = .N,
                              dataset_leftout = datasets[1]), .(lon,lat)]

evap_trend_ins_neg <- evap_trend[dataset != datasets[1] & trend_direction == "negative", 
                                 .(N_ins_neg_theil_sen = .N,
                                   dataset_leftout = datasets[1]), .(lon,lat)]
#### Repeat for other datasets
for(dataset_leftout in datasets[2:length(datasets)]){
  dummy_pos <- evap_trend[dataset != dataset_leftout & trend_direction == "positive significant", 
                               .(N_pos_theil_sen = .N,
                                 dataset_leftout = dataset_leftout), .(lon, lat)]
  evap_trend_pos <- merge(dummy_pos, evap_trend_pos, by = c('lon', 'lat', 'N_pos_theil_sen', 'dataset_leftout'), all = TRUE)
  dummy_neg <- evap_trend[dataset != dataset_leftout & trend_direction == "negative significant", 
                               .(N_neg_theil_sen = .N,
                                 dataset_leftout = dataset_leftout), .(lon,lat)]
  evap_trend_neg <- merge(dummy_neg, evap_trend_neg, by = c('lon', 'lat', 'N_neg_theil_sen', 'dataset_leftout'), all = TRUE)
  
  dummy_ins_pos <- evap_trend[dataset != dataset_leftout & trend_direction == "positive", 
                                   .(N_ins_pos_theil_sen = .N,
                                     dataset_leftout = dataset_leftout), .(lon,lat)]
  evap_trend_ins_pos <- merge(dummy_ins_pos, evap_trend_ins_pos, by = c('lon', 'lat', 'N_ins_pos_theil_sen', 'dataset_leftout'), all = TRUE)
  
  dummy_ins_neg <- evap_trend[dataset != dataset_leftout & trend_direction == "negative", 
                                   .(N_ins_neg_theil_sen = .N,
                                     dataset_leftout = dataset_leftout), .(lon,lat)]
  evap_trend_ins_neg <- merge(dummy_ins_neg, evap_trend_ins_neg, by = c('lon', 'lat', 'N_ins_neg_theil_sen', 'dataset_leftout'), all = TRUE)
}

evap_trend_summary <- merge(evap_trend_pos, evap_trend_ins_pos, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_ins_neg, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)

evap_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_pos_theil_sen), N_ins_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_neg_theil_sen), N_ins_neg_theil_sen:= 0]

evap_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat, dataset_leftout)]
evap_trend_summary[, threshold_likely := 0.33*count,.(lon,lat)]
evap_trend_summary[, threshold_probable := 1,.(lon,lat)]

evap_trend_summary[, trend:= "no trend",]
evap_trend_summary[N_pos_theil_sen > threshold_likely & N_neg_theil_sen == 0, trend:= "positive likely",]
evap_trend_summary[N_pos_theil_sen >= threshold_probable & N_pos_theil_sen <= threshold_likely & N_neg_theil_sen == 0, trend:= "positive probable",]

evap_trend_summary[N_neg_theil_sen > threshold_likely  & N_pos_theil_sen == 0, trend:= "negative likely",]
evap_trend_summary[N_neg_theil_sen >= threshold_probable & N_neg_theil_sen <= threshold_likely & N_pos_theil_sen == 0, trend:= "negative probable",]
evap_trend_summary[N_neg_theil_sen > 0 & N_pos_theil_sen > 0, trend:= "uncertain",]

grid_cell_area <- unique(evap_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_summary <- grid_cell_area[evap_trend_summary, on = .(lon, lat)]

saveRDS(evap_trend_summary, paste0(PATH_SAVE_EVAP_TREND, "global_grid_uncertainty_dataset_leftout.rds"))

evap_trend_summary[, count_all:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
evap_trend_summary <- evap_trend_summary[ count_all >= 182]

land_area <- evap_trend_summary[,.(land_area = sum(area)), .(dataset_leftout)]
uncertain_area <- evap_trend_summary[ trend == "uncertain", .(uncertain_area = sum(area)), dataset_leftout]
notrend_area <- evap_trend_summary[trend == "no trend", .(no_trend_area = sum(area)), dataset_leftout]

uncertain_area <- merge(land_area, uncertain_area, by = ("dataset_leftout"))
uncertain_area[, uncertain_fraction := uncertain_area/land_area]

### Input Data generated in projects/evap_trend/01_d
evap_trend_indices <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))
evap_trend_indices <- evap_trend_indices[count >= 14]
land_area_all <- evap_trend_indices[,sum(area)]
uncertain_area_all <- evap_trend_indices[ trend == "uncertain", sum(area)]
uncertain_area_all/land_area_all

uncertain_area[, all_datasets_uncertain_area_fraction := uncertain_area_all/land_area_all]
uncertain_area[, diff_area_fraction := all_datasets_uncertain_area_fraction-uncertain_fraction]
uncertain_area[, ratio_area_fraction := all_datasets_uncertain_area_fraction/uncertain_fraction]

saveRDS(uncertain_area, paste0(PATH_SAVE_EVAP_TREND, "global_stats_uncertainty_contribution_dataset_leftout.rds"))

## plots ----
uncertain_area <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_stats_uncertainty_contribution_dataset_leftout.rds"))

ggplot(uncertain_area)+
  geom_bar(aes(x = reorder(dataset_leftout, -ratio_area_fraction), y = ((ratio_area_fraction - 1)*100)), 
           stat = "identity")+
  labs(y = "Increase of uncertain area [%]", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
  