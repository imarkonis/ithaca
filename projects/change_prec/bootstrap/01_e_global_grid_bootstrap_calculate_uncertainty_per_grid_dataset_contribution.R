# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# Dataset contribution: Change in area fraction for each probability group per grid per dataset----
source('source/changing_prec.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/changing_prec/bootstrap/01_c
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  
prec_trend <- prec_trend[dataset_count >= 10]

### probability groups with dataset left out----
datasets <- unique(prec_trend$dataset)

prec_trend_pos <- prec_trend[dataset != datasets[1] & trend_direction == "positive significant", 
                             .(N_pos_theil_sen = .N,
                               dataset_leftout = datasets[1]), .(lon,lat)]

prec_trend_neg  <- prec_trend[dataset != datasets[1] & trend_direction == "negative significant", 
                              .(N_neg_theil_sen = .N,
                                dataset_leftout = datasets[1]), .(lon,lat)]

prec_trend_ins_pos <- prec_trend[dataset != datasets[1] & trend_direction == "positive", 
                                 .(N_ins_pos_theil_sen = .N,
                                   dataset_leftout = datasets[1]), .(lon,lat)]

prec_trend_ins_neg <- prec_trend[dataset != datasets[1] & trend_direction == "negative", 
                                 .(N_ins_neg_theil_sen = .N,
                                   dataset_leftout = datasets[1]), .(lon,lat)]
#### Repeat for other datasets
for(dataset_leftout in datasets[2:length(datasets)]){
  dummy_pos <- prec_trend[dataset != dataset_leftout & trend_direction == "positive significant", 
                          .(N_pos_theil_sen = .N,
                            dataset_leftout = dataset_leftout), .(lon, lat)]
  prec_trend_pos <- merge(dummy_pos, prec_trend_pos, by = c('lon', 'lat', 'N_pos_theil_sen', 'dataset_leftout'), all = TRUE)
  dummy_neg <- prec_trend[dataset != dataset_leftout & trend_direction == "negative significant", 
                          .(N_neg_theil_sen = .N,
                            dataset_leftout = dataset_leftout), .(lon,lat)]
  prec_trend_neg <- merge(dummy_neg, prec_trend_neg, by = c('lon', 'lat', 'N_neg_theil_sen', 'dataset_leftout'), all = TRUE)
  
  dummy_ins_pos <- prec_trend[dataset != dataset_leftout & trend_direction == "positive", 
                              .(N_ins_pos_theil_sen = .N,
                                dataset_leftout = dataset_leftout), .(lon,lat)]
  prec_trend_ins_pos <- merge(dummy_ins_pos, prec_trend_ins_pos, by = c('lon', 'lat', 'N_ins_pos_theil_sen', 'dataset_leftout'), all = TRUE)
  
  dummy_ins_neg <- prec_trend[dataset != dataset_leftout & trend_direction == "negative", 
                              .(N_ins_neg_theil_sen = .N,
                                dataset_leftout = dataset_leftout), .(lon,lat)]
  prec_trend_ins_neg <- merge(dummy_ins_neg, prec_trend_ins_neg, by = c('lon', 'lat', 'N_ins_neg_theil_sen', 'dataset_leftout'), all = TRUE)
}

prec_trend_summary <- merge(prec_trend_pos, prec_trend_ins_pos, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_ins_neg, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)

prec_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
prec_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
prec_trend_summary[is.na(N_ins_pos_theil_sen), N_ins_pos_theil_sen:= 0]
prec_trend_summary[is.na(N_ins_neg_theil_sen), N_ins_neg_theil_sen:= 0]

prec_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat, dataset_leftout)]
prec_trend_summary[, threshold_likely := 0.33*count,.(lon,lat)]
prec_trend_summary[, threshold_probable := 1,.(lon,lat)]

min_consensus <- 0.9

prec_trend_summary[, trend:= "no trend",]
prec_trend_summary[N_pos_theil_sen > threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive likely",]
prec_trend_summary[N_pos_theil_sen >= threshold_probable & N_pos_theil_sen <= threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive probable",]

prec_trend_summary[N_neg_theil_sen > threshold_likely  & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative likely",]
prec_trend_summary[N_neg_theil_sen >= threshold_probable & N_neg_theil_sen <= threshold_likely & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative probable",]

prec_trend_summary[N_pos_theil_sen > 0 & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]
prec_trend_summary[N_neg_theil_sen > 0 & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]

grid_cell_area <- unique(prec_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
prec_trend_summary <- grid_cell_area[prec_trend_summary, on = .(lon, lat)]

saveRDS(prec_trend_summary, paste0(PATH_SAVE_CHANGING_PREC, "01_e_global_grid_uncertainty_dataset_leftout_bootstrap.rds"))

prec_trend_summary[, count_all:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
prec_trend_summary <- prec_trend_summary[ count_all >= 132] #(12*11 = 132)
prec_uncertainty <- prec_trend_summary[,.(trend_area = sum(area)),.(trend,
                                                                    dataset_leftout)]
prec_uncertainty[, area := sum(trend_area), .(dataset_leftout)]
prec_uncertainty[, area_fraction := trend_area/area, .(dataset_leftout)]

### Input Data generated in projects/changing_prec/bootstrap/01_d
prec_trend_indices <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_d_global_grid_slope_indices_opp_allowed_bootstrap.rds"))
prec_trend_indices <- prec_trend_indices[count >= 12]
prec_uncertainty_all <- prec_trend_indices[,.(trend_area = sum(area)),.(trend)]
prec_uncertainty_all[, area:= sum(trend_area)]
prec_uncertainty_all[, area_fraction:= trend_area/area]

prec_probability <- merge(prec_uncertainty, prec_uncertainty_all, by = c("trend"), all = T)
prec_probability[, diff_area_fraction := area_fraction.y-area_fraction.x]

saveRDS(prec_probability, paste0(PATH_SAVE_CHANGING_PREC, "01_e_global_stats_probability_group_contribution_dataset_leftout_bootstrap.rds"))

