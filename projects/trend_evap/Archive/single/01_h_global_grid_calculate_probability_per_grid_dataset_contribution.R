# Dataset contribution: Increase in uncertain area fraction for each grid per dataset----
source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/evap_trend/01_e
evap_trend_summary <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_uncertainty_dataset_leftout.rds"))
evap_trend_summary[, count_all:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
evap_trend_summary <- evap_trend_summary[ count_all >= 182]
evap_uncertainty <- evap_trend_summary[,.(trend_area = sum(area)),.(trend,
                                                                   dataset_leftout)]
evap_uncertainty[, area := sum(trend_area), .(dataset_leftout)]
evap_uncertainty[, area_fraction := trend_area/area, .(dataset_leftout)]

### Input Data generated in projects/evap_trend/01_d
evap_trend_indices <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))
evap_trend_indices <- evap_trend_indices[count >= 14]
evap_uncertainty_all <- evap_trend_indices[,.(trend_area = sum(area)),.(trend)]
evap_uncertainty_all[, area:= sum(trend_area)]
evap_uncertainty_all[, area_fraction:= trend_area/area]

evap_probability <- merge(evap_uncertainty, evap_uncertainty_all, by = c("trend"), all = T)
evap_probability[, diff_area_fraction := area_fraction.y-area_fraction.x]

saveRDS(evap_probability, paste0(PATH_SAVE_EVAP_TREND, "global_stats_probability_group_contribution_dataset_leftout.rds"))

