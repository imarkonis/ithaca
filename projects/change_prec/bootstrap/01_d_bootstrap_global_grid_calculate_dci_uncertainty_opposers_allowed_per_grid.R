# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# Calculate dataset concurrence index for each grid ----
# Calculate uncertainty for each grid ----
# Calculate count of grid directions for each grid ----

source('source/changing_prec.R')
source('source/geo_functions.R')

## Data ----
### Input data generated in changing_prec/bootstrap/01_c 
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  
prec_trend <- prec_trend[dataset_count >= 10]

## Analysis 
### Overview ---=
prec_trend[p <= 0.1 & p > 0.05, .N, dataset]
prec_trend[p < 0.05, .N, dataset]
prec_trend[, .N, dataset]
prec_trend[p > 0.1, .N, dataset]

### DCI ----
prec_trend_dci <- prec_trend[,.(DCI_theil_sen = sum(slope/abs(slope)*significant_theil_sen, na.rm = TRUE)/.N),
                             .(lon,lat)]

### probability groups ----
min_consensus <- 0.9

prec_trend_pos <- prec_trend[trend_direction == "positive significant", .(N_pos_theil_sen = .N), .(lon,lat)]
prec_trend_neg <- prec_trend[trend_direction == "negative significant", .(N_neg_theil_sen = .N), .(lon,lat)]
prec_trend_ins_pos <- prec_trend[trend_direction == "positive", .(N_ins_pos_theil_sen = .N), .(lon,lat)]
prec_trend_ins_neg <- prec_trend[trend_direction == "negative", .(N_ins_neg_theil_sen = .N), .(lon,lat)]

prec_trend_summary <- merge(prec_trend_pos, prec_trend_ins_pos, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_ins_neg, by = c("lon", "lat"), all = TRUE)

prec_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
prec_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
prec_trend_summary[is.na(N_ins_pos_theil_sen), N_ins_pos_theil_sen:= 0]
prec_trend_summary[is.na(N_ins_neg_theil_sen), N_ins_neg_theil_sen:= 0]

prec_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
prec_trend_summary[, threshold_likely := 0.33*count,.(lon,lat)]
prec_trend_summary[, threshold_probable := 1,.(lon,lat)]

prec_trend_summary[, trend:= "no trend",]
prec_trend_summary[N_pos_theil_sen > threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive likely",]
prec_trend_summary[N_pos_theil_sen >= threshold_probable & N_pos_theil_sen <= threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive probable",]

prec_trend_summary[N_neg_theil_sen > threshold_likely  & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative likely",]
prec_trend_summary[N_neg_theil_sen >= threshold_probable & N_neg_theil_sen <= threshold_likely & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative probable",]

prec_trend_summary[N_pos_theil_sen > 0 & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]
prec_trend_summary[N_neg_theil_sen > 0 & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]

grid_cell_area <- unique(prec_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
prec_trend_summary<- grid_cell_area[prec_trend_summary, on = .(lon, lat)]

land_area <- prec_trend_summary[,sum(area)]
uncertain_area <- prec_trend_summary[ trend == "uncertain", sum(area)]
notrend_area <- prec_trend_summary[trend == "no trend", sum(area)]
pos_area <- prec_trend_summary[trend %in% c("positive likely", "positive probable"), sum(area)]
neg_area <- prec_trend_summary[trend %in% c("negative likely", "negative probable"), sum(area)]

uncertain_area/land_area
notrend_area/land_area
pos_area/land_area
neg_area/land_area

prec_trend_summary[N_pos_theil_sen > 8, .N]
prec_trend_summary[N_neg_theil_sen > 8, .N]

### Merge data ----
prec_trend_lon_lat <- merge(prec_trend_summary, prec_trend_dci)

## Save data ----
saveRDS(prec_trend_lon_lat, paste0(PATH_SAVE_CHANGING_PREC, "01_d_global_grid_slope_indices_opp_allowed_bootstrap.rds"))

