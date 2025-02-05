# Using p-value thresholds for DCI p-value derived from bootstrap ----
# Calculate dataset concurrence index for each grid ----
# Calculate uncertainty for each grid ----
# Calculate count of grid directions for each grid ----

source('source/evap_trend.R')

## Data ----
### Input data generated in trend_evap/bootstrap/01_c 
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  

## Analysis ----
### p value 0.01 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.01 & slope >= 0, .(N_pos_0_01 = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[p <= 0.01 & slope < 0, .(N_neg_0_01 = .N), .(lat, lon)]
evap_trend_none <- evap_trend[p > 0.01, .(N_none_0_01 = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat"), all = TRUE)

### p value as 0.05 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.05 & slope >= 0, .(N_pos_0_05 = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[p <= 0.05 & slope < 0, .(N_neg_0_05 = .N), .(lat, lon)]
evap_trend_none <- evap_trend[p > 0.05, .(N_none_0_05 = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat"), all = TRUE)

### p value as 0.1 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.1 & slope >= 0, .(N_pos_0_1 = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[p <= 0.1 & slope < 0, .(N_neg_0_1 = .N), .(lat, lon)]
evap_trend_none <- evap_trend[p > 0.1, .(N_none_0_1 = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat"), all = TRUE)

### p value as 0.2 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.2 & slope >= 0, .(N_pos_0_2 = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[p <= 0.2 & slope < 0, .(N_neg_0_2 = .N), .(lat, lon)]
evap_trend_none <- evap_trend[p > 0.2, .(N_none_0_2 = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_none, by = c("lon", "lat"), all = TRUE)

### all slopes ----
evap_trend_pos <- evap_trend[slope >= 0, .(N_pos_all = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[slope < 0, .(N_neg_all = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)


### Fill NA as 0 for count
evap_trend_summary[is.na(evap_trend_summary)] <- 0



### DCI ----
evap_trend_summary[, DCI_0_01 := (N_pos_0_01-N_neg_0_01)/(N_pos_all+N_neg_all)]
evap_trend_summary[, DCI_0_05 := (N_pos_0_05-N_neg_0_05)/(N_pos_all+N_neg_all)]
evap_trend_summary[, DCI_0_1 := (N_pos_0_1-N_neg_0_1)/(N_pos_all+N_neg_all)]
evap_trend_summary[, DCI_0_2 := (N_pos_0_2-N_neg_0_2)/(N_pos_all+N_neg_all)]
evap_trend_summary[, DCI_all := (N_pos_all-N_neg_all)/(N_pos_all+N_neg_all)]


### trend groups
evap_trend_summary[ N_pos_0_01 > 0 , trend_0_01 := "positive"]
evap_trend_summary[ N_neg_0_01 > 0, trend_0_01 := "negative"]
evap_trend_summary[ N_pos_0_01 > 0 & N_neg_0_01 > 0, trend_0_01 := "opposing"]
evap_trend_summary[ N_pos_0_01 == 0 & N_neg_0_01 == 0, trend_0_01 := "none"]

evap_trend_summary[ N_pos_0_05 > 0 , trend_0_05 := "positive"]
evap_trend_summary[ N_neg_0_05 > 0, trend_0_05 := "negative"]
evap_trend_summary[ N_pos_0_05 > 0 & N_neg_0_05 > 0, trend_0_05 := "opposing"]
evap_trend_summary[ N_pos_0_05 == 0 & N_neg_0_05 == 0, trend_0_05 := "none"]

evap_trend_summary[ N_pos_0_1 > 0 , trend_0_1 := "positive"]
evap_trend_summary[ N_neg_0_1 > 0, trend_0_1 := "negative"]
evap_trend_summary[ N_pos_0_1 > 0 & N_neg_0_1 > 0, trend_0_1 := "opposing"]
evap_trend_summary[ N_pos_0_1 == 0 & N_neg_0_1 == 0, trend_0_1 := "none"]

evap_trend_summary[ N_pos_0_2 > 0 , trend_0_2 := "positive"]
evap_trend_summary[ N_neg_0_2 > 0, trend_0_2 := "negative"]
evap_trend_summary[ N_pos_0_2 > 0 & N_neg_0_2 > 0, trend_0_2 := "opposing"]
evap_trend_summary[ N_pos_0_2 == 0 & N_neg_0_2 == 0, trend_0_2 := "none"]


evap_trend_summary[ N_pos_all > 0 , trend_all := "positive"]
evap_trend_summary[ N_neg_all > 0, trend_all := "negative"]
evap_trend_summary[ N_pos_all > 0 & N_neg_all > 0, trend_all := "opposing"]

evap_trend_summary[trend_all == "opposing", p_val_opposing := ">0.2"]
evap_trend_summary[trend_0_2 == "opposing", p_val_opposing := "<=0.2"]
evap_trend_summary[trend_0_1 == "opposing", p_val_opposing := "<=0.1"]
evap_trend_summary[trend_0_05 == "opposing", p_val_opposing := "<=0.05"]
evap_trend_summary[trend_0_01 == "opposing", p_val_opposing := "<=0.01"]
evap_trend_summary[is.na(p_val_opposing), p_val_opposing := "1"]

evap_trend_summary[, N_pos_max := max(c(N_pos_0_01, N_pos_0_05, N_pos_0_1, N_pos_0_2, N_pos_all)) ,.(lat,lon)]
evap_trend_summary[, N_neg_max := max(c(N_neg_0_01, N_neg_0_05, N_neg_0_1, N_neg_0_2, N_neg_all)) ,.(lat,lon)]
evap_trend_summary[, N_pos_max_rel := N_pos_max/(N_pos_max + N_neg_max)]
evap_trend_summary[, N_neg_max_rel := N_neg_max/(N_pos_max + N_neg_max)]
evap_trend_summary[, DCI_max := max(c(abs(DCI_0_01), abs(DCI_0_05), abs(DCI_0_1), abs(DCI_0_2), abs(DCI_all))) ,.(lat,lon)]
evap_trend_summary[, whichDCI_max := which.max(c(abs(DCI_0_01), abs(DCI_0_05), abs(DCI_0_1), abs(DCI_0_2), abs(DCI_all))) ,.(lat,lon)]
evap_trend_summary[, pDCI_max := c("<=0.01", "<=0.05", "<=0.1", "<=0.2", ">0.2")[whichDCI_max]]


## Save data ----
saveRDS(evap_trend_summary, paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

