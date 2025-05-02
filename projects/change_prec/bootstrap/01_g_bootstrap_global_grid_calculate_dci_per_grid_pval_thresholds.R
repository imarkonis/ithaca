# Using p-value thresholds for DCI p-value derived from bootstrap ----
# Calculate dataset concurrence index for each grid ----
# Calculate uncertainty for each grid ----
# Calculate count of grid directions for each grid ----

source('source/changing_prec.R')

## Data ----
### Input data generated in changing_prec/bootstrap/01_c 
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  
prec_trend <- prec_trend[dataset_count >= 10]

## Analysis ----
### p value 0.01 as threshold ----
prec_trend_pos <- prec_trend[p <= 0.01 & slope >= 0, .(N_pos_0_01 = .N), .(lat, lon)]
prec_trend_neg <- prec_trend[p <= 0.01 & slope < 0, .(N_neg_0_01 = .N), .(lat, lon)]
prec_trend_none <- prec_trend[p > 0.01, .(N_none_0_01 = .N), .(lat, lon)]

prec_trend_summary <- merge(prec_trend_pos, prec_trend_neg, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat"), all = TRUE)

### p value as 0.05 as threshold ----
prec_trend_pos <- prec_trend[p <= 0.05 & slope >= 0, .(N_pos_0_05 = .N), .(lat, lon)]
prec_trend_neg <- prec_trend[p <= 0.05 & slope < 0, .(N_neg_0_05 = .N), .(lat, lon)]
prec_trend_none <- prec_trend[p > 0.05, .(N_none_0_05 = .N), .(lat, lon)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat"), all = TRUE)

### p value as 0.1 as threshold ----
prec_trend_pos <- prec_trend[p <= 0.1 & slope >= 0, .(N_pos_0_1 = .N), .(lat, lon)]
prec_trend_neg <- prec_trend[p <= 0.1 & slope < 0, .(N_neg_0_1 = .N), .(lat, lon)]
prec_trend_none <- prec_trend[p > 0.1, .(N_none_0_1 = .N), .(lat, lon)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat"), all = TRUE)

### p value as 0.2 as threshold ----
prec_trend_pos <- prec_trend[p <= 0.2 & slope >= 0, .(N_pos_0_2 = .N), .(lat, lon)]
prec_trend_neg <- prec_trend[p <= 0.2 & slope < 0, .(N_neg_0_2 = .N), .(lat, lon)]
prec_trend_none <- prec_trend[p > 0.2, .(N_none_0_2 = .N), .(lat, lon)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_none, by = c("lon", "lat"), all = TRUE)

### all slopes ----
prec_trend_pos <- prec_trend[slope >= 0, .(N_pos_all = .N), .(lat, lon)]
prec_trend_neg <- prec_trend[slope < 0, .(N_neg_all = .N), .(lat, lon)]

prec_trend_summary <- merge(prec_trend_summary , prec_trend_pos, by = c("lon", "lat"), all = TRUE)
prec_trend_summary <- merge(prec_trend_summary , prec_trend_neg, by = c("lon", "lat"), all = TRUE)


### Fill NA as 0 for count
prec_trend_summary[is.na(prec_trend_summary)] <- 0



### DCI ----
prec_trend_summary[, DCI_0_01 := (N_pos_0_01-N_neg_0_01)/(N_pos_all+N_neg_all)]
prec_trend_summary[, DCI_0_05 := (N_pos_0_05-N_neg_0_05)/(N_pos_all+N_neg_all)]
prec_trend_summary[, DCI_0_1 := (N_pos_0_1-N_neg_0_1)/(N_pos_all+N_neg_all)]
prec_trend_summary[, DCI_0_2 := (N_pos_0_2-N_neg_0_2)/(N_pos_all+N_neg_all)]
prec_trend_summary[, DCI_all := (N_pos_all-N_neg_all)/(N_pos_all+N_neg_all)]


### trend groups
prec_trend_summary[ N_pos_0_01 > 0 , trend_0_01 := "positive"]
prec_trend_summary[ N_neg_0_01 > 0, trend_0_01 := "negative"]
prec_trend_summary[ N_pos_0_01 > 0 & N_neg_0_01 > 0, trend_0_01 := "opposing"]
prec_trend_summary[ N_pos_0_01 == 0 & N_neg_0_01 == 0, trend_0_01 := "none"]

prec_trend_summary[ N_pos_0_05 > 0 , trend_0_05 := "positive"]
prec_trend_summary[ N_neg_0_05 > 0, trend_0_05 := "negative"]
prec_trend_summary[ N_pos_0_05 > 0 & N_neg_0_05 > 0, trend_0_05 := "opposing"]
prec_trend_summary[ N_pos_0_05 == 0 & N_neg_0_05 == 0, trend_0_05 := "none"]

prec_trend_summary[ N_pos_0_1 > 0 , trend_0_1 := "positive"]
prec_trend_summary[ N_neg_0_1 > 0, trend_0_1 := "negative"]
prec_trend_summary[ N_pos_0_1 > 0 & N_neg_0_1 > 0, trend_0_1 := "opposing"]
prec_trend_summary[ N_pos_0_1 == 0 & N_neg_0_1 == 0, trend_0_1 := "none"]

prec_trend_summary[ N_pos_0_2 > 0 , trend_0_2 := "positive"]
prec_trend_summary[ N_neg_0_2 > 0, trend_0_2 := "negative"]
prec_trend_summary[ N_pos_0_2 > 0 & N_neg_0_2 > 0, trend_0_2 := "opposing"]
prec_trend_summary[ N_pos_0_2 == 0 & N_neg_0_2 == 0, trend_0_2 := "none"]


prec_trend_summary[ N_pos_all > 0 , trend_all := "positive"]
prec_trend_summary[ N_neg_all > 0, trend_all := "negative"]
prec_trend_summary[ N_pos_all > 0 & N_neg_all > 0, trend_all := "opposing"]

prec_trend_summary[trend_all == "opposing", p_val_opposing := ">0.2"]
prec_trend_summary[trend_0_2 == "opposing", p_val_opposing := "<=0.2"]
prec_trend_summary[trend_0_1 == "opposing", p_val_opposing := "<=0.1"]
prec_trend_summary[trend_0_05 == "opposing", p_val_opposing := "<=0.05"]
prec_trend_summary[trend_0_01 == "opposing", p_val_opposing := "<=0.01"]
prec_trend_summary[is.na(p_val_opposing), p_val_opposing := "1"]

prec_trend_summary[, N_pos_max := max(c(N_pos_0_01, N_pos_0_05, N_pos_0_1, N_pos_0_2, N_pos_all)) ,.(lat,lon)]
prec_trend_summary[, N_neg_max := max(c(N_neg_0_01, N_neg_0_05, N_neg_0_1, N_neg_0_2, N_neg_all)) ,.(lat,lon)]
prec_trend_summary[, N_pos_max_rel := N_pos_max/(N_pos_max + N_neg_max)]
prec_trend_summary[, N_neg_max_rel := N_neg_max/(N_pos_max + N_neg_max)]
prec_trend_summary[, DCI_max := max(c(abs(DCI_0_01), abs(DCI_0_05), abs(DCI_0_1), abs(DCI_0_2), abs(DCI_all))) ,.(lat,lon)]
prec_trend_summary[, whichDCI_max := which.max(c(abs(DCI_0_01), abs(DCI_0_05), abs(DCI_0_1), abs(DCI_0_2), abs(DCI_all))) ,.(lat,lon)]
prec_trend_summary[, pDCI_max := c("<=0.01", "<=0.05", "<=0.1", "<=0.2", ">0.2")[whichDCI_max]]


## Save data ----
saveRDS(prec_trend_summary, paste0(PATH_SAVE_CHANGING_PREC, "01_g_global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

