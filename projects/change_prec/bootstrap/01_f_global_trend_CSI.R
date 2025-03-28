# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# global CSI and BIAS of trends between datasets----
source('source/changing_prec.R')

## Data ----
### Input Data generated in projects/trend_prec/bootstrap/01_c
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_intersection_lat_lon_bootstrap.rds"))  

datasets <- unique(prec_trend$dataset)

### Dcast ----
prec_trend_dcast <- dcast(prec_trend, formula = lon + lat ~ dataset, value.var = "trend_direction")

### Groups ----
# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), .N]
    dummy_a <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), .N]
    
    dummy_b <- prec_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B)== "positive significant") |
                                  (get(dataset_A) != "negative significant" & get(dataset_B) == "negative significant"), .N]
    
    dummy_insig <- prec_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N]      
    dummy <- data.table(dataset_A = dataset_A, dataset_B = dataset_B, 
             a = dummy_a, b = dummy_b, c = dummy_common, 
             N_insig = dummy_insig)
    if(dataset_A == datasets[1] & dataset_B == datasets[1] ){
      data_success <- dummy
    }else{
      data_success <- merge(data_success, dummy, by = c("dataset_A", "dataset_B", "a", "b", "c", "N_insig"), all = T)
    }
  }
}

data_success[, all := a+b+c+N_insig]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]

## Save Data ----
saveRDS(data_success, paste0(PATH_SAVE_CHANGING_PREC, "01_f_global_CSI_BIAS_dataset_bootstrap.rds"))
