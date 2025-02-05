# global CSI and BIAS of trends between datasets----
source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/evap_trend/01_c
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon.rds"))  

datasets <- unique(evap_trend$dataset)

### Dcast ----
evap_trend_dcast <- dcast(evap_trend, formula = lon + lat ~ dataset, value.var = "trend_direction")

### Groups ----
# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), .N]
    dummy_a <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), .N]
    
    dummy_b <- evap_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B)== "positive significant") |
                                  (get(dataset_A) != "negative significant" & get(dataset_B) == "negative significant"), .N]
    
    dummy_insig <- evap_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
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
saveRDS(data_success, paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset.rds"))
