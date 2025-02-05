# P-val thresholds ----
# global CSI and BIAS of trends between datasets----
source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/trend_evap/bootstrap/01_c
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap.rds"))  

datasets <- unique(evap_trend$dataset)

thresholds <- c(0.01, 0.05, 0.1, 0.2, 1)
thresholds_chr <- c("0_01", "0_05", "0_1", "0_2", "1")

for(i in 1:5){
  evap_trend[p <= thresholds[i], significant_theil_sen := TRUE]
  evap_trend[p > thresholds[i], significant_theil_sen := FALSE]
  
  evap_trend[significant_theil_sen*slope > 0, trend_direction := "positive significant" ]
  evap_trend[significant_theil_sen*slope < 0, trend_direction := "negative significant"  ]
  evap_trend[significant_theil_sen == FALSE & slope > 0, trend_direction := "positive" ]
  evap_trend[significant_theil_sen == FALSE & slope <= 0, trend_direction := "negative"  ]
  
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
  saveRDS(data_success, paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap", thresholds_chr[i],".rds"))
  
}

p_chr <- c("0_01", "0_05", "0_1", "0_2", "1")
for(i in 1:5){
  dummy <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap", thresholds_chr[i],".rds"))
  
  if(i == 1){
    data <- dummy
  }else{
    data <- merge(data, dummy, by = c("dataset_A", "dataset_B"), all = T, suffixes = c(p_chr[i-1], p_chr[i]))
  }

}

saveRDS(data, paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap_all_p.rds"))
