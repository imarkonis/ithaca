# CSI and BIAS of trends between datasets for different masks ----
source('source/evap_trend.R')
source('source/graphics.R')

## Data ----
### Input Data generated in projects/evap_trend/01_d ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon.rds"))  
### Input Data generated in projects/partition_evap/04 ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

datasets <- unique(evap_trend$dataset)

## Analysis ----
evap_trend_masks <- merge(evap_trend, evap_mask, all.x = T, by = c("lon", "lat"))


### Land cover  ----

#### Dcast ----
evap_trend_dcast <- dcast(evap_trend_masks, formula = lon + lat + land_cover_short_class ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (a = .N), land_cover_short_class]
    colnames(dummy_common)[2] <- "c"
        
    dummy_insig <- evap_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, land_cover_short_class]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (c = .N), land_cover_short_class]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- evap_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
                                    (get(dataset_A) != "negative significant" & get(dataset_B) == "negative significant"), (b = .N), land_cover_short_class]
      colnames(dummy_b)[2] <- "b"
      
    }
    
    dummy <- merge(dummy_common, dummy_b, by = "land_cover_short_class", all = T)
    dummy <- merge(dummy, dummy_a, by = "land_cover_short_class", all = T)
    dummy <- merge(dummy, dummy_insig, by = "land_cover_short_class", all = T)
    dummy[, dataset_A := dataset_A]
    dummy[, dataset_B := dataset_B]
    if(dataset_A == datasets[1] & dataset_B == datasets[1] ){
      data_success <- dummy
    }else{
      data_success <- merge(data_success, dummy, by = c("dataset_A", "dataset_B", "a", "b", "c", "N", "land_cover_short_class"), all = T)
    }
  }
}

data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_EVAP_TREND, "land_cover_CSI_BIAS.rds"))

#### Biome ----

evap_trend_dcast <- dcast(evap_trend_masks, formula = lon + lat + biome_class ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (a = .N), biome_class]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- evap_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, biome_class]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (c = .N), biome_class]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- evap_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
                                    (get(dataset_A) != "negative significant" & get(dataset_B) == "negative significant"), (b = .N), biome_class]
      colnames(dummy_b)[2] <- "b"
      
    }
    
    dummy <- merge(dummy_common, dummy_b, by = "biome_class", all = T)
    dummy <- merge(dummy, dummy_a, by = "biome_class", all = T)
    dummy <- merge(dummy, dummy_insig, by = "biome_class", all = T)
    dummy[, dataset_A := dataset_A]
    dummy[, dataset_B := dataset_B]
    if(dataset_A == datasets[1] & dataset_B == datasets[1] ){
      data_success <- dummy
    }else{
      data_success <- merge(data_success, dummy, by = c("dataset_A", "dataset_B", "a", "b", "c", "N", "biome_class"), all = T)
    }
  }
}

data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]

data_success[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
data_success[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
data_success[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
data_success[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
data_success[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
data_success[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
data_success[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
data_success[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
data_success[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
data_success[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
data_success[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
data_success[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
data_success[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
data_success[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
data_success[, biome_short_class := factor(biome_short_class)]

#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_EVAP_TREND, "biome_CSI_BIAS.rds"))

### Elevation class  ----

#### Dcast ----
evap_trend_dcast <- dcast(evap_trend_masks, formula = lon + lat + elev_class ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (a = .N), elev_class]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- evap_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, elev_class]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (c = .N), elev_class]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- evap_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
                                    (get(dataset_A) != "negative significant" & get(dataset_B) == "negative significant"), (b = .N), elev_class]
      colnames(dummy_b)[2] <- "b"
      
    }
    
    dummy <- merge(dummy_common, dummy_b, by = "elev_class", all = T)
    dummy <- merge(dummy, dummy_a, by = "elev_class", all = T)
    dummy <- merge(dummy, dummy_insig, by = "elev_class", all = T)
    dummy[, dataset_A := dataset_A]
    dummy[, dataset_B := dataset_B]
    if(dataset_A == datasets[1] & dataset_B == datasets[1] ){
      data_success <- dummy
    }else{
      data_success <- merge(data_success, dummy, by = c("dataset_A", "dataset_B", "a", "b", "c", "N", "elev_class"), all = T)
    }
  }
}

data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_EVAP_TREND, "elevation_CSI_BIAS.rds"))

### IPCC reference regions ----
#### Dcast ----
evap_trend_dcast <- dcast(evap_trend_masks, formula = lon + lat + IPCC_ref_region ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (a = .N), IPCC_ref_region]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- evap_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, IPCC_ref_region]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (c = .N), IPCC_ref_region]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- evap_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
                                    (get(dataset_A) != "negative significant" & get(dataset_B) == "negative significant"), (b = .N), IPCC_ref_region]
      colnames(dummy_b)[2] <- "b"
      
    }
    
    dummy <- merge(dummy_common, dummy_b, by = "IPCC_ref_region", all = T)
    dummy <- merge(dummy, dummy_a, by = "IPCC_ref_region", all = T)
    dummy <- merge(dummy, dummy_insig, by = "IPCC_ref_region", all = T)
    dummy[, dataset_A := dataset_A]
    dummy[, dataset_B := dataset_B]
    if(dataset_A == datasets[1] & dataset_B == datasets[1] ){
      data_success <- dummy
    }else{
      data_success <- merge(data_success, dummy, by = c("dataset_A", "dataset_B", "a", "b", "c", "N", "IPCC_ref_region"), all = T)
    }
  }
}

data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_EVAP_TREND, "ipcc_CSI_BIAS.rds"))

### Koeppen-Geiger ----
#### Dcast ----
evap_trend_dcast <- dcast(evap_trend_masks, formula = lon + lat + KG_class_3 ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (a = .N), KG_class_3]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- evap_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, KG_class_3]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- evap_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (c = .N), KG_class_3]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- evap_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
                                    (get(dataset_A) != "negative significant" & get(dataset_B) == "negative significant"), (b = .N), KG_class_3]
      colnames(dummy_b)[2] <- "b"
      
    }
    
    dummy <- merge(dummy_common, dummy_b, by = "KG_class_3", all = T)
    dummy <- merge(dummy, dummy_a, by = "KG_class_3", all = T)
    dummy <- merge(dummy, dummy_insig, by = "KG_class_3", all = T)
    dummy[, dataset_A := dataset_A]
    dummy[, dataset_B := dataset_B]
    if(dataset_A == datasets[1] & dataset_B == datasets[1] ){
      data_success <- dummy
    }else{
      data_success <- merge(data_success, dummy, by = c("dataset_A", "dataset_B", "a", "b", "c", "N", "KG_class_3"), all = T)
    }
  }
}

data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_EVAP_TREND, "KG_3_CSI_BIAS.rds"))


## read data

land_cover <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_CSI_BIAS.rds"))

ggplot(land_cover[CSI < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI))+
  facet_wrap(~land_cover_short_class)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "heatplot_land_cover_CSI.png"), 
       width = 8, height = 8)

ggplot(land_cover)+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS))+
  facet_wrap(~land_cover_short_class)+
  scale_fill_gradient2(midpoint = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "heatplot_land_cover_BIAS.png"), 
       width = 8, height = 8)
