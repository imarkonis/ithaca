# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# CSI and BIAS of trends between datasets for different masks ----
source('source/changing_prec.R')

## Data ----
### Input Data generated in projects/changing_prec/bootstrap/01_c
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_intersection_lat_lon_bootstrap.rds"))  
prec_mask <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))

datasets <- unique(prec_trend$dataset)

## Analysis ----
prec_trend_masks <- merge(prec_trend, prec_mask, all.x = T, by = c("lon", "lat"))


### Land cover  ----

#### Dcast ----
prec_trend_dcast <- dcast(prec_trend_masks, formula = lon + lat + land_cover_short_class ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (c = .N), land_cover_short_class]
    colnames(dummy_common)[2] <- "c"
        
    dummy_insig <- prec_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, land_cover_short_class]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (a = .N), land_cover_short_class]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- prec_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
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

data_success[is.na(c), c := 0]
data_success[is.na(a), a := 0]
data_success[is.na(b), b := 0]
data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_CHANGING_PREC, "02_g_land_cover_CSI_BIAS_bootstrap.rds"))

#### Biome ----

prec_trend_dcast <- dcast(prec_trend_masks, formula = lon + lat + biome_class ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (c = .N), biome_class]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- prec_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, biome_class]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (a = .N), biome_class]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- prec_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
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
data_success[is.na(c), c := 0]
data_success[is.na(a), a := 0]
data_success[is.na(b), b := 0]
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
saveRDS(data_success, paste0(PATH_SAVE_CHANGING_PREC, "02_g_biome_CSI_BIAS_bootstrap.rds"))

### Elevation class  ----

#### Dcast ----
prec_trend_dcast <- dcast(prec_trend_masks, formula = lon + lat + elev_class ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (c = .N), elev_class]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- prec_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, elev_class]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (a = .N), elev_class]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- prec_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
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
data_success[is.na(c), c := 0]
data_success[is.na(a), a := 0]
data_success[is.na(b), b := 0]
data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_CHANGING_PREC, "02_g_elevation_CSI_BIAS_bootstrap.rds"))

### IPCC reference regions ----
#### Dcast ----
prec_trend_dcast <- dcast(prec_trend_masks, formula = lon + lat + IPCC_ref_region ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (c = .N), IPCC_ref_region]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- prec_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, IPCC_ref_region]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (a = .N), IPCC_ref_region]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- prec_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
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
data_success[is.na(c), c := 0]
data_success[is.na(a), a := 0]
data_success[is.na(b), b := 0]
data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_CHANGING_PREC, "02_g_ipcc_CSI_BIAS_bootstrap.rds"))

### Koeppen-Geiger ----
#### Dcast ----
prec_trend_dcast <- dcast(prec_trend_masks, formula = lon + lat + KG_class_3 ~ dataset, value.var = "trend_direction")

# Note, definition causes double counting of opposing significant trends so sum is larger than sum of records

for(dataset_A in datasets){
  for(dataset_B in datasets){
    dummy_common <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) == "positive significant") |
                                  (get(dataset_A) == "negative significant" & get(dataset_B) == "negative significant"), (c = .N), KG_class_3]
    colnames(dummy_common)[2] <- "c"
    
    dummy_insig <- prec_trend_dcast[get(dataset_A) != "positive significant" & get(dataset_B) != "positive significant" &
                                      get(dataset_A) != "negative significant" & get(dataset_B) != "negative significant", .N, KG_class_3]   
    if(dataset_A == dataset_B){
      dummy_a <- copy(dummy_common)
      dummy_b <- copy(dummy_common)
      dummy_a[, a:= 0]
      dummy_b[, b:= 0]
      dummy_a[, c := NULL]
      dummy_b[, c := NULL]
    }else{
      dummy_a <- prec_trend_dcast[(get(dataset_A) == "positive significant" & get(dataset_B) != "positive significant") |
                                    (get(dataset_A) == "negative significant" & get(dataset_B) != "negative significant"), (a = .N), KG_class_3]
      colnames(dummy_a)[2] <- "a"
      
      dummy_b <- prec_trend_dcast[(get(dataset_A) != "positive significant" & get(dataset_B) == "positive significant") |
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
data_success[is.na(c), c := 0]
data_success[is.na(a), a := 0]
data_success[is.na(b), b := 0]
data_success[, all := a+b+c+N]
data_success[, CSI := c/(a+b+c)]
data_success[, BIAS := (c+b)/(c+a)]


#### save data ----
saveRDS(data_success, paste0(PATH_SAVE_CHANGING_PREC, "02_g_KG_3_CSI_BIAS_bootstrap.rds"))


## read data

land_cover <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "02_g_land_cover_CSI_BIAS_bootstrap.rds"))
land_cover[, BIAS_brks := cut(BIAS, breaks = c(1/20,1/5,1/2,1,2,5,20))]
land_cover[, CSI_brks := cut(CSI, breaks = c(-1,1/1000,1/10,2/10,3/10,1))]

ggplot(land_cover[CSI < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_brks))+
  facet_wrap(~land_cover_short_class)+
  scale_fill_manual(values = c("black","darkred", "darkorange", "orange", "yellow"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "02_g_heatplot_land_cover_CSI_bootstrap.png"), 
       width = 8, height = 8)

ggplot(land_cover[CSI < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS_brks))+
  facet_wrap(~land_cover_short_class)+
  scale_fill_manual(values = c("darkblue", "royalblue3", "lightblue", "orange","darkorange","darkred"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "02_g_heatplot_land_cover_BIAS_bootstrap.png"), 
       width = 8, height = 8)
