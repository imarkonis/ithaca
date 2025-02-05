source('source/exeves.R')

masks <- readRDS(paste0(PATH_SAVE, "partition_evap/evap_masks.rds"))
masks[elev_class == '0-100', .(lon, lat), KG_class_2]

set.seed(1979)
random_kg_locations <- masks[masks[
  elev_class == '0-100' & 
    rel_dataset_agreement != 'low' &
    rel_dataset_agreement != 'below average' &
    rel_dataset_agreement != 'average', 
  .I[sample(.N, 1)], by = KG_class_2]$V1]

dataset_brick_evap <- brick(paste0(PATH_EVAP_SIM, 'gleam-v3-7a_e_mm_land_198001_202212_025_daily.nc'))

#result <- extract(dataset_brick_evap, random_kg_locations[, .(lon, lat)], cellnumbers = T)


layer_n <- nlayers(dataset_brick_evap)

no_cores <- detectCores() - 1
if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
registerDoParallel(cores = no_cores)
dummie <- foreach (layer_count = 1:layer_n, .combine = rbind) %dopar% {
  dummie_layer <- dataset_brick_evap[[layer_count]]
  dummie_layer <- extract(dummie_layer, random_kg_locations[, .(lon, lat)])
  dummie_layer
}
colnames(dummie) <- random_kg_locations$KG_class_2
evap_kg_locations <- data.table(dummie)
evap_kg_locations[, date := dataset_brick_evap@z$Date]
