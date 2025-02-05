source('source/exeves.R')

region <- 'random_locations'

masks <- readRDS(paste0(PATH_SAVE, "partition_evap/evap_masks.rds"))
masks[elev_class == '0-100', .(lon, lat), KG_class_2]

set.seed(1)
random_kg_locations <- masks[masks[
  elev_class == '0-100' & 
    lat > 0 &
    rel_dataset_agreement != 'low' &
    rel_dataset_agreement != 'below average' &
    rel_dataset_agreement != 'average', 
  .I[sample(.N, 1)], by = KG_class_2]$V1]

random_kg_locations <- random_kg_locations[complete.cases(random_kg_locations)]

dataset_brick_evap <- brick(paste0(PATH_EVAP_SIM, 'gleam-v3-7a_e_mm_land_198001_202212_025_daily.nc'))
dataset_brick_prec <- brick('../../shared/data/sim/precip/raw/mswx-past_tp_mm_land_197901_202309_025_daily.nc')
dataset_brick_lwrad <- brick('../../shared/data/sim/other/radiation/longrad/raw/mswx-past_strd_Wm-2_land_197901_202310_025_daily.nc')
dataset_brick_swrad <- brick('../../shared/data/sim/other/radiation/shortrad/raw/mswx-past_ssrd_Wm-2_land_197901_202310_025_daily.nc')
dataset_brick_sensible <- brick('../../shared/data_downloads/gleam_data_AR/gleam-v3-8a_sh_wm-2_land_198001_202212_025_daily.nc')
dataset_brick_temp <- brick('../../shared/data/sim/temperature/raw/mswx-past_t2m_degC_land_197901_202310_025_daily.nc')

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
dataset_dt <- melt(evap_kg_locations, id.vars = 'date')
colnames(dataset_dt)[2] <- 'KG_class_2'       
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, 'evap_', region, '_gleam.rds'))

plot(x = random_kg_locations$lon, y = random_kg_locations$lat)

layer_n <- nlayers(dataset_brick_prec)

no_cores <- detectCores() - 1
if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
registerDoParallel(cores = no_cores)
dummie <- foreach (layer_count = 1:layer_n, .combine = rbind) %dopar% {
  dummie_layer <- dataset_brick_prec[[layer_count]]
  dummie_layer <- extract(dummie_layer, random_kg_locations[, .(lon, lat)])
  dummie_layer
}
colnames(dummie) <- random_kg_locations$KG_class_2
evap_kg_locations <- data.table(dummie)
evap_kg_locations[, date := dataset_brick_prec@z$Date]
dataset_dt <- melt(evap_kg_locations, id.vars = 'date')
colnames(dataset_dt)[2] <- 'KG_class_2'       
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, 'prec_', region, '.rds'))

layer_n <- nlayers(dataset_brick_lwrad)

no_cores <- detectCores() - 1
if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
registerDoParallel(cores = no_cores)
dummie <- foreach (layer_count = 1:layer_n, .combine = rbind) %dopar% {
  dummie_layer <- dataset_brick_lwrad[[layer_count]]
  dummie_layer <- extract(dummie_layer, random_kg_locations[, .(lon, lat)])
  dummie_layer
}
colnames(dummie) <- random_kg_locations$KG_class_2
evap_kg_locations <- data.table(dummie)
evap_kg_locations[, date := dataset_brick_lwrad@z$Date]
dataset_dt <- melt(evap_kg_locations, id.vars = 'date')
colnames(dataset_dt)[2] <- 'KG_class_2'       
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, 'lwrad_', region, '.rds'))

layer_n <- nlayers(dataset_brick_swrad)

no_cores <- detectCores() - 1
if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
registerDoParallel(cores = no_cores)
dummie <- foreach (layer_count = 1:layer_n, .combine = rbind) %dopar% {
  dummie_layer <- dataset_brick_swrad[[layer_count]]
  dummie_layer <- extract(dummie_layer, random_kg_locations[, .(lon, lat)])
  dummie_layer
}
colnames(dummie) <- random_kg_locations$KG_class_2
evap_kg_locations <- data.table(dummie)
evap_kg_locations[, date := dataset_brick_swrad@z$Date]
dataset_dt <- melt(evap_kg_locations, id.vars = 'date')
colnames(dataset_dt)[2] <- 'KG_class_2'       
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, 'swrad_', region, '.rds'))

layer_n <- nlayers(dataset_brick_temp)

no_cores <- detectCores() - 1
if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
registerDoParallel(cores = no_cores)
dummie <- foreach (layer_count = 1:layer_n, .combine = rbind) %dopar% {
  dummie_layer <- dataset_brick_temp[[layer_count]]
  dummie_layer <- extract(dummie_layer, random_kg_locations[, .(lon, lat)])
  dummie_layer
}
colnames(dummie) <- random_kg_locations$KG_class_2
evap_kg_locations <- data.table(dummie)
evap_kg_locations[, date := dataset_brick_temp@z$Date]
dataset_dt <- melt(evap_kg_locations, id.vars = 'date')
colnames(dataset_dt)[2] <- 'KG_class_2'       
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, 'temp_', region, '.rds'))

layer_n <- nlayers(dataset_brick_sensible)

no_cores <- detectCores() - 1
if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
registerDoParallel(cores = no_cores)
dummie <- foreach (layer_count = 1:layer_n, .combine = rbind) %dopar% {
  dummie_layer <- dataset_brick_sensible[[layer_count]]
  dummie_layer <- extract(dummie_layer, random_kg_locations[, .(lon, lat)])
  dummie_layer
}
colnames(dummie) <- random_kg_locations$KG_class_2
evap_kg_locations <- data.table(dummie)
evap_kg_locations[, date := dataset_brick_sensible@z$Date]
dataset_dt <- melt(evap_kg_locations, id.vars = 'date')
colnames(dataset_dt)[2] <- 'KG_class_2'       
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, 'sensible_', region, '.rds'))