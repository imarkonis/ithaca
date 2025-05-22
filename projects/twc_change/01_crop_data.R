source('source/twc_change.R')
library(pRecipe)

registerDoParallel(N_DATASETS_PREC)

# Datasets - Main

PATH_OUTPUT_RAW_PREC <- paste0(PATH_OUTPUT_RAW, "prec/") 
PATH_OUTPUT_RAW_EVAP <- paste0(PATH_OUTPUT_RAW, "evap/") 
PATH_OUTPUT_RAW_OTHER <- paste0(PATH_OUTPUT_RAW, "other/")
dir.create(PATH_OUTPUT_RAW_PREC)
dir.create(PATH_OUTPUT_RAW_EVAP)
dir.create(PATH_OUTPUT_RAW_OTHER)

foreach(dataset_count = 1:N_DATASETS_PREC) %dopar% {
  result <- subset_data(PREC_FNAMES[dataset_count], yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
  short_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_FNAMES[dataset_count])
  nc_out <- paste0(PATH_OUTPUT_RAW_PREC, short_name,
                   "_tp_mm_land_198001_201912_025_monthly.nc")
  saveNC(result, nc_out)
}

foreach(dataset_count = 1:N_DATASETS_EVAP) %dopar% {
  result <- subset_data(EVAP_FNAMES[dataset_count], yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
  short_name <- sub(".*/([^_/]*)_.*", "\\1", EVAP_FNAMES[dataset_count])
  nc_out <- paste0(PATH_OUTPUT_RAW_EVAP, short_name,
                   "_e_mm_land_198001_201912_025_monthly.nc")
  saveNC(result, nc_out)
}

#Precipitation
prec_fnames <- list.files(PATH_OUTPUT_RAW_PREC, full.names = T)
dataset_variable <- 'prec'

dataset_to_dt <- brick(prec_fnames[1])
dataset_dt <- tabular(dataset_to_dt)
dataset_dt[, variable := dataset_variable]
short_name <- sub(".*/([^_/]*)_.*", "\\1", prec_fnames[1])
dataset_dt[, dataset := factor(short_name)]

for(dataset_count in 2:N_DATASETS_PREC){
  dataset_to_dt <- brick(prec_fnames[dataset_count])
  dummy <- tabular(dataset_to_dt)
  dummy[, variable := dataset_variable]
  short_name <- sub(".*/([^_/]*)_.*", "\\1", prec_fnames[dataset_count])
  dummy[, dataset := factor(short_name)]
  print(prec_fnames[dataset_count])
  dataset_dt <- rbind(dataset_dt, dummy)
}

for(dataset_count in 1:N_DATASETS_PREC){
  dataset_to_dt <- brick(prec_fnames[dataset_count])
  dummy <- tabular(dataset_to_dt)
  dummy[, variable := dataset_variable]
  short_name <- sub(".*/([^_/]*)_.*", "\\1", prec_fnames[dataset_count])
  dummy[, dataset := factor(short_name)]
  print(prec_fnames[dataset_count])
  dataset_dt <- rbind(dataset_dt, dummy)
}

#Evaporation
evap_fnames <- list.files(PATH_OUTPUT_RAW_EVAP, full.names = T)
dataset_variable <- 'evap'

for(dataset_count in 1:N_DATASETS_EVAP){
  dataset_to_dt <- brick(evap_fnames[dataset_count])
  dummy <- tabular(dataset_to_dt)
  dummy[, variable := dataset_variable]
  short_name <- sub(".*/([^_/]*)_.*", "\\1", evap_fnames[dataset_count])
  dummy[, dataset := factor(short_name)]
  print(evap_fnames[dataset_count])
  dataset_dt <- rbind(dataset_dt, dummy)
}

saveRDS(dataset_dt, paste0(PATH_OUTPUT_RAW, 'prec_evap_raw.Rds'))

#Water storage/Soil moisture

dummy <- brick("~/shared/data/obs/other/waterstorage/raw/grace-gfz_ws_mm_global_200204_202112_025_monthly.nc")
dummy_dt <- tabular(dummy)
dummy_annual_dt <- dummy_dt[, .(value = mean(value)), .(lon, lat, year = year(date))] 

saveRDS(dummy_annual_dt[year <= 2019], paste0(PATH_OUTPUT_RAW_OTHER, 'grace_yearly_2019.Rds'))

dummy_annual_dt[vakues]
dummy <- subset_data("~/shared/data/obs/soilmoisture/raw/esa-cci-sm-v07-1_swv_m3m-3_land_197811_202112_025_yearly.nc", yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
dummy_dt <- tabular(dummy)

saveRDS(dummy_dt, paste0(PATH_OUTPUT_RAW_OTHER, 'esa-cci_yearly.Rds'))
