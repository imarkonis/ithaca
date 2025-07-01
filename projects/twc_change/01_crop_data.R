source('source/twc_change.R')
source('source/dataset%registry.R')
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

#Runoff
library(sf)
library(dplyr)
library(data.table)
runoff_robin_shp <- st_read('~/shared/data/geodata/robin_v1_Jan2025/ROBIN_V1_Shapefiles_Jan2025.shp')
runoff_robin_shp <- st_make_valid(runoff_robin_shp)
runoff_robin_shp <- runoff_robin_shp[st_is_valid(runoff_robin_shp), ]
runoff_robin_meta <- fread('~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv')

csv_files <- list.files("~/shared/data/stations/robin_v1/source/", pattern = "\\.csv$", full.names = TRUE)
runoff_robin <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)

runoff_robin_day <- merge(runoff_robin, runoff_robin_meta[, .(robin_id = ROBIN_ID, area = AREA)])
SEC_IN_DAY <- 60*60*24
runoff_robin_day[, flow_mm := (flow_cumecs * SEC_IN_DAY / (area * 10^6)) * 1000][, area := NULL][, flow_cumecs := NULL]
runoff_robin_day[, flow_mm := round(flow_mm, 2)]
setnames(runoff_robin_day, 'flow_mm', 'flow')
dir.create('~/shared/data/stations/robin_v1/raw')
saveRDS(runoff_robin_day, '~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_daily.rds')

runoff_robin_day[, year := as.integer(format(date, "%Y"))]
runoff_robin_day[, month := as.integer(format(date, "%m"))]

runoff_robin_month <- runoff_robin_day[
  , .(flow = sum(flow, na.rm = TRUE)), 
  by = .(robin_id, year, month)
]

runoff_robin_year <- runoff_robin_day[
  , .(flow = sum(flow, na.rm = TRUE)), 
  by = .(robin_id, year)
]

saveRDS(runoff_robin_month, '~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_monthly.rds')
saveRDS(runoff_robin_year, '~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_yearly.rds')

prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
prec_evap_grids <- unique(prec_evap[, .(lon, lat)])
prec_evap_sf <- st_as_sf(prec_evap_grids, coords = c("lon", "lat"), crs = 4326)
prec_evap_in_robin <- st_join(prec_evap_sf, runoff_robin_shp, left = FALSE)

prec_evap_in_robin$lon <- st_coordinates(prec_evap_in_robin)[,1]
prec_evap_in_robin$lat <- st_coordinates(prec_evap_in_robin)[,2]
robin_coords <- as.data.table(prec_evap_in_robin)
robin_coords <- robin_coords[, .(lon, lat, robin_id = ROBIN_ID)]

saveRDS(robin_coords, paste0(PATH_OUTPUT_RAW, 'robin_coords.rds'))
saveRDS(prec_evap_robin, paste0(PATH_OUTPUT_RAW, 'prec_evap_robin.rds'))
