source('source/twc_change.R')
source('source/data_registry.R')
library(pRecipe)

# Helper functions

get_prec_nc_out <- function(fname) {
  short_name <- sub(".*/([^_/]*)_.*", "\\1", fname)
  paste0(PATH_OUTPUT_RAW_PREC, short_name, "_yearly.nc")
}
get_evap_nc_out <- function(fname) {
  short_name <- sub(".*/([^_/]*)_.*", "\\1", fname)
  paste0(PATH_OUTPUT_RAW_EVAP, short_name, "_yearly.nc")
}
get_tabular_out <- function(dataset_name, variable) {
  paste0(PATH_OUTPUT_RAW_TABULAR, gsub(" ", "_", dataset_name), "_", variable, ".Rds")
}

# Datasets - Main

evap_datasets <- filter_datasets(
  dname = EVAP_ALL_NAMES_SHORT,
  var = "evap",
  tstep = "yearly",
  area = "land",
  var2 = "e"
)
evap_datasets_filepath <- evap_datasets[year(end_date) > 2018, file]
evap_datasets_name <- evap_datasets[year(end_date) > 2018, name]

prec_datasets <- filter_datasets(
  dname = PREC_ALL_NAMES_SHORT,
  var = "precip",
  tstep = "yearly",
  area = "land"
)
prec_datasets_filepath <- prec_datasets[, file]
prec_datasets_name <- prec_datasets[, name]

# For Precipitation
foreach(dataset_count = 1:length(prec_datasets_filepath)) %dopar% {
  fname <- prec_datasets_filepath[dataset_count]
  short_name <- sub(".*/([^_/]*)_.*", "\\1", fname)
  nc_out <- paste0(PATH_OUTPUT_RAW_PREC, short_name, "_yearly.nc")
  if (!file.exists(nc_out)) {
    result <- subset_data(fname, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
    saveNC(result, nc_out)
  } else {
    message("Skipping existing output: ", nc_out)
  }
}

for (i in seq_along(prec_datasets_filepath)) {
  dataset_name <- prec_datasets_name[i]
  nc_fname     <- get_prec_nc_out(prec_datasets_filepath[i])
  tab_fname    <- get_tabular_out(dataset_name, "prec")
  if (file.exists(nc_fname) && !file.exists(tab_fname)) {
    cat("Processing:", dataset_name, "\n")
    r <- brick(nc_fname)
    tab <- tabular(r)
    tab[, variable := "prec"]
    tab[, dataset := factor(dataset_name)]
    saveRDS(tab, tab_fname)
  } else {
    message("Skipping (already processed or missing input): ", dataset_name)
  }
}
# For Evaporation
foreach(dataset_count = 1:length(evap_datasets_filepath)) %dopar% {
  fname <- evap_datasets_filepath[dataset_count]
  short_name <- sub(".*/([^_/]*)_.*", "\\1", fname)
  nc_out <- paste0(PATH_OUTPUT_RAW_EVAP, short_name, "_yearly.nc")
  if (!file.exists(nc_out)) {
    result <- subset_data(fname, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
    saveNC(result, nc_out)
  } else {
    message("Skipping existing output: ", nc_out)
  }
}

for (i in seq_along(evap_datasets_filepath)) {
  dataset_name <- evap_datasets_name[i]
  nc_fname     <- get_evap_nc_out(evap_datasets_filepath[i])
  tab_fname    <- get_tabular_out(dataset_name, "evap")
  if (file.exists(nc_fname) && !file.exists(tab_fname)) {
    cat("Processing:", dataset_name, "\n")
    r <- brick(nc_fname)
    tab <- tabular(r)
    tab[, variable := "evap"]
    tab[, dataset := factor(dataset_name)]
    saveRDS(tab, tab_fname)
    # If you want CSV as well: fwrite(tab, sub("\\.Rds$", ".csv", tab_fname))
  } else {
    message("Skipping (already processed or missing input): ", dataset_name)
  }
}

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
runoff_robin_shp <- st_read('~/shared/data/geodata/robin_v1_Jan2025/ROBIN_V1_Shapefiles_Jan2025.shp')
runoff_robin_shp <- st_make_valid(runoff_robin_shp)
runoff_robin_shp <- runoff_robin_shp[st_is_valid(runoff_robin_shp), ]
runoff_robin_meta <- fread('~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv')

csv_files <- list.files("~/shared/data/stations/robin_v1/source/", pattern = "\\.csv$", full.names = TRUE)
runoff_robin <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)

runoff_robin_day <- merge(runoff_robin, runoff_robin_meta[, .(robin_id = ROBIN_ID, area = AREA)])
runoff_robin_day[, flow_mm := (flow_cumecs * SEC_IN_DAY / (area * 10^6)) * 1000][, area := NULL][flow_cumecs := NULL]
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
