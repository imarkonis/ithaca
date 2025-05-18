source('source/exeves.R')
library(pRecipe)

study_area_name <- 'wce'
spatial_info <- pRecipe_masks()[ipcc_short_region == "WCE"]
area_coords <- spatial_info[, .(lon, lat)]

MIN_LON <- area_coords[, min(lon)]
MAX_LON <- area_coords[, max(lon)]
MIN_LAT <- area_coords[, min(lat)]
MAX_LAT <- area_coords[, max(lat)]

area_box <- c(MIN_LON, MAX_LON, MIN_LAT, MAX_LAT)

dataset_brick_evap <- brick(paste0(PATH_EVAP_SIM, 'gleam-v3-7a_e_mm_land_198001_202212_025_daily.nc'))
dataset_brick_prec <- brick(paste0(PATH_PREC_SIM, 'mswx-past_tp_mm_land_197901_202309_025_daily.nc'))
dataset_brick_lwrad <- brick('../../shared/data/sim/other/radiation/longrad/raw/mswx-past_strd_Wm-2_land_197901_202310_025_daily.nc')
dataset_brick_swrad <- brick('../../shared/data/sim/other/radiation/shortrad/raw/mswx-past_ssrd_Wm-2_land_197901_202310_025_daily.nc')

dataset_brick_evap_study_area <- subset_data(dataset_brick_evap, box = area_box, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 

saveNC(dataset_brick_evap_study_area, paste0(PATH_OUTPUT_RAW, 'gleam_e_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_evap_study_area)
spatial_info[dataset_dt, on = .(lon, lat)]
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_evap_gleam.rds'))
rm(dataset_dt); gc()

dataset_brick_prec_study_area <- subset_data(dataset_brick_prec, box = area_box, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
saveNC(dataset_brick_prec_study_area, paste0(PATH_OUTPUT_RAW, 'mswx-past_tp_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_prec_study_area)

saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_prec.rds'))
rm(dataset_dt); gc()

dataset_brick_lwrad_study_area <- subset_data(dataset_brick_lwrad, box = area_box, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
saveNC(dataset_brick_lwrad_study_area, paste0(PATH_OUTPUT_RAW, 'mswx-past_strd_Wm-2_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_lwrad_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_lwrad.rds'))
rm(dataset_dt); gc()

dataset_brick_swrad_study_area <- subset_data(dataset_brick_swrad, box = area_box, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
saveNC(dataset_brick_swrad_study_area, paste0(PATH_OUTPUT_RAW, 'mswx-past_ssrd_Wm-2_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_swrad_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_swrad.rds'))
rm(dataset_dt); gc()
