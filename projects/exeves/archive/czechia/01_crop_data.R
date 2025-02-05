source('source/exeves.R')
library(pRecipe)

study_area <- c(CZECHIA_LON_MIN, CZECHIA_LON_MAX, CZECHIA_LAT_MIN, CZECHIA_LAT_MAX)
#study_area <- c(-11, 35, 33, 71)
study_area_name <- 'czechia'

dataset_brick_evap <- brick(paste0(PATH_EVAP_SIM, 'camele_e_mm_land_198001_202212_025_daily.nc'))
dataset_brick_evap_2 <- brick(paste0(PATH_EVAP_SIM, 'gleam_e_mm_land_198001_202212_025_daily.nc'))
dataset_brick_prec <- brick(paste0(PATH_PREC_OBS, 'mswx_tp_mm_land_197901_202309_025_daily.nc'))
dataset_brick_lwrad <- brick('../../shared/data/sim/other/radiation/longrad/raw/mswx_strd_Wm-2_land_197901_202310_025_daily.nc')
dataset_brick_swrad <- brick('../../shared/data/sim/other/radiation/shortrad/raw/mswx_ssrd_Wm-2_land_197901_202310_025_daily.nc')

dataset_brick_prec_3h <- brick('../../shared/data/obs/precip/processed/mswx_tp_mm_Czechia_198001_202212_01_3hourly.nc')
dataset_brick_swrad_3h <- brick('../../shared/data/sim/other/radiation/shortrad/processed/mswx_ssrd_Wm-2_Czechia_198101_202212_01_3hourly.nc')

dataset_brick_evap_study_area <- subset_data(dataset_brick_evap, box = study_area, 
                                             yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
dataset_brick_evap_study_area <- crop_data(dataset_brick_evap_study_area,
                   '../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp') 
saveNC(dataset_brick_evap_study_area, paste0(PATH_OUTPUT_RAW, 'camele_e_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_evap_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_evap_camele.rds'))
rm(dataset_dt); gc()

dataset_brick_evap_study_area <- subset_data(dataset_brick_evap_2, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
dataset_brick_evap_study_area <- crop_data(dataset_brick_evap_study_area,
                                           '../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp') 
saveNC(dataset_brick_evap_study_area, paste0(PATH_OUTPUT_RAW, 'gleam_e_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_evap_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_evap_gleam.rds'))
rm(dataset_dt); gc()

dataset_brick_prec_study_area <- subset_data(dataset_brick_prec, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
dataset_brick_prec_study_area <- crop_data(dataset_brick_prec_study_area,
                                           '../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp') 
saveNC(dataset_brick_prec_study_area, paste0(PATH_OUTPUT_RAW, 'mswx_tp_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_prec_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_prec.rds'))
rm(dataset_dt); gc()

dataset_brick_lwrad_study_area <- subset_data(dataset_brick_lwrad, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
dataset_brick_lwrad_study_area <- crop_data(dataset_brick_lwrad_study_area,
                                           '../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp') 
saveNC(dataset_brick_lwrad_study_area, paste0(PATH_OUTPUT_RAW, 'mswx_strd_Wm-2_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_lwrad_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_lwrad.rds'))
rm(dataset_dt); gc()

dataset_brick_swrad_study_area <- subset_data(dataset_brick_swrad, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
dataset_brick_swrad_study_area <- crop_data(dataset_brick_swrad_study_area,
                                            '../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp') 
saveNC(dataset_brick_swrad_study_area, paste0(PATH_OUTPUT_RAW, 'mswx_ssrd_Wm-2_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_swrad_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_swrad.rds'))
rm(dataset_dt); gc()

dataset_brick_swrad_study_area <- subset_data(dataset_brick_swrad_3h, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
dataset_brick_swrad_study_area <- crop_data(dataset_brick_swrad_3h,
                                            '../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp') 
saveNC(dataset_brick_swrad_3h, paste0(PATH_OUTPUT_RAW, 'mswx_ssrd_Wm-2_', study_area_name, '_198001_202212_025_3h.nc'))
dataset_dt <- tabular(dataset_brick_swrad_3h)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_swrad_3h.rds'))
rm(dataset_dt); gc()




#No crop
dataset_dt <- tabular(dataset_brick_evap)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, 'global_evap.rds'))
