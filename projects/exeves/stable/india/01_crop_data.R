source('source/exeves_ind.R')
library(pRecipe)
library(terra)
library(sf)

# study_area <- c(CZECHIA_LON_MIN, CZECHIA_LON_MAX, CZECHIA_LAT_MIN, CZECHIA_LAT_MAX)
study_area <- c(INDIA_LON_MIN, INDIA_LON_MAX, INDIA_LAT_MIN, INDIA_LAT_MAX)

study_area_name <- 'india'
india_states <- st_read('~/shared/data/geodata/maps/admin/india/india_st.shp')
india_outline <- st_union(india_states)
st_crs(india_outline) <- 4326
st_write(india_outline, '~/shared/data/geodata/maps/admin/india/india_outline.shp', delete_layer = TRUE)

# dataset_brick_evap <- brick(paste0(PATH_EVAP_SIM, 'gleam-v3-7a_e_mm_land_198001_202212_025_daily.nc'))
dataset_brick_evap <- brick('~/shared/data_review/gleam_v4.1a_Akbar/gleam-v4-1a_e_mm_land_198001_202312_daily_025_daily.nc')
dataset_brick_prec <- brick(paste0(PATH_PREC_OBS, 'mswx-past_tp_mm_land_197901_202309_025_daily.nc'))
dataset_brick_lwrad <- brick('~/shared/data/sim/other/radiation/longrad/raw/mswx-past_strd_Wm-2_land_197901_202310_025_daily.nc')
dataset_brick_swrad <- brick('~/shared/data/sim/other/radiation/shortrad/raw/mswx-past_ssrd_Wm-2_land_197901_202310_025_daily.nc')
dataset_brick_temp <- brick("~/shared/data/sim/temperature/raw/mswx-past_t2m_degC_land_197901_202310_025_daily.nc")
# dataset_brick_sensible <- brick('~/shared/data_downloads/gleam_data_AR/gleam-v3-8a_sh_wm-2_land_198001_202212_025_daily.nc')
dataset_brick_sensible <- brick('~/shared/data_review/gleam_v4.1a_Akbar/gleam-v4-1a_h_wm-2_land_198001_202312_025_daily.nc')

# new GLEAM location-- /mnt/shared/data_review/gleam_v4.1a_Akbar

# Datasets - Main

## GLEAM evaporation
dataset_brick_evap_study_area <- subset_data(dataset_brick_evap, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
# dataset_brick_evap_study_area <- crop_data(dataset_brick_evap_study_area,
#                                            '~/shared/data/geodata/maps/admin/india/india_st.shp')
dataset_brick_evap_study_area <- crop_data(dataset_brick_evap_study_area, 
                                           '~/shared/data/geodata/maps/admin/india/india_outline.shp')
saveNC(dataset_brick_evap_study_area, paste0(PATH_OUTPUT_RAW, 'gleam_e_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_evap_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_evap_gleam.rds'))
rm(dataset_dt); gc()

## MSWX precipitation
dataset_brick_prec_study_area <- subset_data(dataset_brick_prec, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
dataset_brick_prec_study_area <- crop_data(dataset_brick_prec_study_area,
                                           '~/shared/data/geodata/maps/admin/india/india_outline.shp') 
saveNC(dataset_brick_prec_study_area, paste0(PATH_OUTPUT_RAW, 'mswx_tp_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_prec_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_prec.rds'))
rm(dataset_dt); gc()

## MSWX longwave radiation
dataset_brick_lwrad_study_area <- subset_data(dataset_brick_lwrad, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
dataset_brick_lwrad_study_area <- crop_data(dataset_brick_lwrad_study_area,
                                           '~/shared/data/geodata/maps/admin/india/india_outline.shp') 
saveNC(dataset_brick_lwrad_study_area, paste0(PATH_OUTPUT_RAW, 'mswx_strd_Wm-2_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_lwrad_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_lwrad.rds'))
rm(dataset_dt); gc()

## MSWX shortwave radiation
dataset_brick_swrad_study_area <- subset_data(dataset_brick_swrad, box = study_area, yrs = c(year(START_PERIOD_1), year(END_PERIOD_2)))
dataset_brick_swrad_study_area <- crop_data(dataset_brick_swrad_study_area,
                                            '~/shared/data/geodata/maps/admin/india/india_outline.shp') 
saveNC(dataset_brick_swrad_study_area, paste0(PATH_OUTPUT_RAW, 'mswx_ssrd_Wm-2_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_swrad_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_swrad.rds'))
rm(dataset_dt); gc()

## MSWX temperature
dataset_brick_temp_study_area <- subset_data(dataset_brick_temp, box = study_area, 
                                             yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
dataset_brick_temp_study_area <- crop_data(dataset_brick_temp_study_area,
                                           '~/shared/data/geodata/maps/admin/india/india_outline.shp') 
saveNC(dataset_brick_temp_study_area, paste0(PATH_OUTPUT_RAW, 'mswx-past_t2m_degC_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_temp_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_temp.rds'))

## GLEAM sensible heat
dataset_brick_sensible_study_area <- subset_data(dataset_brick_sensible, box = study_area, 
                                             yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
dataset_brick_sensible_study_area <- crop_data(dataset_brick_sensible_study_area,
                                           '~/shared/data/geodata/maps/admin/india/india_outline.shp') 
dataset_dt <- tabular(dataset_brick_sensible_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_sensible.rds'))
rm(dataset_dt); gc()


# Datasets - Validation

## CAMELE evaporation
dataset_brick_evap_2 <- brick(paste0(PATH_EVAP_SIM, 'camele_e_mm_land_198001_202212_025_daily.nc'))
dataset_brick_evap_study_area <- subset_data(dataset_brick_evap_2, box = study_area, 
                                             yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
dataset_brick_evap_study_area <- crop_data(dataset_brick_evap_study_area,
                                           '~/shared/data/geodata/maps/admin/india/india_outline.shp') 
saveNC(dataset_brick_evap_study_area, paste0(PATH_OUTPUT_RAW, 'camele_e_mm_', study_area_name, '_198001_202212_025_daily.nc'))
dataset_dt <- tabular(dataset_brick_evap_study_area)
saveRDS(dataset_dt, paste0(PATH_OUTPUT_DATA, study_area_name, '_evap_camele.rds'))
rm(dataset_dt); gc()

