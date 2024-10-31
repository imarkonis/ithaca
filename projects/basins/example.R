install.packages("evapoRe")
library(evapoRe)
library(pRecipe)

source('source/crop_catchment.R')
source('source/aggregate_ts_catchment.R')

sample_catchment <- crop_catchment(lon = 15, lat = 50, catchment_level = 8, 
               dataset_fnames = '../../shared/data/obs/precip/raw/cru-ts-v4-07_tp_mm_land_190101_202212_025_monthly.nc')

aa <- aggregate_ts_catchment(sample_catchment)



