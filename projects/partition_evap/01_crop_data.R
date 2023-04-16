# Reading and subsetting data for the specified region and period

source('source/partition_prec.R')
source('source/geo_functions.R')

registerDoParallel(cores = N_CORES - 1)

## Analysis

foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_time(brick(PREC_FNAMES_2000_2019_FULL_RECORD[[dataset_count]]),
                            period_start, period_end)
  nc_out <- paste0(PATH_SAVE_PARTITION_PREC_RAW,
                   PREC_FNAMES_SHORT_2000_2019_FULL_RECORD[[dataset_count]],
                   "_tp_mm_land_200001_201912_025_yearly.nc")
  save_nc(result, nc_out)
} 


