# Reads and subsets data as netcdf files for the specified period.

source('source/main.R')
source('source/changing_prec.R')
source('source/geo_functions.R')

library(stringr)

load("~/shared/data_projects/ithaca/misc/prec_fnames_2000_2019_full_record_prec_change.Rdata") # Created by database/06b_dataset_fnames_prec_change.R

registerDoParallel(cores = N_CORES - 1)
n_datasets_2000_2019 <- length(PREC_FNAMES_2000_2019_FULL_RECORD)
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END

foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_time(brick(PREC_FNAMES_2000_2019_FULL_RECORD[[dataset_count]]),
                            period_start, period_end)
  
  cleaned_names <- str_extract(PREC_FNAMES_2000_2019_FULL_RECORD, "(?<=//)[^_]+") #extract the corresponding dataset names
  cleaned_names <- gsub("-v\\d+-\\d+|-v\\d+", "", cleaned_names)
  
  nc_out <- paste0(PATH_SAVE_CHANGING_PREC_RAW,
                   cleaned_names[[dataset_count]],
                   "_tp_mm_land_200001_201912_025_yearly.nc")
  save_nc(result, nc_out)
} 

PREC_FNAMES_2000_2019 <- list.files(path = PATH_SAVE_CHANGING_PREC_RAW, full.names = TRUE)
dummy <- strsplit(PREC_FNAMES_2000_2019, split = '//')
dummy <- sapply(dummy, "[[", 2)
dummy <- strsplit(dummy, split = '_')
PREC_FNAMES_SHORT_2000_2019 <- sapply(dummy, "[[", 1)

save(PREC_FNAMES_2000_2019, PREC_FNAMES_SHORT_2000_2019, file = paste0(PATH_SAVE_CHANGING_PREC, "prec_names_2000_2019.Rdata"))