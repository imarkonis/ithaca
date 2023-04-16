# Reading and subsetting data for the specified region and period

source('source/europe.R')
source('source/geo_functions.R')

## Load data
prec_mswep <- brick(FNAME_PREC_MSWEP)
prec_gpcp <- brick(FNAME_PREC_GPCP)

## Analysis
mswep_case_study <- crop_space_time(prec_mswep,
                                   period_start, period_end, 
                                   study_area)

gpcp_case_study <- crop_space_time(prec_gpcp,
                                   period_start, period_end, 
                                   study_area)

## Save data
name_mswep_nc <- paste0(PATH_SAVE_EUROPE_RAW, "/mswep_tp_mm_europe_198001_201912_025_monthly.nc")
name_gpcp_nc <- paste0(PATH_SAVE_EUROPE_RAW, "/gpcp_tp_mm_europe_198001_201912_025_monthly.nc")

save_nc(mswep_case_study, name_mswep_nc)
save_nc(gpcp_case_study, name_gpcp_nc)
