# temperatureitation based index using era5-land ----
source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/cdo_functions.R')


## Load data ----
fnames_nc <- list.files(path = paste0(PATH_DATA, "sim/temperature/raw/"), pattern = "*.nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("era5-land", fnames_nc)]

fname <- fnames_product[grep("yearly", fnames_product)]

fname
## Analysis ----

# Time select
data <- brick(fname)
period_start <- as.Date("2000-01-01") 
period_end <- as.Date("2019-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

output_fname <- paste0(PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly.nc")

cdo_seltimestep_fnc(fname, outputfile_name = paste0(PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly.nc"), start_time = start_time, end_time = end_time)

# Mean
cdo_timmean_fnc(output_fname, outputfile_name = paste0(PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly_mean.nc"))
# Standard deviation
cdo_timstd_fnc(output_fname, outputfile_name = paste0(PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly_std.nc"))


cmd_cdo <- paste0("cdo sub ", PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly.nc ", 
                  PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly_mean.nc ", 
                  PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly-mean.nc")
system(cmd_cdo)
cdo_sinfo_fnc(paste0(PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly-mean.nc"))

cmd_cdo <- paste0("cdo div ",PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly-mean.nc ", 
                  PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly_std.nc ", 
                  PATH_DATA, "sim/temperature/processed/era5-land_sti_land_200001_201912_025_yearly.nc ")
system(cmd_cdo)

## remove temp files ----
system(paste0("rm ",PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly_mean.nc"))
system(paste0("rm ",PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly_std.nc"))
system(paste0("rm ",PATH_DATA, "sim/temperature/processed/era5-land_t2m_K_land_200001_201912_025_yearly-mean.nc"))


## check and save data files ----
cdo_sinfo_fnc(paste0(PATH_DATA, "sim/temperature/processed/era5-land_sti_land_200001_201912_025_yearly.nc"))
cdo_info_fnc(paste0(PATH_DATA, "sim/temperature/processed/era5-land_sti_land_200001_201912_025_yearly.nc"))

data_sti <- brick(paste0(PATH_DATA, "sim/temperature/processed/era5-land_sti_land_200001_201912_025_yearly.nc"))
data_sti_dt <- brick_to_dt(data_sti)
saveRDS(data_sti_dt, paste0(PATH_SAVE_PARTITION_EVAP, "sti_200001_201912_era5-land.rds"))

## plot data ----

ggplot(data_sti_dt[time == as.Date("2004-01-01")])+
  geom_tile(aes(x = x, y = y, fill = value, col = value))+
  theme_bw()
