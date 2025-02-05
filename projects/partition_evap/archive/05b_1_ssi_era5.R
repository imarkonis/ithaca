# SSI based on era5 level 1,2 and 3 ----
source("source/geo_functions.R")
source("source/cdo_functions.R")
source('source/partition_evap.R')

library(pRecipe)

## geospatial 
library(raster)
library(ncdf4)
library(sp)
library(sf)
library(stars)

fnames_nc <- list.files(path = paste0(PATH_DATA, "sim/soilmoisture/raw/"), pattern = "*.nc$", 
                       full.names = T)
## level 3 ----

fname_product <- fnames_nc[grep("era5_swv3", fnames_nc)]

fname <- fname_product[grep("global", fname_product)]

fname

### Mask data ----
cdo_sinfo_fnc(fname)
cmd_cdo <- paste0("cdo vardes ", fname)
system(cmd_cdo)

fname_temp <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv3_frac_land_195901_202112_025_monthly_temp.nc")

output_name <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv3_frac_land_195901_202112_025_monthly.nc")

cmd_mask_ocean <- paste0("cdo -mul /home/rstudio/shared/data_projects/ithaca/misc/water_bodies_mask_global_025.nc ", fname, " ",fname_temp)
system(cmd_mask_ocean)

cmd_cdo <- paste0("cdo vardes ", fname_temp)
system(cmd_cdo)

cdo_change_field_fnc(inputfile_name = fname_temp, outputfile_name = output_name,
                     new_var_name = "swvl3", old_var_name = "lsm",
                     new_long_name = "'Volumetric soil water layer 3'",
                     old_unit = "'(0 - 1)'", new_unit = "'m**3 m**-3'")

### Remove temporary files ----
system(paste0("rm ",fname_temp))

### Estimate Yearly data ----
fname_tmp <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv3_frac_land_195901_202112_025_yearly_temp1.nc")
fname_tmp2 <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv3_frac_land_195901_202112_025_yearly_temp2.nc")
fname_tmp3 <-paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv3_frac_land_195901_202112_025_yearly_temp3.nc")

output_file <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv3_frac_land_195901_202112_025_yearly.nc")

cmd_cdo <- paste0("cdo -b 32 yearmonmean ", output_name," ", fname_tmp)
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname_tmp)
system(cmd_cdo)
cmd_cdo <- paste0("cdo setmon,1 ", fname_tmp," ", fname_tmp2 )
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname_tmp2)
system(cmd_cdo)
cmd_cdo <- paste0("cdo setday,1 ", fname_tmp2, " ", fname_tmp3) 
system(cmd_cdo)
cmd_cdo <- paste0("cdo settime,00:00:00 ", fname_tmp3, " ", output_file)
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", output_file)
system(cmd_cdo)

### Remove temporary files ----
system(paste0("rm ",fname_tmp))
system(paste0("rm ",fname_tmp2))
system(paste0("rm ",fname_tmp3))

## level 2 ----

fname_product <- fnames_nc[grep("era5_swv2", fnames_nc)]

fname <- fname_product[grep("global", fname_product)]

fname
### Mask data ----

cdo_sinfo_fnc(fname)
cmd_cdo <- paste0("cdo vardes ", fname)
system(cmd_cdo)

fname_temp <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv2_frac_land_195901_202112_025_monthly_temp.nc")

output_name <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv2_frac_land_195901_202112_025_monthly.nc")

cmd_mask_ocean <- paste0("cdo -mul /home/rstudio/shared/data_projects/ithaca/misc/water_bodies_mask_global_025.nc ", fname, " ",fname_temp)
system(cmd_mask_ocean)

cmd_cdo <- paste0("cdo vardes ", fname_temp)
system(cmd_cdo)

cdo_change_field_fnc(inputfile_name = fname_temp, outputfile_name = output_name,
                     new_var_name = "swvl2", old_var_name = "lsm",
                     new_long_name = "'Volumetric soil water layer 2'",
                     old_unit = "'(0 - 1)'", new_unit = "'m**3 m**-3'")

### Remove temporary files ----
system(paste0("rm ",fname_temp))

### Estimate Yearly data ----
fname_tmp <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv2_frac_land_195901_202112_025_yearly_temp1.nc")
fname_tmp2 <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv2_frac_land_195901_202112_025_yearly_temp2.nc")
fname_tmp3 <-paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv2_frac_land_195901_202112_025_yearly_temp3.nc")

output_file <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv2_frac_land_195901_202112_025_yearly.nc")

cmd_cdo <- paste0("cdo -b 32 yearmonmean ", output_name," ", fname_tmp)
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname_tmp)
system(cmd_cdo)
cmd_cdo <- paste0("cdo setmon,1 ", fname_tmp," ", fname_tmp2 )
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname_tmp2)
system(cmd_cdo)
cmd_cdo <- paste0("cdo setday,1 ", fname_tmp2, " ", fname_tmp3) 
system(cmd_cdo)
cmd_cdo <- paste0("cdo settime,00:00:00 ", fname_tmp3, " ", output_file)
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", output_file)
system(cmd_cdo)

### Remove temporary files ----
system(paste0("rm ",fname_tmp))
system(paste0("rm ",fname_tmp2))
system(paste0("rm ",fname_tmp3))

## level 1 ----

fname_product <- fnames_nc[grep("era5_swv1", fnames_nc)]

fname <- fname_product[grep("global", fname_product)]

fname

cdo_sinfo_fnc(fname)
cmd_cdo <- paste0("cdo vardes ", fname)
system(cmd_cdo)

### Mask data ----

fname_temp <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv1_frac_land_195901_202112_025_monthly_temp.nc")

output_name <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv1_frac_land_195901_202112_025_monthly.nc")

cmd_mask_ocean <- paste0("cdo -mul /home/rstudio/shared/data_projects/ithaca/misc/water_bodies_mask_global_025.nc ", fname, " ",fname_temp)
system(cmd_mask_ocean)

cmd_cdo <- paste0("cdo vardes ", fname_temp)
system(cmd_cdo)

cdo_change_field_fnc(inputfile_name = fname_temp, outputfile_name = output_name,
                     new_var_name = "swvl1", old_var_name = "lsm",
                     new_long_name = "'Volumetric soil water layer 1'",
                     old_unit = "'(0 - 1)'", new_unit = "'m**3 m**-3'")

### Remove temporary files ----
system(paste0("rm ",fname_temp))

### Estimate Yearly data ----
fname_tmp <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv1_frac_land_195901_202112_025_yearly_temp1.nc")
fname_tmp2 <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv1_frac_land_195901_202112_025_yearly_temp2.nc")
fname_tmp3 <-paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv1_frac_land_195901_202112_025_yearly_temp3.nc")

output_file <- paste0(PATH_DATA, "sim/soilmoisture/raw/era5_swv1_frac_land_195901_202112_025_yearly.nc")

cmd_cdo <- paste0("cdo -b 32 yearmonmean ", output_name," ", fname_tmp)
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname_tmp)
system(cmd_cdo)
cmd_cdo <- paste0("cdo setmon,1 ", fname_tmp," ", fname_tmp2 )
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname_tmp2)
system(cmd_cdo)
cmd_cdo <- paste0("cdo setday,1 ", fname_tmp2, " ", fname_tmp3) 
system(cmd_cdo)
cmd_cdo <- paste0("cdo settime,00:00:00 ", fname_tmp3, " ", output_file)
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", output_file)
system(cmd_cdo)

### Remove temporary files ----
system(paste0("rm ",fname_tmp))
system(paste0("rm ",fname_tmp2))
system(paste0("rm ",fname_tmp3))

## Calculate composite soil moisture from level 1, 2 and 3 ----
fnames_nc <- list.files(path = paste0(PATH_DATA, "sim/soilmoisture/raw/"), pattern = "*.nc$", 
                        full.names = T)
fname_product <- fnames_nc[grep("era5_swv1", fnames_nc)]
fname_level1 <- fname_product[grep("yearly", fname_product)]
fname_product <- fnames_nc[grep("era5_swv2", fnames_nc)]
fname_level2 <- fname_product[grep("yearly", fname_product)]
fname_product <- fnames_nc[grep("era5_swv3", fnames_nc)]
fname_level3 <- fname_product[grep("yearly", fname_product)]

output_name <- paste0(PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_195901_202112_025_yearly.nc")

cmd_cdo <- paste0("cdo merge ", fname_level1, " ", fname_level2, " ", fname_level3, " ", output_name)
system(cmd_cdo)

output_name_sum <- paste0(PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_195901_202112_025_yearly_sum.nc")

cmd_cdo <- paste0(" cdo vardes ", output_name)
system(cmd_cdo)

cmd_cdo <- paste0("cdo expr,'swv_sum=swvl1*0.07+swvl2*0.21+swvl3*0.72;' ",output_name, " ", output_name_sum)
system(cmd_cdo)

cmd_cdo <- paste0(" cdo vardes ", output_name_sum)
system(cmd_cdo)

### Calculate mean and standard deviation ----

data <- brick(output_name_sum)
data@z$Date

output_fname <- paste0(PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly.nc")
### Select time step ----
cdo_seltimestep_fnc(output_name_sum, outputfile_name = paste0(PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly.nc"), start_time = 42, end_time = 61)

# Mean
cdo_timmean_fnc(output_fname, outputfile_name = paste0(PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly_mean.nc"))
# Standard deviation
cdo_timstd_fnc(output_fname, outputfile_name = paste0(PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly_std.nc"))


### Calculate SSI ----
cmd_cdo <- paste0("cdo sub ", PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly.nc ", 
                  PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly_mean.nc ", 
                  PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly-mean.nc")
system(cmd_cdo)

cmd_cdo <- paste0("cdo div ",PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly-mean.nc ", 
                  PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly_std.nc ", 
                  PATH_DATA,"sim/soilmoisture/processed/era5_ssi_land_200001_201912_025_yearly.nc")
system(cmd_cdo)

### Remove temporary files ----
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly_mean.nc"))
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly_std.nc"))
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/processed/era5_swv_frac_land_200001_201912_025_yearly-mean.nc"))

## Save data as data.table ----
data_ssi <- brick(paste0(PATH_DATA, "sim/soilmoisture/processed/era5_ssi_land_200001_201912_025_yearly.nc"))
data_ssi_dt <- brick_to_dt(data_ssi)
saveRDS(data_ssi_dt, paste0(PATH_SAVE_PARTITION_EVAP, "ssi_200001_201912_era5-land.rds"))
