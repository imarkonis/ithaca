# rename bess and camele data ----
source('source/partition_evap.R')
source("source/cdo_functions.R")

## BESS  ----


fnames_nc <- list.files(path = PATH_EVAP_SIM, pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("bess", fnames_nc)]

### Monthly ----
fname <- fnames_product[grep("monthly", fnames_product)]

fname
cmd_cdo_variable_name <- paste0("cdo vardes ", fname)
system(cmd_cdo_variable_name)

fname_tmp <- "/home/rstudio/shared/data/sim/evap/raw//bess_e_mm_land_198201_201912_025_monthly_newname.nc"

cdo_change_field_fnc(inputfile_name = fname, outputfile_name = fname_tmp, old_var_name = "mask", new_var_name = "e", 
                     old_unit = " ", new_unit = "mm", new_long_name = "'Total Evapotranspiration'") 


#### Check name: Data product, variables, unit, scale, beginning of time, end of time, spatial resolution, temporal resolution ----

cmd_cdo_variable_name <- paste0("cdo vardes ", fname_tmp)
system(cmd_cdo_variable_name)

cdo_info_fnc(fname_tmp)
cdo_sinfo_fnc(fname_tmp)

cmd_cdo_griddes <- paste0("cdo -griddes ", fname_tmp)
system(cmd_cdo_griddes)

grid_location <- "/mnt/shared/data_projects/ithaca/misc/grid_ithaca"
cmd_cat_grid <- paste0("cat ", grid_location)
system(cmd_cat_grid)

#### Replace old file ----

cmd_system <- paste("mv", fname_tmp, fname)
system(cmd_system)


### Daily ----

fnames_nc <- list.files(path = PATH_EVAP_SIM, pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("bess", fnames_nc)]

fname <- fnames_product[grep("daily", fnames_product)]

fname
cmd_cdo_variable_name <- paste0("cdo vardes ", fname)
system(cmd_cdo_variable_name)

fname_tmp <- "/home/rstudio/shared/data/sim/evap/raw//bess_e_mm_land_198201_201912_025_daily_newname.nc"

cdo_cmd <- paste0("cdo -chname,mask,e -chunit,,mm ", fname," ", fname_tmp)
print(cdo_cmd)
system(cdo_cmd)
nc_cmd <- paste0("ncatted -a long_name,e,o,c,'Total Evapotranspiration' " , fname_tmp)
system(nc_cmd)


####  Check name: Data product, variables, unit, scale, beginning of time, end of time, spatial resolution, temporal resolution ----

cmd_cdo_variable_name <- paste0("cdo vardes ", fname_tmp)
system(cmd_cdo_variable_name)

cdo_info_fnc(fname_tmp)
cdo_sinfo_fnc(fname_tmp)

cmd_cdo_griddes <- paste0("cdo -griddes ", fname_tmp)
system(cmd_cdo_griddes)

grid_location <- "/mnt/shared/data_projects/ithaca/misc/grid_ithaca"
cmd_cat_grid <- paste0("cat ", grid_location)
system(cmd_cat_grid)

#### Replace old file ----

cmd_system <- paste("mv", fname_tmp, fname)
system(cmd_system)


## CAMELE data ----

### Monthly ----

fnames_nc <- list.files(path = PATH_EVAP_SIM, pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("camele", fnames_nc)]

fname <- fnames_product[grep("monthly", fnames_product)]

fname
cmd_cdo_variable_name <- paste0("cdo vardes ", fname)
system(cmd_cdo_variable_name)

fname_tmp <- "/home/rstudio/shared/data/sim/evap/raw//camele_e_mm_land_198201_201912_025_monthly_newname.nc"

cdo_cmd <- paste0("cdo -chname,mask,e -chunit,,mm ", fname," ", fname_tmp)
print(cdo_cmd)
system(cdo_cmd)
nc_cmd <- paste0("ncatted -a long_name,e,o,c,'Total Evapotranspiration' " , fname_tmp)
system(nc_cmd)

####  Check name: Data product, variables, unit, scale, beginning of time, end of time, spatial resolution, temporal resolution ----

cmd_cdo_variable_name <- paste0("cdo vardes ", fname_tmp)
system(cmd_cdo_variable_name)

cdo_info_fnc(fname_tmp)
cdo_sinfo_fnc(fname_tmp)

cmd_cdo_griddes <- paste0("cdo -griddes ", fname_tmp)
system(cmd_cdo_griddes)

cmd_cat_grid <- paste0("cat ", grid_location)
system(cmd_cat_grid)

#### Replace old file ----

cmd_system <- paste("mv", fname_tmp, fname)
system(cmd_system)

### Daily ----
fnames_nc <- list.files(path = PATH_EVAP_SIM, pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("camele", fnames_nc)]

fname <- fnames_product[grep("daily", fnames_product)]

fname
cmd_cdo_variable_name <- paste0("cdo vardes ", fname)
system(cmd_cdo_variable_name)

fname_tmp <- "/home/rstudio/shared/data/sim/evap/raw//camele_e_mm_land_198201_201912_025_daily_newname.nc"

cdo_cmd <- paste0("cdo -chname,mask,e -chunit,,mm ", fname," ", fname_tmp)
print(cdo_cmd)
system(cdo_cmd)
nc_cmd <- paste0("ncatted -a long_name,e,o,c,'Total Evapotranspiration' " , fname_tmp)
system(nc_cmd)

####  Check name: Data product, variables, unit, scale, beginning of time, end of time, spatial resolution, temporal resolution ----

cmd_cdo_variable_name <- paste0("cdo vardes ", fname_tmp)
system(cmd_cdo_variable_name)

cdo_info_fnc(fname_tmp)
cdo_sinfo_fnc(fname_tmp)

cmd_cdo_griddes <- paste0("cdo -griddes ", fname_tmp)
system(cmd_cdo_griddes)

cmd_cat_grid <- paste0("cat ", grid_location)
system(cmd_cat_grid)

#### Replace old file ----

cmd_system <- paste("mv", fname_tmp, fname)
system(cmd_system)

