# Review gleam data ----
source('source/partition_evap.R')
source("source/cdo_functions.R")
source("source/geo_functions.R")

library(pRecipe)

## geospatial 
library(raster)
library(ncdf4)
library(sp)
library(sf)
library(stars)

## stats
library("openair")

## Load data ----
fnames_nc <- list.files(path = "~/shared/data_review/gleam_v4.1a_Akbar", pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("gleam", fnames_nc)]

fnames_time <- fnames_product[grep("daily", fnames_product)]

fname <- fnames_time[grep("smrz", fnames_time)]

## 1. Check name: Data product, variables, unit, scale, beginning of time, end of time, spatial resolution, temporal resolution ----
fname

## 2. Check unit conversion/scaling (nc history)----
# multipliers "-mulc"
# muldpm
cmd_ncdump <- paste0("ncdump -h ", fname)
system(cmd_ncdump)

# variable name
cmd_cdo_variable_name <- paste0("cdo vardes ", fname)
system(cmd_cdo_variable_name)

# summary
cdo_info_fnc(fname)

## 3. plot map to see if it looks okay ----
data <- pRecipe::tabular(fname) 
pRecipe::plot_map(data[[12]])

## 4. Check projection ----
# projection = lonlat
# lon: from east to west and -180 to 180 (1440)
# lat:from north to south and 90 to -90 (720)
crs(data)
extent(data)

# OG grid location for comparison
cmd_cdo_griddes <- paste0("cdo -griddes ", fname)
system(cmd_cdo_griddes)

grid_location <- "/mnt/shared/data_projects/ithaca/misc/grid_ithaca"
cmd_cat_grid <- paste0("cat ", grid_location)
system(cmd_cat_grid)

## 5. Check time units ----
# daily: hours should always be 00:00:00
# monthly: day should always be 01
# yearly: month and day should be 01-01
cdo_sinfo_fnc(fname)
data@z$Date

## 6. Check data type: Data type F32z ----
# scroll to top of output
cdo_sinfo_fnc(fname)

## 7. Compare values to at least one other publication ----
cdo_timmean_fnc(inputfile_name = fname, outputfile_name = "~/Review/gleam_review_time_mean.nc")
cdo_info_fnc(inputfile_name = "~/Review/gleam_review_time_mean.nc")


period_start <- as.Date("1982-01-01") 
period_end <- as.Date("2020-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/gleam_review_sel_i.nc", start_time = start_time, end_time = end_time)
cdo_fldmean_fnc(inputfile_name = "~/Review/gleam_review_sel_i.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean.nc")

cmd_cdo_trend <- paste0("cdo trend ~/Review/gleam_review_sel_fldmean.nc ~/Review/gleam_review_sel_fldmean_trend_a.nc ~/Review/gleam_review_sel_fldmean_trend_b.nc")
system(cmd_cdo_trend)
cdo_info_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean_trend_b.nc")

fld_means <- cdo_info_to_text_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc", outputfile_name = "~/Review/gleam_1982_2020.txt")
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, date := paste0(year, "-01-01 00:00:00")]
fld_means[, date := as.POSIXct(date)]
slope <- TheilSen(fld_means, pollutant = "value", autocor = FALSE, plot = F, silent = T)
slope
lm(value ~ year, data = fld_means)
mean_fld <- mean(fld_means$value, na.rm = T)
plot(fld_means$date, fld_means$value-mean_fld, type = "b", ylim = c(-30, 30))


# Comparison to Liu et al. 2023, JoH, 617 (2023) 128887
period_start <- as.Date("2003-01-01") 
period_end <- as.Date("2013-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/gleam_review_sel.nc", start_time = start_time, end_time = end_time)
cdo_fldmean_fnc(inputfile_name = "~/Review/gleam_review_sel.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean.nc")
cdo_timmean_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean_timmean.nc")
cdo_info_fnc("~/Review/gleam_review_sel_fldmean_timmean.nc")

# Comparison to Pan et al. 1. magnitude
period_start <- as.Date("2001-01-01") 
period_end <- as.Date("2011-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/gleam_review_sel.nc", start_time = start_time, end_time = end_time)
cdo_fldmean_fnc(inputfile_name = "~/Review/gleam_review_sel.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean.nc")
cdo_timmean_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean_timmean.nc")
cdo_info_fnc("~/Review/gleam_review_sel_fldmean_timmean.nc")

# trend
period_start <- as.Date("1982-01-01") 
period_end <- as.Date("2011-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/gleam_review_sel.nc", start_time = start_time, end_time = end_time)

cdo_fldmean_fnc(inputfile_name = "~/Review/gleam_review_sel.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean.nc")
cdo_info_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc")
fld_means <- cdo_info_to_text_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc", outputfile_name = "~/Review/gleam_1982_2019.txt")
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, date := paste0(year, "-01-01 00:00:00")]
fld_means[, date := as.POSIXct(date)]
slope <- TheilSen(fld_means, pollutant = "value", autocor = FALSE, plot = F, silent = T)
slope
lm(value ~ year, data = fld_means)


# trend Ma et al. 2003 to 2019
period_start <- as.Date("2003-01-01") 
period_end <- as.Date("2019-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/gleam_review_sel.nc", start_time = start_time, end_time = end_time)

cdo_fldmean_fnc(inputfile_name = "~/Review/gleam_review_sel.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean.nc")
cdo_info_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc")
fld_means <- cdo_info_to_text_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc", outputfile_name = "~/Review/gleam_1982_2019.txt")
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, date := paste0(year, "-01-01 00:00:00")]
fld_means[, date := as.POSIXct(date)]
slope <- TheilSen(fld_means, pollutant = "value", autocor = FALSE, plot = F, silent = T)
slope
summary(lm(value ~ year, data = fld_means))

plot(fld_means$value, type = "b", ylim = c(450, 700))
