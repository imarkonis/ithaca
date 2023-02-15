## packages
# generic
library(data.table)
library(plyr)
library(lubridate)

# plotting
library(ggplot2)
library(ggpubr)

# geospatial
library(raster)
library(ncdf4)
library(sp)
library(sf)
library(stars)

# parallel
library(doParallel)

## Paths
PATH_DATA <- '~/shared/data/'
PATH_DATA_REVIEW <- '~/shared/data_review/'
PATH_SAVE <- '~/shared/data_projects/ithaca/'
PATH_PREC_SIM <- paste0(PATH_DATA, "sim/precip/raw/")
PATH_PREC_OBS <- paste0(PATH_DATA, "obs/precip/raw/")
PATH_EVAP_SIM <- paste0(PATH_DATA, "sim/evap/raw/")
PATH_EVAP_OBS <- paste0(PATH_DATA, "obs/evap/raw/")

## Datasets

# Types
PREC_DATASETS_OBS <- c("cpc", "cru-ts", "em-earth", "ghcn", "gpcc", "precl", "udel")
PREC_DATASETS_REANAL <- c("20cr", "era20c", "era5", "ncep-doe", "ncep-ncar")
PREC_DATASETS_REMOTE <- c("chirps", "cmap", "cmorph", "gpcp", "gpm-imerg", "mswep", "persiann", "trmm-3b43")
PREC_DATASETS_HYDROL <- c("gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate")

## Constants
# Time
ITHACA_PERIOD_START <- as.Date("1960-01-01")
ITHACA_PERIOD_END <- as.Date("2019-12-31")

ITHACA_PERIOD_1_START <- as.Date("1960-01-01")
ITHACA_PERIOD_1_END <- as.Date("1989-12-31")
ITHACA_PERIOD_2_START <- as.Date("1990-01-01")
ITHACA_PERIOD_2_END <- as.Date("2019-12-31")

# Space
PILOT_LAT_MAX <- 5 
PILOT_LAT_MIN <- -5.25
PILOT_LON_MIN  <- 33.75
PILOT_LON_MAX <- 44.25

## Variable names
PREC_NAME <- "prec"
EVAP_NAME <- "evap"

PREC_NAME_SHORT <- "tp"
EVAP_NAME_SHORT <- "e"

## Parallelization
N_CORES <- detectCores()
registerDoParallel(cores = N_CORES - 1)
