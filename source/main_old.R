## packages
# generic
library(data.table)
library(plyr)
library(dplyr)
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
PATH_RUNOFF_SIM <- paste0(PATH_DATA, "sim/runoff/raw/")

## Datasets
PREC_GLOBAL_DATASETS <- c("cpc", "cru-ts", "em-earth", "ghcn", "gpcc", "precl", "udel", 
                          "20cr", "era20c", "era5-land", "jra55", "merra2", "ncep-doe", "ncep-ncar", 
                          "cmap", "gpcp", "gpm-imerg", "mswep",  
                          "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate")


EVAP_GLOBAL_DATASETS <- c("bess", "camele", "era5-land", "etmonitor", "etsynthesis", "fldas", "gldas-clsm", "gldas-noah", 
                          "gldas-vic", "gleam", "jra55","merra2", "mod16a", "terraclimate")

# Types
PREC_DATASETS_OBS <- c("cpc", "cru-ts", "em-earth", "ghcn", "gpcc", "precl", "udel")
PREC_DATASETS_REANAL <- c("20cr", "era20c", "era5-land", "jra55", "merra2", "ncep-doe", "ncep-ncar")
PREC_DATASETS_REMOTE <- c("chirps", "cmap", "cmorph", "gpcp", "gpm-imerg", "mswep", "persiann", "trmm-3b43")
PREC_DATASETS_HYDROL <- c("fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate")

#EVAP_DATASETS_OBS <- c()
EVAP_DATASETS_REANAL <- c("era5-land", "jra55", "merra2")
EVAP_DATASETS_REMOTE <- c("bess", "etmonitor", "gleam","mod16a")
EVAP_DATASETS_HYDROL <- c("fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate")
EVAP_DATASETS_ENSEMB <- c("camele", "etsynthesis", "synthesizedet")
                          
## Constants
# Time
DAYS_IN_YEAR <- 365.25
ITHACA_PERIOD_START <- as.Date("1960-01-01")
ITHACA_PERIOD_END <- as.Date("2019-12-31")

ITHACA_PERIOD_1_START <- as.Date("1960-01-01")
ITHACA_PERIOD_1_END <- as.Date("1989-12-31")
ITHACA_PERIOD_2_START <- as.Date("1990-01-01")
ITHACA_PERIOD_2_END <- as.Date("2019-12-31")

# Space
GLOBAL_AREA <- 1.345883e+14 
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

## Palettes
WATER_CYCLE_CHANGE_PALETTE <- c('steelblue3', 'darkgreen', 'darkred', 'darkorange') #Wetter - Accelerated, Wetter - Deccelerated, Drier - Accelerated, Drier - Deccelerated
AGU_PALETTE <- c('#00324A', '#005294', '#058ECD', '#FFFFFF') 
SUBDUED_PROF_PALETTE = c("#90AFC5", "#336B87", "#2A3132", "#763626")

## Other
M2_TO_KM2 <- 10 ^ (-6)
MM_TO_M <- 10 ^ (-3)
MM_TO_KM <- 10 ^ (-6)
SEC_IN_DAY <- 60 * 60 * 24
