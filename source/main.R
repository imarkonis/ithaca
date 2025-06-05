## --- Packages ---
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(raster)
  library(ncdf4)
  library(sp)
  library(sf)
  library(stars)
})

# --- Parallel Setup ---
N_CORES <- parallel::detectCores() - 2

## --- Base Paths ---
PATH_ROOT        <- normalizePath("~/shared", mustWork = FALSE)
PATH_DATA        <- file.path(PATH_ROOT, "data")
PATH_DATA_REVIEW <- file.path(PATH_ROOT, "data_review")
PATH_PROJECTS    <- file.path(PATH_ROOT, "data_projects", "ithaca")

PATH_PREC_SIM    <- file.path(PATH_DATA, "sim/precip/raw")
PATH_PREC_OBS    <- file.path(PATH_DATA, "obs/precip/raw")
PATH_EVAP_SIM    <- file.path(PATH_DATA, "sim/evap/raw")
PATH_EVAP_OBS    <- file.path(PATH_DATA, "obs/evap/raw")

PATH_PREC_SIM_PROC    <- file.path(PATH_DATA, "sim/precip/processed")
PATH_PREC_OBS_PROC    <- file.path(PATH_DATA, "obs/precip/processed")
PATH_EVAP_SIM_PROC    <- file.path(PATH_DATA, "sim/evap/processed")
PATH_EVAP_OBS_PROC    <- file.path(PATH_DATA, "obs/evap/processed")

# --- Time Constants ---
DAYS_IN_YEAR <- 365.25
SEC_IN_DAY   <- 86400

ITHACA_PERIOD_START <- as.Date("1960-01-01")
ITHACA_PERIOD_END   <- as.Date("2019-12-31")

ITHACA_PERIOD_1_START <- as.Date("1960-01-01")
ITHACA_PERIOD_1_END   <- as.Date("1989-12-31")
ITHACA_PERIOD_2_START <- as.Date("1990-01-01")
ITHACA_PERIOD_2_END   <- as.Date("2019-12-31")

# --- Spatial Constants ---
GLOBAL_AREA <- 1.345883e+14 
PILOT_LAT_MIN <- -5.25
PILOT_LAT_MAX <- 5.00
PILOT_LON_MIN <- 33.75
PILOT_LON_MAX <- 44.25

# --- Unit Conversions ---
M2_TO_KM2 <- 1e-6
MM_TO_M   <- 1e-3
MM_TO_KM  <- 1e-6

# --- Variable Names ---
PREC_NAME <- "prec"
EVAP_NAME <- "evap"
PREC_NAME_SHORT <- "tp"
EVAP_NAME_SHORT <- "e"

VARIABLES <- list(
  prec = list(name = PREC_NAME, short = PREC_NAME_SHORT),
  evap = list(name = EVAP_NAME, short = EVAP_NAME_SHORT)
)

# --- Palettes ---
PALETTES <- list(
  water_cycle_change = c('steelblue3', 'darkgreen', 'darkred', 'darkorange'), #Wetter - Accelerated, Wetter - Deccelerated, Drier - Accelerated, Drier - Deccelerated
  agu                = c('#00324A', '#005294', '#058ECD', '#FFFFFF'),
  subdued_prof       = c("#90AFC5", "#336B87", "#2A3132", "#763626")
)


