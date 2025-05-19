## packages
# generic
library(data.table)

# plotting
library(ggplot2)
library(ggpubr)
library(rnaturalearth)
library(scales)
library(sf)
library(stars)

# geospatial
library(pRecipe)
library(raster)

# parallel
library(doParallel)

## Paths
PATH_DATA <- "~/shared/data/"
PATH_DATA_REVIEW <- "~/shared/data_review/"
PATH_SAVE <- "~/shared/data_projects/ithaca/"
PATH_PREC_SIM <- paste0(PATH_DATA, "sim/precip/raw/")
PATH_PREC_OBS <- paste0(PATH_DATA, "obs/precip/raw/")

## Datasets
PREC_GLOBAL_DATASETS <- c("20cr", "chirps", "cmap", "cmorph", "cpc",
                          "cru-ts", "era20c", "era5", "era5-land", "em-earth",
                          "fldas", "ghcn", "gldas-clsm", "gldas-noah",
                          "gldas-vic", "gpcc", "gpcp", "gpm-imerg", "gsmap",
                          "jra55", "merra2", "mswep", "ncep-doe", "ncep-ncar",
                          "persiann", "precl", "terraclimate", "trmm-3b43",
                          "udel")

# Types
PREC_DATASETS_OBS <- c("cpc", "cru-ts", "em-earth", "ghcn", "gpcc", "precl",
                       "udel")
PREC_DATASETS_REANAL <- c("20cr", "era20c", "era5", "era5-land", "jra55",
                          "merra2", "ncep-doe", "ncep-ncar")
PREC_DATASETS_REMOTE <- c("chirps", "cmap", "cmorph","gpcp", "gpm-imerg",
                          "gsmap", "mswep", "persiann", "trmm-3b43")
PREC_DATASETS_HYDROL <- c("fldas", "gldas-clsm", "gldas-noah",
                          "gldas-vic", "terraclimate")

### Output
PATH_SAVE_UNCERTAINTY_PREC <- paste0(PATH_SAVE, "uncertainty_prec/")
PATH_SAVE_UNCERTAINTY_PREC_RAW <- paste0(PATH_SAVE, "uncertainty_prec/raw/")
PATH_SAVE_UNCERTAINTY_PREC_SPATIAL <- paste0(PATH_SAVE,
                                             "uncertainty_prec/spatial/")
PATH_SAVE_UNCERTAINTY_PREC_FIGURES <- paste0(PATH_SAVE,
                                             "uncertainty_prec/figures/")
PATH_SAVE_UNCERTAINTY_PREC_TABLES <- paste0(PATH_SAVE,
                                            "uncertainty_prec/tables/")

dir.create(PATH_SAVE_UNCERTAINTY_PREC, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_TABLES, showWarnings = FALSE)

## Constants
#Graphs
GOLDEN_RATIO <- (1 + sqrt(5))/2

# Time
PERIOD_START <- as.Date("2000-01-01")
PERIOD_END <- as.Date("2019-12-31")

# Data
MIN_N_DATASETS <- 21

## Variable names
PREC_NAME <- "prec"

PREC_NAME_SHORT <- "tp"

## Parallelization
N_CORES <- 32

## Other
M2_TO_KM2 <- 10 ^ (-6)
MM_TO_M <- 10 ^ (-3)
MM_TO_KM <- 10 ^ (-6)
