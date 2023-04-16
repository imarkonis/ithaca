source('source/main.R')

## Paths & files
### Input
FNAME_PREC_MSWEP <- "/home/rstudio/shared/data/obs/precip/raw/mswep_tp_mm_land_197902_202212_025_monthly.nc"
FNAME_PREC_GPCC <- "/home/rstudio/shared/data/obs/precip/raw/gpcc_tp_mm_land_189101_201912_025_monthly.nc"
FNAME_PREC_GPCP <- "/home/rstudio/shared/data/obs/precip/raw/gpcp_tp_mm_land_197901_202205_025_monthly.nc"

### Output
PATH_SAVE_EUROPE <- paste0(PATH_SAVE, "europe/")
PATH_SAVE_EUROPE_RAW <- paste0(PATH_SAVE, "europe/raw")
PATH_SAVE_EUROPE_SPATIAL <- paste0(PATH_SAVE, "europe/spatial/")
PATH_SAVE_EUROPE_FIGURES <- paste0(PATH_SAVE, "europe/figures/")
PATH_SAVE_EUROPE_TABLES <- paste0(PATH_SAVE, "europe/tables/")

## Case study
### Time
period_start <- as.Date("1980-01-01") 
period_end <- ITHACA_PERIOD_END

### Space
lat_max <- 70.96513 
lat_min <- 34.80801
lon_max <- 33.79659
lon_min <- -23.14154 

study_area <- extent(lon_min, 
                     lon_max, 
                     lat_min, 
                     lat_max)
