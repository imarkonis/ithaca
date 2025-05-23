source('source/main.R')
load(paste0("../../shared/data_projects/ithaca/twc_change/paths.Rdata"))
  
# Datasets
PREC_FNAMES <- c('~/shared/data/obs/precip/raw/gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc',
                 "~/shared/data/obs/precip/raw/cpc_tp_mm_land_197901_202208_025_yearly.nc",
                 "~/shared/data/obs/precip/raw/em-earth_tp_mm_land_195001_201912_025_yearly.nc",
                 '~/shared/data/sim/precip/raw/era5-land_tp_mm_land_195001_202112_025_yearly.nc',
                 "~/shared/data/sim/precip/raw/merra2-land_tp_mm_land_198001_202308_025_yearly.nc")

PREC_NAMES_SHORT <- c('GPCC', 'CPC', 'EARTH', 'ERA5L', 'MERRA')
N_DATASETS_PREC <- length(PREC_NAMES_SHORT)

EVAP_FNAMES <- c('~/shared/data/sim/evap/raw/gleam-v3-7a_e_mm_land_198001_202112_025_yearly.nc',
                 '~/shared/data/sim/evap/raw/terraclimate_e_mm_land_195801_202112_025_yearly.nc',
                 "~/shared/data/sim/evap/raw/fldas_e_mm_land_198201_202212_025_yearly.nc",
                 "~/shared/data/sim/evap/raw/era5-land_e_mm_land_195001_202112_025_yearly.nc", 
                 "~/shared/data/sim/evap/raw/merra2_e_mm_land_198001_202301_025_yearly.nc") 

EVAP_NAMES_SHORT <- c('GLEAM', 'TERRA', 'FLDAS', 'ERA5L', 'MERRA')
N_DATASETS_EVAP <- length(EVAP_NAMES_SHORT)

# Spatiotemporal data
START_PERIOD_1 <- as.Date("1981-1-1") 
END_PERIOD_1 <- as.Date("2000-12-31")
END_PERIOD_2 <- as.Date("2020-12-31")


# Graphics
MY_PALETTE <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                          "#F4CC70", "#EBB582",  "#BF9A77",
                          "#E38B75", "#CE5A57",  "#CA3433", "#785A46")