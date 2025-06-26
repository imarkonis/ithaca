source('source/main.R')
load(paste0("../../shared/data_projects/ithaca/twc_change/paths.Rdata"))
  
# Datasets
PREC_NAMES_SHORT <- c('ERA5L', "FLDAS", "MSWEP", "MERRA", "TERRA")
PREC_ENSEMBLE_NAMES_SHORT <- c('CPC', 'GPCC', 'EARTH', 'ERA5L', "FLDAS", "MERRA", "PRECL", "TERRA")
N_DATASETS_PREC <- length(PREC_NAMES_SHORT)

EVAP_NAMES_SHORT <- c("ERA5L", "FLDAS", "GLEAM", "MERRA", "TERRA")
EVAP_ENSEMBLE_NAMES_SHORT <- c("BESS", "ERA5L", "ETMON", "ETSYN", "FLDAS", "GLEAM", "MERRA", "TERRA", "VIC")
N_DATASETS_EVAP <- length(EVAP_NAMES_SHORT)


# Spatiotemporal data
START_PERIOD_1 <- as.Date("1981-1-1") 
END_PERIOD_1 <- as.Date("2000-12-31")
END_PERIOD_2 <- as.Date("2020-12-31")


# Graphics
MY_PALETTE <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                          "#F4CC70", "#EBB582",  "#BF9A77",
                          "#E38B75", "#CE5A57",  "#CA3433", "#785A46")