# Listing data to be used in the project
source("source/uncertainty_prec.R")

## Data
PREC_NAMES_SHORT_2000_2019_FULL_RECORD <-  c("cmap", "cmorph", "cpc-global",
                                             "cru-ts-v4-08", "em-earth", "era5",
                                             "era5-land", "fldas", "gpcc-v2022",
                                             "gpcp-cdr-v3-2", "gpm-imerg-v7",
                                             "gsmap-v8", "jra55", "merra-2",
                                             "merra2-land", "mswep-v2-8",
                                             "ncep-doe", "ncep-ncar",
                                             "persiann", "precl", "terraclimate")

PREC_NAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_PREC_SIM,
                                                 full.names = TRUE),
                                      list.files(path = PATH_PREC_OBS,
                                                 full.names = TRUE))

PREC_NAMES_2000_2019_FULL_RECORD <- unique(grep(paste(PREC_NAMES_SHORT_2000_2019_FULL_RECORD,
                                                      collapse = "|"),
                                                PREC_NAMES_2000_2019_FULL_RECORD,
                                                value = TRUE))

PREC_NAMES_2000_2019_FULL_RECORD <- grep("land",
                                         PREC_NAMES_2000_2019_FULL_RECORD,
                                         value = TRUE)

PREC_NAMES_2000_2019_FULL_RECORD <- grep("monthly",
                                         PREC_NAMES_2000_2019_FULL_RECORD,
                                         value = TRUE)

## Save
save(PREC_NAMES_SHORT_2000_2019_FULL_RECORD,
     PREC_NAMES_2000_2019_FULL_RECORD,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                   "prec_names_2000_2019_full_record.rda"))
