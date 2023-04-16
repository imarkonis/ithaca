source("source/main.R")

## Packages 
packages <- c('gtools', 'rnaturalearth', 'kohonen', 'ggthemes', 'scales', 'ggpattern', "corrr")
install.packages(setdiff(packages, rownames(installed.packages())))

## Paths
### Input - Raw data 
PREC_FNAMES_SHORT_2000_2019_FULL_RECORD <-  c("era5", "jra55", "merra2", "ncep-doe", "ncep-ncar", "chirps", 
                                  "cmap", "cmorph", "cpc", "cru-ts", "em-earth", "gpcc", "gpcp",      "gpm-imerg",
                                  "mswep", "persiann")
PREC_FNAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
                                       list.files(path = PATH_PREC_OBS, full.names = TRUE))

PREC_FNAMES_2000_2019_FULL_RECORD <- unique(grep(paste(PREC_FNAMES_SHORT_2000_2019_FULL_RECORD, collapse = "|"), 
                       PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE))
PREC_FNAMES_2000_2019_FULL_RECORD <- grep("land", PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE)
PREC_FNAMES_2000_2019_FULL_RECORD <- grep("yearly", PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE)

### Output
PATH_SAVE_PARTITION_PREC <- paste0(PATH_SAVE, "partition_prec/")
PATH_SAVE_PARTITION_PREC_RAW <- paste0(PATH_SAVE, "partition_prec/raw/")
PATH_SAVE_PARTITION_PREC_SPATIAL <- paste0(PATH_SAVE, "partition_prec/spatial/")
PATH_SAVE_PARTITION_PREC_FIGURES <- paste0(PATH_SAVE, "partition_prec/figures/")
PATH_SAVE_PARTITION_PREC_TABLES <- paste0(PATH_SAVE, "partition_prec/tables/")

### Project data
PREC_FNAMES_2000_2019 <-  list.files(path = PATH_SAVE_PARTITION_PREC_RAW, full.names = TRUE)
dummy <- strsplit(PREC_FNAMES_2000_2019, split = '//')
dummy <- sapply(dummy, "[[", 2)
dummy <- strsplit(dummy, split = '_')
PREC_FNAMES_SHORT_2000_2019 <- sapply(dummy, "[[", 1)

## Variables
MIN_N_DATASETS <- 10
n_datasets_2000_2019 <- length(PREC_FNAMES_SHORT_2000_2019)

## Specify start/end for the period of analysis 
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END
period_months <- interval(period_start, period_end) %/% months(1) + 1


