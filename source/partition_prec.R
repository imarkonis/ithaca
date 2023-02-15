source("source/main.R")

# Packages 
packages <- c('gtools', 'rnaturalearth', 'kohonen', 'ggthemes', 'scales')
install.packages(setdiff(packages, rownames(installed.packages())))

# Paths
load('~/shared/data_projects/ithaca/misc/prec_common_periods_2000_2019.Rdata') #Created in database/scripts/05_shared period_dataset.R

PATH_SAVE_PARTITION_PREC <- paste0(PATH_SAVE, "partition_prec/")
PATH_SAVE_PARTITION_PREC_RAW <- paste0(PATH_SAVE, "partition_prec/raw/")
PATH_SAVE_PARTITION_PREC_SPATIAL <- paste0(PATH_SAVE, "partition_prec/spatial/")
PATH_SAVE_PARTITION_PREC_FIGURES <- paste0(PATH_SAVE, "partition_prec/figures/")
PATH_SAVE_PARTITION_PREC_TABLES <- paste0(PATH_SAVE, "partition_prec/tables/")

PREC_FNAMES_2000_2019 <-  list.files(path = PATH_SAVE_PARTITION_PREC_RAW, full.names = TRUE)
dummy <- strsplit(PREC_FNAMES_2000_2019, split = '//')
dummy <- sapply(dummy, "[[", 2)
dummy <- strsplit(dummy, split = '_')
PREC_FNAMES_SHORT_2000_2019 <- sapply(dummy, "[[", 1)

# Variables
MIN_N_DATASETS <- 10
n_datasets_2000_2019 <- length(PREC_FNAMES_SHORT_2000_2019)


# Specify start/end for the period of analysis 
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END
period_months <- interval(period_start, period_end) %/% months(1) + 1


