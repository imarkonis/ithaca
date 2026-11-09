#Creates project paths

source("source/main.R")
source("source/twc_change.R")
source("database/06_dataset_fnames.R")

## Paths
PATH_OUTPUT <- paste0(PATH_SAVE, "twc_change/")
PATH_OUTPUT_DATA <- paste0(PATH_OUTPUT, "data/")
PATH_OUTPUT_RAW <- paste0(PATH_OUTPUT_DATA, "raw/")
PATH_OUTPUT_FIGURES <- paste0(PATH_OUTPUT, "figures/")
PATH_OUTPUT_TABLES <- paste0(PATH_OUTPUT, "tables/")
PATH_INPUT_RAW <- "../../shared/data/stations/evap/eddy_zao/"

dir.create(PATH_OUTPUT, showWarnings = FALSE)
dir.create(PATH_OUTPUT_DATA, showWarnings = FALSE)
dir.create(PATH_OUTPUT_RAW, showWarnings = FALSE)
dir.create(PATH_OUTPUT_FIGURES, showWarnings = FALSE)
dir.create(PATH_OUTPUT_TABLES, showWarnings = FALSE)

save(PATH_OUTPUT,
     PATH_OUTPUT_RAW,
     PATH_OUTPUT_DATA,
     PATH_OUTPUT_FIGURES,
     PATH_OUTPUT_TABLES,
     PATH_INPUT_RAW,
     file = paste0(PATH_OUTPUT, "paths.Rdata"))
