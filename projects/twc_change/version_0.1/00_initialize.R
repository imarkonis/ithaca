#Creates project paths

source("source/main.R")
source("source/twc_change.R")

## Paths
PATH_OUTPUT <- paste0(PATH_SAVE, "/twc_change/")
PATH_OUTPUT_DATA <- paste0(PATH_OUTPUT, "/data/")
PATH_OUTPUT_RAW <- paste0(PATH_OUTPUT_DATA, "/raw/")
PATH_OUTPUT_FIGURES <- paste0(PATH_OUTPUT, "/figures/")
PATH_OUTPUT_TABLES <- paste0(PATH_OUTPUT, "/tables/")
PATH_OUTPUT_RAW_PREC <- paste0(PATH_OUTPUT_RAW, "/prec/") 
PATH_OUTPUT_RAW_EVAP <- paste0(PATH_OUTPUT_RAW, "/evap/") 
PATH_OUTPUT_RAW_OTHER <- paste0(PATH_OUTPUT_RAW, "/other/")
PATH_OUTPUT_RAW_TABULAR <- paste0(PATH_OUTPUT_RAW, "/tabular/")

dir.create(PATH_OUTPUT, showWarnings = FALSE)
dir.create(PATH_OUTPUT_DATA, showWarnings = FALSE)
dir.create(PATH_OUTPUT_RAW, showWarnings = FALSE)
dir.create(PATH_OUTPUT_FIGURES, showWarnings = FALSE)
dir.create(PATH_OUTPUT_TABLES, showWarnings = FALSE)
dir.create(PATH_OUTPUT_RAW_PREC, showWarnings = FALSE)
dir.create(PATH_OUTPUT_RAW_EVAP, showWarnings = FALSE)
dir.create(PATH_OUTPUT_RAW_OTHER, showWarnings = FALSE)
dir.create(PATH_OUTPUT_RAW_TABULAR, showWarnings = FALSE)

save(PATH_OUTPUT,
     PATH_OUTPUT_RAW,
     PATH_OUTPUT_DATA,
     PATH_OUTPUT_FIGURES,
     PATH_OUTPUT_TABLES,
     PATH_INPUT_RAW,
     PATH_OUTPUT_RAW_PREC,
     PATH_OUTPUT_RAW_EVAP,
     PATH_OUTPUT_RAW_OTHER,
     PATH_OUTPUT_RAW_TABULAR,
     file = paste0(PATH_OUTPUT, "paths.Rdata"))

