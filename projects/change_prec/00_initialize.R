# Listing data to be used in the project
source("source/change_prec.R")

## Data
PREC_NAMES_RAW <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
                          list.files(path = PATH_PREC_OBS, full.names = TRUE))

PREC_NAMES_RAW <- unique(grep(paste(PREC_REPS, collapse = "|"),
                                    PREC_NAMES_RAW, value = TRUE))

PREC_NAMES_RAW <- grep("land", PREC_NAMES_RAW, value = TRUE)

PREC_NAMES_RAW_DLY <- grep("daily", PREC_NAMES_RAW, value = TRUE)

PREC_NAMES_RAW_MON <- grep("monthly", PREC_NAMES_RAW, value = TRUE)

## Save
save(PREC_NAMES_RAW_DLY, PREC_NAMES_RAW_MON,
     file = paste0(PATH_SAVE_CHANGE_PREC, "prec_names_raw.rda"))
