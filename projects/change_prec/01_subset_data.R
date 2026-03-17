# Reading and subsetting data for the specified period
source("source/change_prec.R")

## Data
load(paste0(PATH_SAVE_CHANGE_PREC, "prec_names_raw.rda"))

registerDoParallel(cores = N_CORES)

## Analysis
start_year <- year(PERIOD_START)

end_year <- year(PERIOD_END)

foreach(data_idx = 1:length(PREC_NAMES_RAW_DLY)) %dopar% {
  short_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_NAMES_RAW_DLY[data_idx])
  cdo_string <- paste0("cdo -P 56 -z zip4 -splityear -selyear,", start_year, "/", end_year,
                       " ", PREC_NAMES_RAW_DLY[data_idx], " ",
                       PATH_SAVE_CHANGE_PREC_RAW, short_name, "_daily_")
  system(cdo_string)
  ##
  short_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_NAMES_RAW_MON[data_idx])
  cdo_string <- paste0("cdo -P 56 -z zip4 -selyear,", start_year, "/", end_year,
                       " ", PREC_NAMES_RAW_MON[data_idx], " ",
                       PATH_SAVE_CHANGE_PREC_RAW, short_name,
                       "_tp_mm_land_1991_2024_025_monthly.nc")
  system(cdo_string)
}

### List generated files
PREC_NAMES_1991_2024_DLY <- list.files(path = PATH_SAVE_CHANGE_PREC_RAW,
                                       pattern = "*daily*",
                                       full.names = TRUE)

PREC_NAMES_1991_2024_MON <- list.files(path = PATH_SAVE_CHANGE_PREC_RAW,
                                       pattern = "*monthly.nc",
                                       full.names = TRUE)

## Save
save(PREC_NAMES_1991_2024_DLY, PREC_NAMES_1991_2024_MON,
     file = paste0(PATH_SAVE_CHANGE_PREC, "prec_names_1991_2024.rda"))
