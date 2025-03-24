# Reading and subsetting data for the specified period
source("source/change_prec.R")

## Data
load(paste0(PATH_SAVE_CHANGE_PREC, "prec_names_raw.rda"))

registerDoParallel(cores = N_CORES)

## Analysis
start_year <- year(PERIOD_START)

end_year <- year(PERIOD_END)

foreach(data_idx = 1:10) %dopar% {
  result <- subset_data(PREC_NAMES_RAW[data_idx],
                        yrs = c(start_year, end_year))
  short_name <- sub(".*/([^_/]*)_.*", "\\1", PREC_NAMES_RAW[data_idx])
  nc_out <- paste0(PATH_SAVE_CHANGE_PREC_RAW, short_name,
                   "_tp_mm_land_199001_201912_025_monthly.nc")
  saveNC(result, nc_out)
}

### List generated files
PREC_NAMES_1990_2019 <- list.files(path = PATH_SAVE_CHANGE_PREC_RAW,
                                   pattern = "*monthly.nc",
                                   full.names = TRUE)

## Save
save(PREC_NAMES_1990_2019,
     file = paste0(PATH_SAVE_CHANGE_PREC, "prec_names_1990_2019.rda"))
