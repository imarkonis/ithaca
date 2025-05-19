# Reading and subsetting data for the specified period
source("source/uncertainty_prec.R")

## Data
load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_names_2000_2019_full_record.rda"))

registerDoParallel(cores = N_CORES)

## Analysis
n_datasets_2000_2019 <- length(PREC_NAMES_2000_2019_FULL_RECORD)

start_year <- year(PERIOD_START)

end_year <- year(PERIOD_END)

foreach(data_idx = 1:n_datasets_2000_2019) %dopar% {
  result <- subset_data(PREC_NAMES_2000_2019_FULL_RECORD[data_idx],
                        yrs = c(start_year, end_year))
  short_name <- sub(".*/([^_/]*)_.*", "\\1",
                    PREC_NAMES_2000_2019_FULL_RECORD[data_idx])
  nc_out <- paste0(PATH_SAVE_UNCERTAINTY_PREC_RAW, short_name,
                   "_tp_mm_land_200001_201912_025_monthly.nc")
  saveNC(result, nc_out)
}

### List generated files
PREC_NAMES_2000_2019 <- list.files(path = PATH_SAVE_UNCERTAINTY_PREC_RAW,
                                         pattern = "*200001",
                                         full.names = TRUE)

PREC_NAMES_SHORT_2000_2019 <- sub(".*/([^_/]*)_.*", "\\1",
                                  PREC_NAMES_2000_2019)

## Save
save(PREC_NAMES_SHORT_2000_2019, PREC_NAMES_2000_2019,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_names_2000_2019.rda"))
