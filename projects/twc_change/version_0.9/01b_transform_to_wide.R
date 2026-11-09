source('source/twc_change.R')

files <- list.files(PATH_OUTPUT_RAW_TABULAR, pattern = "\\.Rds$", full.names = TRUE)

get_short_name <- function(fname) sub(".*/([^_/]*)_.*", "\\1", fname)
get_var_from_file <- function(fname) sub(".*_(prec|evap)\\.Rds$", "\\1", fname)

short_names <- sapply(files, get_short_name)
variables   <- sapply(files, get_var_from_file)

prec_indices <- which(short_names %in% PREC_NAMES_SHORT & variables == "prec")
evap_indices <- which(short_names %in% EVAP_NAMES_SHORT & variables == "evap")

dt_list <- list()

# Precipitation
for (i in prec_indices) {
  dt <- readRDS(files[i])
  dt[dataset == "MSWEP", dataset := "GLEAM"]  
  dt_list[[length(dt_list) + 1]] <- dt
}

# Evaporation
for (i in evap_indices) {
  dt <- readRDS(files[i])
  dt_list[[length(dt_list) + 1]] <- dt
}

all_dt <- rbindlist(dt_list, fill = TRUE)

wide_dt <- dcast(
  all_dt,
  lon + lat + date + dataset ~ variable,
  value.var = "value"
)

setcolorder(wide_dt, c("lon", "lat", "date", "dataset", "prec", "evap"))
wide_dt <- wide_dt[complete.cases(wide_dt)]

saveRDS(wide_dt, paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
