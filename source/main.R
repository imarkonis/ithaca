## --- Packages ---

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(raster)
  library(ncdf4)
  library(sp)
  library(sf)
  library(stars)
})

## Paths
## --- Base Paths ---
PATH_ROOT        <- normalizePath("~/shared", mustWork = FALSE)
PATH_DATA        <- file.path(PATH_ROOT, "data")
PATH_DATA_REVIEW <- file.path(PATH_ROOT, "data_review")
PATH_PROJECTS    <- file.path(PATH_ROOT, "data_projects", "ithaca")

PATH_PREC_SIM    <- file.path(PATH_DATA, "sim/precip/raw")
PATH_PREC_OBS    <- file.path(PATH_DATA, "obs/precip/raw")
PATH_EVAP_SIM    <- file.path(PATH_DATA, "sim/evap/raw")
PATH_EVAP_OBS    <- file.path(PATH_DATA, "obs/evap/raw")
PATH_RUNOFF_SIM  <- file.path(PATH_DATA, "sim/runoff/raw")

PATH_PREC_SIM_PROC    <- file.path(PATH_DATA, "sim/precip/processed")
PATH_PREC_OBS_PROC    <- file.path(PATH_DATA, "obs/precip/processed")
PATH_EVAP_SIM_PROC    <- file.path(PATH_DATA, "sim/evap/processed")
PATH_EVAP_OBS_PROC    <- file.path(PATH_DATA, "obs/evap/processed")
PATH_RUNOFF_SIM_PROC  <- file.path(PATH_DATA, "sim/runoff/processed")

PATH_MAP <- list(
  prec = list(OBS = PATH_PREC_OBS, REANAL = PATH_PREC_SIM, REMOTE = PATH_PREC_SIM, HYDROL = PATH_PREC_SIM),
  evap = list(REANAL = PATH_EVAP_SIM, REMOTE = PATH_EVAP_SIM, HYDROL = PATH_EVAP_SIM, ENSEMB = PATH_EVAP_SIM)
)

# --- Time Constants ---
DAYS_IN_YEAR <- 365.25
SEC_IN_DAY   <- 86400

ITHACA_PERIOD_START <- as.Date("1960-01-01")
ITHACA_PERIOD_END   <- as.Date("2019-12-31")

ITHACA_PERIOD_1_START <- as.Date("1960-01-01")
ITHACA_PERIOD_1_END   <- as.Date("1989-12-31")
ITHACA_PERIOD_2_START <- as.Date("1990-01-01")
ITHACA_PERIOD_2_END   <- as.Date("2019-12-31")

# --- Spatial Constants ---
GLOBAL_AREA <- 1.345883e+14 
PILOT_LAT_MIN <- -5.25
PILOT_LAT_MAX <- 5.00
PILOT_LON_MIN <- 33.75
PILOT_LON_MAX <- 44.25

# --- Unit Conversions ---
M2_TO_KM2 <- 1e-6
MM_TO_M   <- 1e-3
MM_TO_KM  <- 1e-6

# --- Variable Names ---
PREC_NAME <- "prec"
EVAP_NAME <- "evap"
PREC_NAME_SHORT <- "tp"
EVAP_NAME_SHORT <- "e"

VARIABLES <- list(
  prec = list(name = PREC_NAME, short = PREC_NAME_SHORT),
  evap = list(name = EVAP_NAME, short = EVAP_NAME_SHORT)
)

# --- Dataset Categories ---
PREC_DATASETS <- list(
  OBS    = c("cpc", "cru-ts", "em-earth", "ghcn", "gpcc", "precl", "udel"),
  REANAL = c("20cr", "era20c", "era5-land", "jra55", "merra2", "merra2-land", "ncep-doe", "ncep-ncar"),
  REMOTE = c("chirps", "cmap", "cmorph", "gpcp", "gpm-imerg", "mswep", "persiann", "trmm-3b43"),
  HYDROL = c("fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate")
)

EVAP_DATASETS <- list(
  REANAL = c("era5-land", "jra55", "merra2", "merra2-land"),
  REMOTE = c("etmonitor", "mod16a"),
  HYDROL = c("gleam", "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "bess", "terraclimate"),
  ENSEMB = c("camele", "etsynthesis", "synthesizedet")
)

PATH_MAP <- list(
  prec = list(OBS = PATH_PREC_OBS, REANAL = PATH_PREC_SIM, REMOTE = PATH_PREC_SIM, HYDROL = PATH_PREC_SIM),
  evap = list(REANAL = PATH_EVAP_SIM, REMOTE = PATH_EVAP_SIM, HYDROL = PATH_EVAP_SIM, ENSEMB = PATH_EVAP_SIM)
)

FNAME_TO_NAME <- list(
  bess = "BESS",
  camele = "CAMEL",
  era5 = "ERA5",
  "era5-land" = "ERA5L",
  etmonitor = "ETMON",
  fldas = "FLDAS",
  "gldas-clsm-v2-1" = "CLSM",
  "gldas-noah-v2-1" = "NOAH",
  "gldas-vic-v2-1" = "VIC",
  "gleam-v3-7a" = "GLEAM",
  "gleam-v4-1a" = "GLEAM",
  jra55 = "JRA55",
  merra2 = "MERRA",
  terraclimate = "TERRA",
  etsynthesis = "ETSYN",
  "gldas-clsm-v2-0" = "CLSM",
  "gldas-noah-v2-0" = "NOAH",
  "gldas-vic-v2-0" = "VIC",
  "mhm-e-obs" = "EOBS",
  fluxcom = "FLUXC",
  "mswx-past" = "MSWX",
  mswx = "MSWX",
  "20cr-v3" = "20CR",
  era20c = "ERA20",
  "merra-2" = "MERRA",
  "merra2-land" = "MERRA",
  "ncep-doe" = "NCDOE",
  "ncep-ncar" = "NCNCR",
  mod16a = "MOD16",
  "grace-gfz" = "GRACE",
  "cmorph-cdr" = "CMORP",
  "em-earth" = "EARTH",
  "gpcc-v2020" = "GPCC",
  "gpcp-v2-3" = "GPCP",
  "gpm-imerg-v6" = "IMERG",
  "gpm-imerg-v7" = "IMERG",
  "mswep-v2-8" = "MSWEP",
  "chirps-v2" = "CHIRP",
  cmap = "CMAP",
  cpc = "CPC",
  "cpc-global" = "CPC",
  "cru-ts-v4-07" = "CRU",
  "cru-ts-v4-08" = "CRU",
  "e-obs" = "EOBS",
  "ghcn-v2" = "GHCN",
  "gpcc-v2022" = "GPCC",
  "gpcp-cdr-v3-2" = "GPCP",
  "gpcp-v3-2" = "GPCP",
  "gsmap-v8" = "GSMAP",
  persiann = "PERSI",
  "persiann-cdr" = "PERSI",
  precl = "PRECL",
  "trmm-3b43" = "TRMM",
  "udel-v5-01" = "UDEL",
  "esa-cci-sm-v07-1" = "CCI"
)

# --- Palettes ---
PALETTES <- list(
  water_cycle_change = c('steelblue3', 'darkgreen', 'darkred', 'darkorange'), #Wetter - Accelerated, Wetter - Deccelerated, Drier - Accelerated, Drier - Deccelerated
  agu                = c('#00324A', '#005294', '#058ECD', '#FFFFFF'),
  subdued_prof       = c("#90AFC5", "#336B87", "#2A3132", "#763626")
)

# --- Parallel Setup ---
N_CORES <- parallel::detectCores() - 2

# --- Optional Utility ---
get_dataset_path <- function(dataset_name, variable, spatial = "025", temporal = "monthly") {
  type <- get_dataset_type(dataset_name, variable)
  dir  <- PATH_MAP[[variable]][[type]]
  var_short <- VARIABLES[[variable]]$short
  
  pattern <- paste0(dataset_name, "_", var_short, "_mm_land_*_", spatial, "_", temporal, ".nc")
  matches <- Sys.glob(file.path(dir, pattern))
  
  if (length(matches) == 0) {
    stop("No matching NetCDF file found for: ", dataset_name)
  }
  
  return(matches[1]) 
}

get_dataset_type <- function(dataset_name, variable) {
  if (is.na(variable) || !(variable %in% names(PATH_MAP))) {
    return("UNKNOWN")
  }
  sets <- if (variable == "prec") PREC_DATASETS else if (variable == "evap") EVAP_DATASETS else return("UNKNOWN")
  for (type in names(sets)) {
    if (tolower(dataset_name) %in% sets[[type]]) return(type)
  }
  return("UNKNOWN")
}

# --- Dataset Metadata ---
build_dataset_metadata <- function() {
  sim_dirs <- list.dirs(file.path(PATH_DATA, "sim"), full.names = TRUE, recursive = TRUE)
  obs_dirs <- list.dirs(file.path(PATH_DATA, "obs"), full.names = TRUE, recursive = TRUE)
  variable_dirs <- c(sim_dirs, obs_dirs)
  
  metadata <- list()
  
  source_type <- ifelse(grepl("/sim", variable_dirs), "sim", "obs")
  
  for (i in seq_along(variable_dirs)) {
    path_dir <- variable_dirs[i]
    source <- source_type[i]
    path_parts <- strsplit(path_dir, "/")[[1]]
    i_root <- which(path_parts %in% c("sim", "obs"))
    variable <- if (length(i_root) == 1 && length(path_parts) > i_root + 1) {
      if (path_parts[i_root + 1] == "other" && length(path_parts) > i_root + 2) {
        path_parts[i_root + 2]
      } else {
        path_parts[i_root + 1]
      }
    } else {
      NA
    }
    
    files <- list.files(path_dir, full.names = TRUE, recursive = TRUE)
    
    for (f in files) {
      if (is.na(variable)) next
      fname <- tools::file_path_sans_ext(basename(f))
      parts <- unlist(strsplit(fname, "_"))
      if (length(parts) < 8) next
      
      dataset_id <- parts[1]
      var_short <- parts[2]
      unit <- parts[3]
      extent <- parts[4]
      start_time <- parts[5]
      end_time <- parts[6]
      spatial_res <- parts[7]
      timestep <- parts[8]
      
      metadata[[length(metadata) + 1]] <- list(
        source = source,
        name = ifelse(dataset_id %in% names(FNAME_TO_NAME), FNAME_TO_NAME[[dataset_id]], toupper(dataset_id)),
        fname = dataset_id,
        variable = variable,
        type = if (!is.na(variable)) get_dataset_type(dataset_id, variable) else "UNKNOWN",
        unit = unit,
        extent = extent,
        start_date = as.Date(paste0(substr(start_time, 1, 4), "-", substr(start_time, 5, 6), "-01")),
        end_date = as.Date(paste0(substr(end_time, 1, 4), "-", substr(end_time, 5, 6), "-01")),
        spatial = spatial_res,
        timestep = timestep,
        file_type = tools::file_ext(f),
        file = f
      )
    }
  }
  
  metadata <- unique(rbindlist(metadata))
  return(metadata[variable != 'other'])
}

# --- Filter Function for Metadata ---
filter_datasets <- function(var = NULL, src = NULL, typ = NULL,
                            tstep = NULL, spat = NULL, ftype = NULL,
                            area = NULL, dname = NULL, pattern = NULL) {
  dt <- copy(dataset_metadata)
  
  if (!is.null(var))    dt <- dt[variable %in% var]
  if (!is.null(src))    dt <- dt[source %in% src]
  if (!is.null(typ))    dt <- dt[type %in% typ]
  if (!is.null(tstep))  dt <- dt[timestep %in% tstep]
  if (!is.null(spat))   dt <- dt[spatial %in% spat]
  if (!is.null(ftype))  dt <- dt[file_type %in% ftype]
  if (!is.null(area))   dt <- dt[extent %in% area]
  if (!is.null(dname))  dt <- dt[name %in% dname]
  if (!is.null(pattern)) dt <- dt[grepl(pattern, file)]
  
  return(dt[])
}

summarize_datasets <- function(var = NULL, src = NULL, typ = NULL,
                               tstep = NULL, spat = NULL, ftype = NULL,
                               area = NULL, dname = NULL, pattern = NULL) {
  dt <- copy(dataset_metadata)
  
  if (!is.null(var))    dt <- dt[variable %in% var]
  if (!is.null(src))    dt <- dt[source %in% src]
  if (!is.null(typ))    dt <- dt[type %in% typ]
  if (!is.null(tstep))  dt <- dt[timestep %in% tstep]
  if (!is.null(spat))   dt <- dt[spatial %in% spat]
  if (!is.null(ftype))  dt <- dt[file_type %in% ftype]
  if (!is.null(area))   dt <- dt[extent %in% area]
  if (!is.null(dname))  dt <- dt[name %in% dname]
  if (!is.null(pattern)) dt <- dt[grepl(pattern, file)]
  
  summary <- list(
    fname    = sort(unique(dt$fname)),
    name     = sort(unique(dt$name)),
    variable = sort(unique(dt$variable)),
    type     = sort(unique(dt$type)),
    source   = sort(unique(dt$source)),
    timestep = sort(unique(dt$timestep)),
    spatial  = sort(unique(dt$spatial)),
    extent   = sort(unique(dt$extent)),
    file_type = sort(unique(dt$file_type))
  )
  
  return(summary)
}


