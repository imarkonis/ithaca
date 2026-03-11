# --- Dataset Info ---
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
  "20cr-v3"           = "20CR",
  "bess"              = "BESS",
  "camele"            = "CAMEL",
  "chirps-v2"         = "CHIRP",
  "cmap"              = "CMAP",
  "cmorph-cdr"        = "CMORP",
  "cpc"               = "CPC",
  "cpc-global"        = "CPC",
  "cru-ts-v4-07"      = "CRU",
  "cru-ts-v4-08"      = "CRU",
  "e-obs"             = "EOBS",
  "em-earth"          = "EARTH",
  "era20c"            = "ERA20",
  "era5"              = "ERA5",
  "era5-land"         = "ERA5L",
  "esa-cci-sm-v07-1"  = "CCI",
  "etsynthesis"       = "ETSYN",
  "etmonitor"         = "ETMON",
  "fldas"             = "FLDAS",
  "fluxcom"           = "FLUXC",
  "ghcn-v2"           = "GHCN",
  "gldas-clsm-v2-0"   = "CLSM",
  "gldas-clsm-v2-1"   = "CLSM",
  "gldas-noah-v2-0"   = "NOAH",
  "gldas-noah-v2-1"   = "NOAH",
  "gldas-vic-v2-0"    = "VIC",
  "gldas-vic-v2-1"    = "VIC",
  "gleam-v4-1a"       = "GLEAM",
  "gpcc-v2022"        = "GPCC",
  "gpcp-cdr-v3-2"     = "GPCP",
  "gpcp-v2-3"         = "GPCP",
  "gpcp-v3-2"         = "GPCP",
  "gpm-imerg-v7"      = "IMERG",
  "grace-gfz"         = "GRACE",
  "gsmap-v8"          = "GSMAP",
  "jra55"             = "JRA55",
  "merra-2"           = "MERRA",
  "merra2"            = "MERRA",
  "merra2-land"       = "MERRA",
  "mhm-e-obs"         = "EOBS",
  "mod16a"            = "MOD16",
  "mswep-v2-8"        = "MSWEP",
  "mswx"              = "MSWX",
  "mswx-past"         = "MSWX",
  "ncep-doe"          = "NCDOE",
  "ncep-ncar"         = "NCNCR",
  "persiann"          = "PERSI",
  "persiann-cdr"      = "CDR",
  "precl"             = "PRECL",
  "terraclimate"      = "TERRA",
  "trmm-3b43"         = "TRMM",
  "udel-v5-01"        = "UDEL"
)

# --- Utility Functions ---
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
      metric <- parts[2]
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
        metric = metric,
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
filter_datasets <- function( dname = NULL, var = NULL, var2 = NULL, src = NULL, typ = NULL,
                             tstep = NULL, spat = NULL, ftype = NULL,
                             area = NULL) {
  dataset_metadata <- build_dataset_metadata()
  
  dt <- copy(dataset_metadata)
  
  if (!is.null(dname))  dt <- dt[name %in% dname]
  if (!is.null(var))    dt <- dt[variable %in% var]
  if (!is.null(var2))   dt <- dt[metric %in% var2]
  if (!is.null(src))    dt <- dt[source %in% src]
  if (!is.null(typ))    dt <- dt[type %in% typ]
  if (!is.null(tstep))  dt <- dt[timestep %in% tstep]
  if (!is.null(spat))   dt <- dt[spatial %in% spat]
  if (!is.null(ftype))  dt <- dt[file_type %in% ftype]
  if (!is.null(area))   dt <- dt[extent %in% area]
  
  
  return(dt[])
}

summarize_datasets <- function(var = NULL, src = NULL, typ = NULL,
                               tstep = NULL, spat = NULL, ftype = NULL,
                               area = NULL, dname = NULL) {
  dataset_metadata <- build_dataset_metadata()
  
  dt <- copy(dataset_metadata)
  
  if (!is.null(var))    dt <- dt[variable %in% var]
  if (!is.null(src))    dt <- dt[source %in% src]
  if (!is.null(typ))    dt <- dt[type %in% typ]
  if (!is.null(tstep))  dt <- dt[timestep %in% tstep]
  if (!is.null(spat))   dt <- dt[spatial %in% spat]
  if (!is.null(ftype))  dt <- dt[file_type %in% ftype]
  if (!is.null(area))   dt <- dt[extent %in% area]
  if (!is.null(dname))  dt <- dt[name %in% dname]
  
  summary <- list(
    fname    = sort(unique(dt$fname)),
    name     = sort(unique(dt$name)),
    variable = sort(unique(dt$variable)),
    metric   = sort(unique(dt$metric)),
    type     = sort(unique(dt$type)),
    source   = sort(unique(dt$source)),
    timestep = sort(unique(dt$timestep)),
    spatial  = sort(unique(dt$spatial)),
    extent   = sort(unique(dt$extent)),
    file_type = sort(unique(dt$file_type))
  )
  
  return(setDT(summary))
}
