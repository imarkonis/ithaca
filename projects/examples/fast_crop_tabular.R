required_packages <- c("terra", "future.apply", "arrow")
installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(required_packages[!installed])
invisible(lapply(required_packages, library, character.only = TRUE))

load_dataset <- function(meta_row) {
  stopifnot(is.data.frame(meta_row) && nrow(meta_row) == 1 && "file" %in% names(meta_row))
  file_path <- meta_row$file
  stopifnot(file.exists(file_path))
  message("Loading: ", meta_row$name, " | ", meta_row$variable)
  ds <- terra::rast(file_path)
  attr(ds, "meta") <- meta_row
  return(ds)
}

# Batch load based on filters with parallel processing
load_datasets <- function(datasets) {
  future.apply::future_lapply(seq_len(nrow(datasets)), function(i) load_dataset(datasets[i, , drop = FALSE]))
}

process_dataset <- function(ds, crop_extent = NULL, time_range = NULL) {
  if (!is.null(crop_extent)) {
    ds <- terra::crop(ds, crop_extent)
  }
  
  if (!is.null(time_range)) {
    # Extract time from layer names or metadata
    time_vals <- tryCatch({
      terra::time(ds)
    }, error = function(e) {
      # fallback using layer names if time() is not available
      layer_names <- names(ds)
      ymds <- suppressWarnings(lubridate::ymd(sub(".*_(\\d{4})(\\d{2})$", "\\1\\2", layer_names)))
      ymds
    })
    
    # Subset layers by date
    keep_layers <- which(time_vals >= time_range[1] & time_vals <= time_range[2])
    if (length(keep_layers) > 0) {
      ds <- ds[[keep_layers]]
    } else {
      warning("No layers within time range: ", paste(time_range, collapse = " - "))
      ds <- ds[[0]]
    }
  }
  
  return(ds)
}

pipeline_step <- function(selected, crop_extent = NULL, time_range = NULL) {
  datasets <- load_datasets(selected)
  lapply(datasets, process_dataset, crop_extent = crop_extent, time_range = time_range)
}

raster_to_datatable <- function(r) {
  stopifnot(inherits(r, "SpatRaster"))
  
  # Attempt to extract time
  time_vals <- tryCatch(terra::time(r), error = function(e) NULL)
  
  # Determine valid date sequence
  if (!is.null(time_vals) && length(time_vals) == terra::nlyr(r)) {
    date_seq <- as.Date(time_vals)
  } else {
    start_date <- tryCatch({
      meta <- attr(r, "meta")
      if (!is.null(meta$start_date)) as.Date(meta$start_date) else as.Date("2000-01-01")
    }, error = function(e) as.Date("2000-01-01"))
    
    step <- ifelse(grepl("year", names(r)[1], ignore.case = TRUE), "year", "month")
    date_seq <- seq(start_date, by = step, length.out = terra::nlyr(r))
  }
  
  # Convert to long-format data.table
  df <- as.data.frame(r, xy = TRUE, long = TRUE, na.rm = TRUE)
  setDT(df)
  setnames(df, c("x", "y", "layer", "value"), c("lon", "lat", "layer", "value"))
  
  df[, layer_index := as.integer(gsub("\\D", "", layer))]
  df[, date := date_seq[layer_index]]
  
  df <- df[complete.cases(df), .(lon, lat, date, value)]
  setkey(df, lon, lat)
  
  return(df)
}


raster_to_datatable <- function(r) {
  stopifnot(inherits(r, "SpatRaster"))
  
  # Extract coordinates and values
  coords <- terra::xyFromCell(r, 1:terra::ncell(r))
  values <- terra::values(r)
  dt <- as.data.table(cbind(coords, values))
  
  # Wide to long
  dt_long <- melt(dt, id.vars = c("x", "y"), variable.name = "layer", value.name = "value")
  
  # Map to dates
  meta <- attr(r, "meta")
  if (!is.null(meta) && "start_date" %in% names(meta)) {
    n_layers <- terra::nlyr(r)
    dates <- seq(meta$start_date, by = "month", length.out = n_layers)
    date_map <- data.table(layer = names(r), date = dates)
    dt_long <- merge(dt_long, date_map, by = "layer", all.x = TRUE)
  }
  
  # Clean up
  dt_long <- dt_long[complete.cases(dt_long)]
  setnames(dt_long, c("x", "y"), c("lon", "lat"))
  dt_long <- dt_long[, .(lon, lat, date, value)][order(lon, lat, date)]
  
  return(dt_long[])
}




# Optional arrow support
cache_arrow <- function(data, path) {
  arrow::write_parquet(data, path)
}

load_arrow <- function(path) {
  arrow::read_parquet(path)
}
