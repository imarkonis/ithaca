roi <- terra::ext(33.75, 44.25, -5.25, 5.00)
time_range <- as.Date(c("2000-01-01", "2010-12-31"))

# Filter metadata
selected <- filter_datasets(dname = c("EARTH", "GLEAM"), var = "evap", tstep = 'monthly', area = 'land')
selected <- filter_datasets(dname = c("GLEAM"),  tstep = 'yearly')

test <- raster_to_datatable(load_dataset(selected))

# Load and preprocess
results <- pipeline_step(selected)

# Inspect one of the processed datasets and its metadata
terra::plot(results[[1]])
attr(results[[1]], "meta")

raster_to_datatable(results[[1]])



# 1. Select a lon/lat pair from the data.table
dt <- raster_to_datatable(results[[1]])
sample_point <- dt[1, .(lon, lat)]

# 2. Extract values from terra raster at that point
r <- results[[1]]
values_from_raster <- terra::extract(r, sample_point[, .(lon, lat)])

# 3. Get corresponding values from data.table
values_from_dt <- dt[lon == sample_point$lon & lat == sample_point$lat][order(date), value]

# 4. Compare
comparison <- data.table(
  raster = as.numeric(values_from_raster[1, -1]),  # remove ID column
  dt = values_from_dt
)

# 5. Show difference (if any)
comparison[, difference := abs(raster - dt)]

print(comparison)


safe_raster_to_dt <- function(r) {
  if (!inherits(r, "SpatRaster")) stop("Input must be a SpatRaster")
  meta <- attr(r, "meta")
  
  df <- terra::as.data.frame(r, xy = TRUE, long = TRUE, na.rm = TRUE)
  setDT(df)
  setnames(df, c("x", "y", "layer", "value"), c("lon", "lat", "date", "value"))
  
  # Convert layer to actual date based on meta
  if (!is.null(meta)) {
    nlyr <- terra::nlyr(r)
    if (!is.null(meta$start_date)) {
      df[, date := seq(meta$start_date, by = "month", length.out = nlyr)[as.integer(gsub("\\D", "", date))]]
    }
  }
  
  df <- df[complete.cases(df)]
  setcolorder(df, c("lon", "lat", "date", "value"))
  setkey(df, lon, lat)
  return(df)
}
dt_results <- future.apply::future_lapply(results, safe_raster_to_dt, future.seed = TRUE)

library(future)
plan(multisession, workers = 8)
dt_results <- future_lapply(results, raster_to_datatable)

