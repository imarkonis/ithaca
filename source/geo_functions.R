#' Slopes
#'
#' Function to compute linear slope of each raster grid cell
#' 
#' @param x a RasterBrick to be masked
#' @param annual a character string with the desired aggregation function. Suitable options are:
#' \itemize{
#' \item "max"
#' \item "mean"
#' \item "median"
#' \item "min"
#' \item "sum"
#' }
#' @return a RasterLayer with slope values

brick_slopes <- function(dummie_brick, annual = NULL){
  if(!is.null(annual)){
    dummie_dates <- as.Date(names(dummie_brick), format = "X%Y.%m.%d")
    dummie_start <- dummie_dates[1]
    dummie_end <- tail(dummie_dates, 1)
    if ((month(dummie_start) != 1) & (month(dummie_end) != 12)){
      start_year <- as.Date(paste0(year(dummie_start) + 1,'-01-01'), format = '%Y-%m-%d')
      end_year <- as.Date(paste0(year(dummie_end) - 1,'-12-01'), format = '%Y-%m-%d')
    } else if ((month(dummie_start) != 1) & (month(dummie_end) == 12)){
      start_year <- as.Date(paste0(year(dummie_start) + 1,'-01-01'), format = '%Y-%m-%d')
      end_year <- dummie_end
    } else if ((month(dummie_start) == 1) & (month(dummie_end) != 12)){
      start_year <- dummie_start
      end_year <- as.Date(paste0(year(dummie_end) - 1,'-12-01'), format = '%Y-%m-%d')
    } else {
      start_year <- dummie_start
      end_year <- dummie_end
    }
    dummie_brick <- subset(dummie_brick, which(getZ(dummie_brick) >= start_year & (getZ(dummie_brick) <= end_year)))
    dummie_brick <- setZ(dummie_brick, seq(start_year, end_year, by = 'month'))
    dummie_brick <- zApply(dummie_brick, by = year,
                           fun = match.fun(annual), na.rm = TRUE)
    dummie_brick <- setZ(dummie_brick, seq(start_year, end_year, by = 'year'))
  }
  dummie_time <- 1:nlayers(dummie_brick)
  X <- cbind(1, dummie_time)
  invXtX <- solve(t(X) %*% X) %*% t(X)
  quickfun <- function(y) (invXtX %*% y)[2]
  dummie_slopes <- calc(dummie_brick, quickfun)
  return(dummie_slopes)
}

#' Lake mask
#'
#' Function to mask the lakes in a data set
#' 
#' @param x a RasterBrick to be masked
#' @return a RasterBrick with the respective mask

lake_mask <- function(x){
  lsmask <- raster("~/shared/data_projects/ithaca/misc/water-bodies-mask_global_025.nc")
  mask_ext <- extent(lsmask)
  x_ext <- extent(x)
  if (x_ext < mask_ext){
    lsmask <- crop(lsmask, x_ext)
  }
  dummie <- mask(x, lsmask)
  return(dummie)
}

# Subset a brick object over space and time
crop_time <- function(dataset, start, end){
  time_filter <- which(getZ(dataset) >= start & 
                         (getZ(dataset) <= end))
  cropped <- subset(dataset, time_filter)
  dummie_names <- names(cropped)
  dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  cropped[cropped <= -9999] <- NA
  cropped <- setZ(cropped, dummie_Z)
  return(cropped)
}

crop_space_time <- function(dataset, start, end, crop_box){
  time_filter <- which(getZ(dataset) >= start & 
                         (getZ(dataset) <= end))
  filtered <- subset(dataset, time_filter)
  cropped <- crop(filtered, crop_box)
  dummie_names <- names(cropped)
  dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  cropped[cropped <= -9999] <- NA
  cropped <- setZ(cropped, dummie_Z)
  return(cropped)
}

# Transform a brick object to data.table format

brick_to_dt <- function(x){
  x_dt <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE)
  x_dt <- as.data.table(x_dt)
  setnames(x_dt, colnames(x_dt)[3], 'time')
  x_dt[, time := as.Date(time)]
  return(x_dt)
}

# Transform a raster object to data.table format

raster_to_dt <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  dummie <- foreach (idx = 1:nlayers(x), .combine = rbind) %dopar% {
    dummie_layer <- x[[idx]]
    dummie_layer <- as.data.frame(dummie_layer, xy = TRUE,long = TRUE, na.rm = TRUE) %>% as.data.table()
    setnames(dummie_layer, c("lon", "lat", "date", "value"))
    dummie_layer
  }
  return(dummie)
}

#' Grid area of each cell
#'
#' Function to compute area of each cell in m2
#' 
#' @param x a data.table. dt(lon, lat)
#' @return a data.table. dt(lon, lat, area)

grid_area <- function(x){
  x$value <- 1
  coordinates(x) <- ~ lon + lat
  gridded(x) <- TRUE
  x <- raster(x)
  proj4string(x) <- CRS("+proj=longlat +datum=WGS84")
  dummie <- area(x, na.rm = TRUE)
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  dummie <- dummie[, .(x, y, value)][, value := value * 1000000]
  setnames(dummie, c("x", "y", "value"), c("lon", "lat", "area"))
  return(dummie)
}

#' Spatial weights
#'
#' Function to compute weights of each cell in a given region
#' 
#' @param x a data.table. dt(lon, lat)
#' @return a data.table. dt(lon, lat, weight)

spatial_weight <- function(x){
  x$value <- 1
  coordinates(x) <- ~ lon + lat
  gridded(x) <- TRUE
  x <- raster(x)
  proj4string(x) <- CRS("+proj=longlat +datum=WGS84")
  dummie <- area(x, na.rm = TRUE, weights = TRUE)
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  dummie <- dummie[, .(x, y, value)]
  setnames(dummie, c("x", "y", "value"), c("lon", "lat", "weight"))
  return(dummie)
}

#' Save .nc file
#'
#' Function to save data compatible with pRecipe in .nc file
#'
#' @import ncdf4
#' @param dummie_nc a Raster object to be saved.
#' @param nc_out a character string with the file path for the saved file.
#' @return No return value, called to save a file

save_nc <- function(dummie_nc, nc_out, name_def = "tp", longname_def = "Total precipitation"){
  lon <- xFromCol(dummie_nc) %>% round(., 4)
  lat <- yFromRow(dummie_nc) %>% round(., 4)
  time <- getZ(dummie_nc)
  if (is.character(time) | is.numeric(time)) {
    if (is.numeric(time)) {
      time <- as.character(time)
    }
    if (length(time[time == "00"]) >= 1) {
      time <- sub("^00$", "", time)
      time <- time[time != ""]
      time <- as.Date(time)
    } else if (!Reduce("|",grepl("-01", time))) {
      time <- as.numeric(time)
      dummie_origin <- "1970-01-01 00:00:00"
      time <- as.Date(time, origin = dummie_origin)
    } else {
      time <- as.Date(time)
    }
  }
  tp <- getValues(dummie_nc)
  tp[is.na(tp)] <- -9999
  deflon <- ncdim_def("lon", vals = lon, longname = "longitude",
                      units = "degrees_east")
  deflat <- ncdim_def("lat", vals = lat, longname = "latitude",
                      units = "degrees_north")
  deftime <- ncdim_def("time", vals = as.numeric(time), longname = "time",
                       units = "days since 1970-01-01 00:00:0.0",
                       calendar = "standard",
                       unlim = TRUE)
  deftp <- ncvar_def(name = name_def, units = "mm", 
                     list(deflon, deflat, deftime), 
                     missval = -9999,
                     compression = 4,
                     longname = longname_def,
                     prec = "float")
  ncoutput <- nc_create(nc_out, list(deftp), force_v4 = TRUE, verbose = FALSE)
  ncvar_put(ncoutput, deftp, tp)
  ncatt_put(ncoutput,"lon","axis","X") 
  ncatt_put(ncoutput,"lat","axis","Y")
  ncatt_put(ncoutput,"time","axis","T")
  nc_close(ncoutput)
}

#' Parallelisation of bootstrapped trend across lon lat using the Theil-Sen slope and block-bootstrap from openair package
#' 
#' @import openair
#' @param x input data.table with lon and lat columns, dt(lon, lat, date, pollutant)
#' @param pollutant column name used for trend
#' @param autocor if true slope estimate will be bootstrapped
#' @return data.table with slope, lower and upper estimates and p-value

trends_lon_lat_boot <- function(x, pollutant = "evap", autocor = TRUE) {
no_cores <- detectCores() - 1
if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
registerDoParallel(cores = no_cores)
if (length(unique(x$lon)) > length(unique(x$lat))) {
  x <- split(x, by = "lon")
} else {
  x <- split(x, by = "lat")
}

dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("openair")) %dopar% {
  dummie_row <- x[[idx]]
  dummie_row <- dummie_row[, TheilSen(.SD, pollutant = pollutant, autocor = autocor, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)]
                           , 
                           .(lon, lat)]
}
return(dummie)
}

#' Parallelisation of bootstrapped trend across lon, lat for each dataset using the Theil-Sen slope and block-bootstrap from openair package
#' 
#' @import openair
#' @param x input data.table with lon and lat columns, dt(lon, lat, date, dataset, pollutant)
#' @param pollutant column name used for trend
#' @param autocor if true slope estimate will be bootstrapped
#' @return data.table with slope, lower and upper estimates and p-value
trends_datasets_boot <- function(x, pollutant = "evap", autocor = TRUE) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }
  
  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("openair")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, TheilSen(.SD, pollutant = pollutant, autocor = autocor, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)]
                             , 
                             .(lon, lat, dataset)]
  }
  return(dummie)
}


rank_repres <- function(data, method = "all", ensemble = "median") {
  prec_ensemble <- data[, .(ensemble = match.fun(ensemble)(value, na.rm = TRUE)),
                        .(date)]
  
  if (method == "mean") {
    stat_ensemble <- prec_ensemble[, .(mean_ensemble = mean(ensemble, na.rm = TRUE))]
    prec_data <- data[, .(repres_metric = mean(value, na.rm = TRUE)), .(dataset)]
    stat_ensemble <- cbind(prec_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(repres_metric = abs((repres_metric - mean_ensemble)/mean_ensemble)),
                                   .(dataset)]
    stat_ensemble <- stat_ensemble[, .(repres_metric = (1 - repres_metric)), .(dataset)]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
  } else if (method == "var") {
    stat_ensemble <- prec_ensemble[, .(var_ensemble = sd(ensemble, na.rm = TRUE)^2)]
    prec_data <- data[, .(repres_metric = sd(value, na.rm = TRUE)^2), .(dataset)]
    stat_ensemble <- cbind(prec_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(repres_metric = abs((repres_metric - var_ensemble)/var_ensemble)),
                                   .(dataset)]
    stat_ensemble <- stat_ensemble[, .(repres_metric = (1 - repres_metric)), .(dataset)]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
  } else if (method == "slope") {
    stat_ensemble <- prec_ensemble[, .(trend_ensemble = lm(ensemble ~ date)$coefficients[2])]
    prec_data <- data[, .(repres_metric = lm(value ~ date)$coefficients[2]), .(dataset)]
    stat_ensemble <- cbind(prec_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(repres_metric = abs((repres_metric - trend_ensemble)/trend_ensemble)),
                                   .(dataset)]
    stat_ensemble <- stat_ensemble[, .(repres_metric = (1 - repres_metric)), .(dataset)]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
  } else if (method == "kge") {
    stat_ensemble <- prec_ensemble[, .(mean_ensemble = mean(ensemble, na.rm = TRUE),
                                       sd_ensemble = sd(ensemble, na.rm = TRUE))]
    stat_data <- data[, .(mean_prec = mean(value, na.rm = TRUE),
                          sd_prec = sd(value, na.rm = TRUE)), .(dataset)]
    stat_ensemble <- cbind(stat_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(alpha = (sd_prec/mean_prec)/(sd_ensemble/mean_ensemble),
                                       beta = mean_prec/mean_ensemble), .(dataset)]
    prec_data <- merge(data, prec_ensemble, by = "date", allow.cartesian = TRUE)
    prec_data <- prec_data[, .(r_prec = cor(value, ensemble,
                                            use = "pairwise.complete.obs")),
                           .(dataset)]
    stat_ensemble <- merge(stat_ensemble, prec_data, by = "dataset")
    stat_ensemble <- stat_ensemble[, .(repres_metric = 1 - sqrt(((r_prec - 1)^2) + ((alpha - 1)^2) + ((beta - 1)^2))), .(dataset)]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
  } else if (method == "tss") {
    stat_ensemble <- prec_ensemble[, .(sd_ensemble = sd(ensemble, na.rm = TRUE))]
    stat_data <- data[, .(sd_prec = sd(value, na.rm = TRUE)), .(dataset)]
    stat_ensemble <- cbind(stat_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(a = sd_prec/sd_ensemble), .(dataset)]
    prec_data <- merge(data, prec_ensemble, by = "date", allow.cartesian = TRUE)
    prec_data <- prec_data[, .(r_prec = cor(value, ensemble,
                                            use = "pairwise.complete.obs")),
                           .(dataset)]
    stat_ensemble <- merge(stat_ensemble, prec_data, by = "dataset")
    stat_ensemble <- stat_ensemble[, .(repres_metric = 2*(1 + r_prec)/((a + (1/a))^2)), .(dataset)]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
  } else if (method == "kld") {
    Q <- approxfun(density(prec_ensemble$ensemble))
    ALL_DATA <- unique(data$dataset)
    stat_ensemble <- foreach(idx = 1:length(ALL_DATA), .combine = rbind) %do% {
      prec_data <- data[dataset == ALL_DATA[idx]]
      P <- approxfun(density(prec_data$value))
      dx <- diff(sort(unique(prec_data$value)))
      dy <- diff(sort(unique(prec_ensemble$ensemble)))
      ex <- min(dx)
      ey <- min(dy)
      e <- min(ex, ey)/2
      n <- length(prec_data$value)
      x <- sort(prec_data$value)
      KL <- sum(log((P(x) - P(x - e))/(Q(x) - Q(x - e))), na.rm = TRUE) / n
      if (KL < 0) (KL <- 0)
      dummie <- data.table("dataset" = ALL_DATA[idx], repres_metric = KL)
      dummie
    }
    stat_ensemble[, repres_metric := 1 - repres_metric]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
  } else if (method == "all") {
    ## Mean
    stat_ensemble <- prec_ensemble[, .(mean_ensemble = mean(ensemble, na.rm = TRUE))]
    prec_data <- data[, .(repres_metric = mean(value, na.rm = TRUE)), .(dataset)]
    stat_ensemble <- cbind(prec_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(repres_metric = abs((repres_metric - mean_ensemble)/mean_ensemble)),
                                   .(dataset)]
    stat_ensemble <- stat_ensemble[, .(repres_metric = (1 - repres_metric)), .(dataset)]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    dummie_all <- stat_ensemble[order(-repres_metric)]
    dummie_all <- dummie_all[, .(mean = repres_metric), .(dataset)]
    ## Variance
    stat_ensemble <- prec_ensemble[, .(var_ensemble = sd(ensemble, na.rm = TRUE)^2)]
    prec_data <- data[, .(repres_metric = sd(value, na.rm = TRUE)^2), .(dataset)]
    stat_ensemble <- cbind(prec_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(repres_metric = abs((repres_metric - var_ensemble)/var_ensemble)),
                                   .(dataset)]
    stat_ensemble <- stat_ensemble[, .(repres_metric = (1 - repres_metric)), .(dataset)]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
    stat_ensemble <- stat_ensemble[, .(dataset, variance = repres_metric)]
    dummie_all <- merge(dummie_all, stat_ensemble, by = "dataset")
    ## Slope
    stat_ensemble <- prec_ensemble[, .(trend_ensemble = lm(ensemble ~ date)$coefficients[2])]
    prec_data <- data[, .(repres_metric = lm(value ~ date)$coefficients[2]), .(dataset)]
    stat_ensemble <- cbind(prec_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(repres_metric = abs((repres_metric - trend_ensemble)/trend_ensemble)),
                                   .(dataset)]
    stat_ensemble <- stat_ensemble[, .(repres_metric = (1 - repres_metric)), .(dataset)]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
    stat_ensemble <- stat_ensemble[, .(dataset, slope = repres_metric)]
    dummie_all <- merge(dummie_all, stat_ensemble, by = "dataset")
    ## KGE
    stat_ensemble <- prec_ensemble[, .(mean_ensemble = mean(ensemble, na.rm = TRUE),
                                       sd_ensemble = sd(ensemble, na.rm = TRUE))]
    stat_data <- data[, .(mean_prec = mean(value, na.rm = TRUE),
                          sd_prec = sd(value, na.rm = TRUE)), .(dataset)]
    stat_ensemble <- cbind(stat_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(alpha = (sd_prec/mean_prec)/(sd_ensemble/mean_ensemble),
                                       beta = mean_prec/mean_ensemble), .(dataset)]
    prec_data <- merge(data, prec_ensemble, by = "date", allow.cartesian = TRUE)
    prec_data <- prec_data[, .(r_prec = cor(value, ensemble,
                                            use = "pairwise.complete.obs")),
                           .(dataset)]
    stat_ensemble <- merge(stat_ensemble, prec_data, by = "dataset")
    stat_ensemble <- stat_ensemble[, .(repres_metric = 1 - sqrt(((r_prec - 1)^2) + ((alpha - 1)^2) + ((beta - 1)^2))), .(dataset)]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
    stat_ensemble <- stat_ensemble[, .(dataset, kge = repres_metric)]
    dummie_all <- merge(dummie_all, stat_ensemble, by = "dataset")
    ## TSS
    stat_ensemble <- prec_ensemble[, .(sd_ensemble = sd(ensemble, na.rm = TRUE))]
    stat_data <- data[, .(sd_prec = sd(value, na.rm = TRUE)), .(dataset)]
    stat_ensemble <- cbind(stat_data, stat_ensemble)
    stat_ensemble <- stat_ensemble[, .(a = sd_prec/sd_ensemble), .(dataset)]
    prec_data <- merge(data, prec_ensemble, by = "date", allow.cartesian = TRUE)
    prec_data <- prec_data[, .(r_prec = cor(value, ensemble,
                                            use = "pairwise.complete.obs")),
                           .(dataset)]
    stat_ensemble <- merge(stat_ensemble, prec_data, by = "dataset")
    stat_ensemble <- stat_ensemble[, .(repres_metric = 2*(1 + r_prec)/((a + (1/a))^2)), .(dataset)]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
    stat_ensemble <- stat_ensemble[, .(dataset, tss = repres_metric)]
    dummie_all <- merge(dummie_all, stat_ensemble, by = "dataset")
    ## KLD
    Q <- approxfun(density(prec_ensemble$ensemble))
    ALL_DATA <- unique(data$dataset)
    stat_ensemble <- foreach(idx = 1:length(ALL_DATA), .combine = rbind) %do% {
      prec_data <- data[dataset == ALL_DATA[idx]]
      P <- approxfun(density(prec_data$value))
      dx <- diff(sort(unique(prec_data$value)))
      dy <- diff(sort(unique(prec_ensemble$ensemble)))
      ex <- min(dx)
      ey <- min(dy)
      e <- min(ex, ey)/2
      n <- length(prec_data$value)
      x <- sort(prec_data$value)
      KL <- sum(log((P(x) - P(x - e))/(Q(x) - Q(x - e))), na.rm = TRUE) / n
      if (KL < 0) (KL <- 0)
      dummie <- data.table("dataset" = ALL_DATA[idx], repres_metric = KL)
      dummie
    }
    stat_ensemble[, repres_metric := 1 - repres_metric]
    stat_ensemble[repres_metric < 0, repres_metric := 0]
    stat_ensemble <- stat_ensemble[order(-repres_metric)]
    stat_ensemble <- stat_ensemble[, .(dataset, kld = repres_metric)]
    dummie_all <- merge(dummie_all, stat_ensemble, by = "dataset")
    stat_ensemble <- dummie_all
  }
  return(stat_ensemble)
}

plot_ipcc_pies <- function(to_plot) {
  # deps
  require(data.table)
  require(ggplot2)
  require(scatterpie)
  require(maps)
  
  # ensure data.table
  dt <- data.table::as.data.table(to_plot)
  
  # detect the "variable" column (factor/character, not lon/lat/value/region)
  base_exclude <- c("lon","lat","value","region","ipcc_short_region","x","y")
  var_candidates <- setdiff(
    names(dt)[vapply(dt, function(col) is.factor(col) || is.character(col), logical(1L))],
    base_exclude
  )
  if (length(var_candidates) == 0L)
    stop("Couldn't find the 'variable' column. Make sure one column is factor/character (besides lon/lat/value).")
  variable_col <- var_candidates[1L]
  
  # masks (keep land only) → add region
  masks <- pRecipe::pRecipe_masks()
  dt <- merge(
    dt,
    masks[land_mask == "land", .(lon, lat, ipcc_short_region)],
    by = c("lon","lat")
  )
  setnames(dt, "ipcc_short_region", "region")
  
  # IPCC reference hexagon centroids
  ipcc_hexagon <- data.table::fread("/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv")[
    , .(region = Acronym, x = CENTROIX, y = CENTROIY)
  ]
  
  # add hexagon coords and clean
  dt <- merge(dt, ipcc_hexagon, by = "region", allow.cartesian = TRUE)
  dt <- dt[complete.cases(dt)]
  
  # count slices per region for pies
  counts <- dt[, .N, by = .(region, x, y, var = get(variable_col))]
  to_plot_pie <- dcast(counts, region + x + y ~ var, value.var = "N", fill = 0)
  
  # world basemap
  world <- ggplot2::map_data("world")
  base_map <- ggplot(world, aes(long, lat)) +
    geom_map(map = world, aes(map_id = region), fill = NA, color = "black") +
    coord_quickmap()
  
  # pie columns: everything except id/coords
  pie_cols <- setdiff(names(to_plot_pie), c("region","x","y"))
  
  # pallete
  MY_PALETTE <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                            "#F4CC70", "#EBB582",  "#BF9A77",
                            "#E38B75", "#CE5A57",  "#CA3433", "#785A46")
                            # plot
  p <- base_map +
    scatterpie::geom_scatterpie(
      data = to_plot_pie,
      aes(x = x, y = y, group = region),
      cols = pie_cols
    ) +
    theme_void()
  
  return(p)
}
