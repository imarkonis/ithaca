library(doParallel)
library(foreach)
library(data.table)
library(parallel)
library(pRecipe)
library(RPostgres)
library(raster)
library(rpostgis)
library(rstudioapi)
library(sf)
library(terra)

crop_catchment <- function(lon, lat, dataset_fnames, catchment_level){
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  
  catchment_level <- sort(catchment_level)
  
  con <- dbConnect(Postgres(), dbname = 'earth', host = '10.152.183.146',
                   port = 5432, user = askForPassword('Database user'),      
                   password = askForPassword('Database password'))
  
  atlas <- readRDS('~/shared/data_projects/schemata/data/basin_atlas_feats.rds')
  
  basin <- lapply(catchment_level, function(x) {
    st_read(con, query = paste0('SELECT * FROM basin_boundaries.global_', x,
                                ' WHERE ST_Within(ST_SetSRID(ST_POINT(',
                                lon, ', ', lat,'), 4326), geom::geometry)'))
  }) 
  
  XMIN <- 180
  XMAX <- -180
  YMIN <- 90
  YMAX <- -90
  MAX_EXTENT <- lapply(basin, st_bbox)
  MAX_EXTENT <- lapply(MAX_EXTENT, function(x){
    if (x$xmin < XMIN) (XMIN <- as.numeric(x$xmin))
    if (x$ymin < YMIN) (YMIN <- as.numeric(x$ymin))
    if (x$xmax > XMAX) (XMAX <- as.numeric(x$xmax))
    if (x$ymax > YMAX) (YMAX <- as.numeric(x$ymax))
    return(c(XMIN, YMIN, XMAX, YMAX))
  }) %>% as.data.table() %>% t() %>% as.data.table()
  
  MAX_EXTENT <- MAX_EXTENT[, .(xmin = min(V1), ymin = min(V2), xmax = max(V3),
                               ymax = max(V4))] %>% as.numeric()
  
  basin <-rbindlist(basin)
  
  basin[, pfaf_id := as.character(pfaf_id)]
  
  basin <- merge(basin, atlas, by = c('pfaf_id', 'hybas_id', 'coast'))
  
  cropped <- foreach(data_idx = 1:length(dataset_fnames), .combine = rbind) %do% {
    dummie_brick <- brick(dataset_fnames[data_idx])
    
    dummie_table <- foreach (idx = 1:nlayers(dummie_brick), .combine = rbind) %dopar% {
      dummie_layer <- dummie_brick[[idx]]
      dummie_layer <- as.data.frame(dummie_layer, xy = TRUE, long = TRUE,
                                    na.rm = TRUE) %>% as.data.table()
      setnames(dummie_layer, c("lon", "lat", "date", "value"),
               skip_absent = TRUE)
      dummie_layer <- dummie_layer[(lon >= MAX_EXTENT[1] - 1) &
                                     (lon <= MAX_EXTENT[3] + 1) &
                                     (lat >= MAX_EXTENT[2] - 1) &
                                     (lat <= MAX_EXTENT[4] + 1)]
      dummie_layer
    }
    
    dummie_crop <- foreach(basin_idx = 1:length(catchment_level), .combine = rbind) %dopar% {
      dummie_basin <- basin[level == catchment_level[basin_idx]] %>% st_as_sf()
      coordinates(dummie_table) <- ~ lon + lat
      proj4string(dummie_table) <- st_crs(dummie_basin)$proj4string
      dummie_table <- dummie_table[!is.na(over(dummie_table, as(as(dummie_basin, "Spatial"), "SpatialPolygons"))), ]
      dummie_table <- as.data.table(dummie_table)
      dummie_table$pfaf_id <- dummie_basin$pfaf_id
      return(dummie_table)
    }
    
    dummie_name <- sub(".*/([^_/]*)_.*", "\\1", dataset_fnames[data_idx])
    dummie_crop$dataset <- dummie_name
    return(dummie_crop)
  }
}
