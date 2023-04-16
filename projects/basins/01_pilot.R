library(data.table)
library(ggplot2)
library(sf)
library(raster)
library(rpostgis)
library(RPostgres)
library(tmap)
library(pRecipe)

db_name <- 'earth'
db_schema <- 'basin_boundaries'
host_ip <- '10.152.183.146' 
port_n <- '5432'
tmap_options(check.and.fix = TRUE)

con <- dbConnect(Postgres(), dbname = db_name, host = host_ip, port = port_n,        
                 user = "yannis",      
                 password = rstudioapi::askForPassword("Database password"))

single_pfaf_id <- 17249
single_basin <- st_read(con, query = paste0("SELECT * FROM ", db_schema, ".basins_all_regions_4_11 
                                         WHERE (LEFT(pfaf_id, 5) IN ('", single_pfaf_id, "'))"))

tm_shape(single_basin) + tm_polygons(style = "quantile", col = "pfaf_id") +
  tm_legend(outside = TRUE, text.size = .8) 

global <- brick("../../shared/data/obs/precip/raw/em-earth_tp_mm_land_195001_201912_025_monthly.nc")

test <- crop(global, single_basin)
test_mask <- mask(test, single_basin)
plot(test_mask)
