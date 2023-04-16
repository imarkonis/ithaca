#' Landmask
#'
#' Function to mask a data set
#' 
#' @param x a RasterBrick to be masked
#' @param keep_land logical. If TRUE (default) you get land values, else you get ocean values
#' 
#' @example
#' dummie_brick <- brick("~/shared/data/sim/precip/raw/ncep-doe_tp_mm_global_197901_202208_025_monthly.nc")
#' land_brick <- landmask(dummie_brick)
#' ocean_brick <- landmask(dummie_brick, keep_land = FALSE)


source('./source/main.R')

landmask <- function(x, keep_land = TRUE){
  lsmask <- raster("~/shared/data/geodata/masks/final/land_ocean/mask_land_ocean_025.nc")
  inv_mask <- !keep_land
  mask_ext <- extent(lsmask)
  x_ext <- extent(x)
  if (x_ext < mask_ext){
    lsmask <- crop(lsmask, x_ext)
  }
  dummie <- mask(x, lsmask, inverse = inv_mask)
  return(dummie)
}
