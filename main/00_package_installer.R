#Check and installation of the necessary packages for the project

options(repos = 'http://cran.rstudio.org')
have_packages <- installed.packages()
cran_packages <- c('data.table', 'tidyverse', 'dbplyr', 
                   'foreach', 'parallel', 'doParallel',
                   'raster', 'sf', 'sfc', 'rgdal', 'RNetCDF', 'rasterVis', 
                   'RPostgres', 'rpostgis', 
                   'pRecipe', 'gtools', 'rnaturalearth')
to_install <- setdiff(cran_packages, have_packages[, 1])
if(length(to_install)>0) install.packages(to_install)
