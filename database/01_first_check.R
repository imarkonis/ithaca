#Quick check of data directory consistency and downloaded data content

source('./scripts/source/main.R')

path_em_earth <- '../../shared/data/obs/precip/raw/em-earth_tp_mm_global-land_195001_201912_025_monthly.nc'
path_era5 <- '../../shared/data/sim/cloudcover/raw/era5_tcc_frac_global_195901_202112_025_monthly.nc'

map_scale_size <- 12
n_random_months <- 20
random_months <- sample(1:100, n_random_months)

em_earth_prcp <- stack(path_em_earth)
for(a_month in random_months){
  image(em_earth_prcp[[a_month]],
        col = hcl.colors(map_scale_size, "Blues3", rev = TRUE))
}

era5_cloud <- stack(path_era5)
for(a_month in random_months){
  image(era5_cloud[[a_month]], 
        col = hcl.colors(map_scale_size, "Blues3"))
}

