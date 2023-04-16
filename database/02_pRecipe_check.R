#Checking subseting and ploting with pRecipe

source('./scripts/source/main.R')

library(pRecipe)

path_era5 <- paste0(path_data, 'sim/precip/raw/era5_tp_mm_global_195901_202112_025_monthly.nc')

#download_data(name = "era5", project_folder = ".")
show_info("./data/database/era5_tp_mm_global_195901_202112_025_monthly.nc")

#subset_spacetime("era5", start_year = 1981, end_year = 2020, 
#                 bbox = c(2,28,42,58), 
#                 database_path = "./data/database/")
show_info("./data/processed/era5_tp_mm_subset_1981_2020_025_monthly.nc")


#subset_spacetime("era5", start_year = 1981, end_year = 2020, 
#                 bbox = c(2,28,42,58), 
#                 database_path = paste0(path_data, 'sim/precip/raw/'))

make_ts("era5", database_path = "./data/database/")
tp_ts <- readr::read_csv("./data/processed/era5_tp_mm_global_195901_202112_025_monthly_ts.csv")
head(tp_ts, 12)

era5_subset <- raster::brick("./data/processed/era5_tp_mm_subset_1981_2020_025_monthly.nc")
make_ts(era5_subset)
subset_ts <- readr::read_csv("./data/processed/era5_tp_mm_subset_1981_2020_025_monthly_ts.csv")
head(subset_ts, 12)


global <- raster::raster("./data/database/era5_tp_mm_global_195901_202112_025_monthly.nc")
plot_map(global[[1]])

central_europe <- raster::brick("./data/processed/era5_tp_mm_subset_1981_2020_025_monthly.nc")
plot_map(central_europe[[1]])

plot_line(tp_ts)
plot_heatmap(subset_ts)
plot_box(subset_ts)




#
show_info(path_era5)
make_ts("era5", database_path = "./data/database/")
tp_ts <- readr::read_csv("./data/processed/era5_ts.csv")
head(tp_ts, 12)
