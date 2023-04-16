# Estimation of ensemble statistics
source('source/partition_prec.R')
source('source/geo_functions.R')

## Data
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

## Analysis
registerDoParallel(cores = N_CORES - 1)
prec_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummie_brick <- prec_2000_2019[[dataset_count]]
  dummie_mean <- calc(dummie_brick, fun = mean, na.rm = TRUE) %>%
    as.data.frame(xy = TRUE, na.rm = TRUE) %>% as.data.table()
  dummie_sd <- calc(dummie_brick, fun = sd, na.rm = TRUE) %>%
    as.data.frame(xy = TRUE, na.rm = TRUE) %>% as.data.table()
  dummie_table <- merge(dummie_mean, dummie_sd, by = c('x', 'y'))
  setnames(dummie_table, c('lon', 'lat', 'prec_mean', 'prec_sd'))
  dummie_table$dataset <- names(prec_2000_2019[dataset_count])
  return(dummie_table)
}
setkeyv(prec_datasets, c("lon", "lat", "dataset"))
prec_datasets[, n_datasets := .N, .(lon, lat)]
prec_datasets <- prec_datasets[n_datasets >= MIN_N_DATASETS]
prec_datasets[dataset %in% PREC_DATASETS_OBS, dataset_type := 'ground stations'
              ][dataset %in% PREC_DATASETS_REANAL, dataset_type := 'reanalysis'
                ][dataset %in% PREC_DATASETS_REMOTE, dataset_type := 'remote sensing']

### Precipitation volumes 
grid_cell_area <- unique(prec_datasets[, .(lon, lat)]) %>% grid_area() # m2
prec_mean_datasets <- prec_datasets[, .(prec_mean = mean(prec_mean)), .(lon, lat, n_datasets)]
prec_volume <- grid_cell_area[prec_mean_datasets, on = .(lon, lat)]
prec_volume[, prec_volume_year := area * M2_TO_KM2 * prec_mean * MM_TO_KM][, prec_mean := NULL] # km3

prec_datasets <- prec_datasets[, .(lon, lat, dataset, dataset_type, prec_mean, prec_sd)]
prec_datasets[, prec_mean := round(prec_mean, 2)][, prec_sd := round(prec_sd, 2)]

## Save data 
saveRDS(prec_datasets, paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
saveRDS(prec_volume, paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))

## Validation
for(dataset_count in 1:n_datasets_2000_2019){
  plot(prec_2000_2019[[dataset_count]]$X2010.01.01)
}




