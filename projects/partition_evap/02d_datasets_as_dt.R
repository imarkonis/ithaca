# Transform datasets from brick to a single data table (large memory requirements)

source('source/partition_prec.R')
source('source/geo_functions.R')

## Data 
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

## Analysis
registerDoParallel(cores = N_CORES - 1)
prec_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- brick_to_dt(prec_2000_2019[[dataset_count]])
  dummy$dataset <- names(prec_2000_2019)[[dataset_count]]
  dummy
}

prec_datasets <- prec_datasets[, .(lon = x, lat = y, year = factor(year(time)), dataset, prec = value)]

## Save data
saveRDS(prec_datasets, paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
