# Transforms datasets from brick to a single data table (large memory requirements)

source('source/changing_prec.R')
source('source/geo_functions.R')

load("~/shared/data_projects/ithaca/changing_prec/prec_names_2000_2019.Rdata") # Created by database/06b_dataset_fnames_prec_change.R

## Data 
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 
n_datasets_2000_2019 <- length(PREC_FNAMES_2000_2019)

## Analysis
registerDoParallel(cores = N_CORES - 1)
prec_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- pRecipe::tabular(prec_2000_2019[[dataset_count]])
  dummy$dataset <- names(prec_2000_2019)[[dataset_count]]
  dummy
}

prec_datasets <- prec_datasets[, .(lon, lat, year = factor(year(date)), dataset, prec = value)]
unique(prec_datasets, by = "dataset")

## Save data
saveRDS(prec_datasets, paste0(PATH_SAVE_CHANGING_PREC, "prec_datasets.rds"))
