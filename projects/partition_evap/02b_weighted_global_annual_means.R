# Estimation of ensemble statistics
source('source/partition_prec.R')
source('source/geo_functions.R')

## Data
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

### Analysis
registerDoParallel(cores = N_CORES - 1)

prec_annual <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  dummie_raster <- prec_2000_2019[[dataset_count]]
  dummie_weights <- area(dummie_raster, na.rm = TRUE, weights = TRUE)
  dummie_table <- dummie_raster * dummie_weights
  dummie_table <- cellStats(dummie_table, 'sum')
  dummie_table <- data.table(getZ(dummie_raster), dummie_table)
  setnames(dummie_table, c("date", "wavg_yearly"))
  dummie_table <- dummie_table[, .(year = year(date), prec_mean =  wavg_yearly)]
}
names(prec_annual) <- names(prec_2000_2019)
prec_annual$`gpm-imerg`[1] <- NA
prec_annual <- bind_rows(prec_annual, .id = "column_label")
colnames(prec_annual)[1] <- "dataset"

saveRDS(prec_annual, paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))
