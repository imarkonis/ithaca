# Estimates the mean global precipitation (mm and km3) per dataset and year
# Weighted mean was used to account for different grid cell size
# Only datasets with global coverage are used

source('source/changing_prec.R')
source('source/geo_functions.R')

library(stringr)

load("~/shared/data_projects/ithaca/misc/prec_fnames_2000_2019_full_record_prec_change.Rdata") # Created by database/06b_dataset_fnames_prec_change.R

## Data
PREC_FNAMES_2000_2019_FULL_RECORD

prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019_FULL_RECORD, brick)
cleaned_names <- str_extract(PREC_FNAMES_2000_2019_FULL_RECORD, "(?<=//)[^_]+") #extract the corresponding dataset names
cleaned_names <- gsub("-v\\d+-\\d+|-v\\d+", "", cleaned_names)
names(prec_2000_2019) <- cleaned_names

# names_prec_2000_2019_global <- PREC_GLOBAL_DATASETS[PREC_GLOBAL_DATASETS %in% PREC_FNAMES_SHORT_2000_2019]
# prec_2000_2019_global <- prec_2000_2019[names_prec_2000_2019_global]
n_datasets_2000_2019_global <- length(prec_2000_2019)

### Analysis
registerDoParallel(cores = N_CORES - 1)

prec_annual <- foreach(dataset_count = 1:n_datasets_2000_2019_global) %dopar% {
  dummie_raster <- prec_2000_2019[[dataset_count]]
  dummie_weights <- area(dummie_raster, na.rm = TRUE, weights = TRUE)
  dummie_table <- dummie_raster * dummie_weights
  dummie_table <- cellStats(dummie_table, 'sum')
  dummie_table <- data.table(getZ(dummie_raster), dummie_table)
  setnames(dummie_table, c("date", "wavg_yearly"))
  dummie_table <- dummie_table[, .(year = year(date), prec_mean =  wavg_yearly)]
}
names(prec_annual) <- cleaned_names
#prec_annual$`gpm-imerg`[1] <- NA don't know why we have to make NA teh first year (i.e., 2001)
#prec_annual <- bind_rows(prec_annual[1:14], .id = "column_label")
prec_annual <- bind_rows(prec_annual, .id = "column_label")
colnames(prec_annual)[1] <- "dataset"
prec_annual[, prec_volume := GLOBAL_AREA * M2_TO_KM2 * prec_mean * MM_TO_KM]

saveRDS(prec_annual, paste0(PATH_SAVE_CHANGING_PREC, "prec_global_annual_mean.rds"))
write.csv(prec_annual, paste0(PATH_SAVE_CHANGING_PREC, "prec_global_annual_mean.csv"))
