# Correlation between datasets for different masks ----
source('source/evap_trend.R')

## libraries ----

source('source/partition_evap.R')

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))

### global ----
global_correlation <- evap_datasets[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap")[,-c(1)]
    , use = "pairwise.complete.obs"), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation"
)), .(lon, lat)]

# 
# global_correlation_p <- evap_datasets[, (melt(
#   as.data.table(
#     cor_pmat(
#       dcast(
#         .SD, formula = year ~ dataset, value.var = "evap")[,-c(1)]
#     ), keep.rownames = T),
#   variable.name = "dataset", value.name = "p-value"
# )), .(lon, lat)]

data_correlation_p[, rn := NULL]
setnames(data_correlation_p, old = "rowname", new  = "rn")

global_correlation <- merge(global_correlation, global_correlation_p, by = c("lon","lat", "rn", "dataset"), all = T)
global_correlation <- global_correlation[complete.cases(global_correlation)]

saveRDS(global_correlation, paste0(PATH_SAVE_PARTITION_EVAP, "gridwise_correlation.rds"))
