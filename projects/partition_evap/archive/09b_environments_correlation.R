# Correlation between datasets for different masks ----

## libraries ----
library(rstatix)

source('source/partition_evap.R')

### Land cover  ----
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_land_cover.rds"))

data_correlation <- data[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
      , use = "pairwise.complete.obs"), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation", id.vars = "rn"
)), .(land_cover_short_class)]


data_correlation_p <- data[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)),  .(land_cover_short_class)]

data_correlation_p[, rn := NULL]
setnames(data_correlation_p, old = "rowname", new  = "rn")

data_correlation <- merge(data_correlation, data_correlation_p, by = c("land_cover_short_class", "rn", "dataset"), all = T)
data_correlation <- data_correlation[complete.cases(data_correlation)]

saveRDS(data_correlation, paste0(PATH_SAVE_PARTITION_EVAP, "correlation_land_cover.rds"))

### Biomes ----
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_biomes.rds"))

data_correlation <- data[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
      , use = "pairwise.complete.obs"), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation", id.vars = "rn"
)), .(biome_short_class)]


data_correlation_p <- data[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)),  .(biome_short_class)]

data_correlation_p[, rn := NULL]
setnames(data_correlation_p, old = "rowname", new  = "rn")

data_correlation <- merge(data_correlation, data_correlation_p, by = c("biome_short_class", "rn", "dataset"), all = T)
data_correlation <- data_correlation[complete.cases(data_correlation)]

saveRDS(data_correlation, paste0(PATH_SAVE_PARTITION_EVAP, "correlation_biome.rds"))

# IPCC regions

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ipcc.rds"))

data_correlation <- data[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
      , use = "pairwise.complete.obs"), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation", id.vars = "rn"
)), .(IPCC_ref_region)]


data_correlation_p <- data[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)),  .(IPCC_ref_region)]

data_correlation_p[, rn := NULL]
setnames(data_correlation_p, old = "rowname", new  = "rn")

data_correlation <- merge(data_correlation, data_correlation_p, by = c("IPCC_ref_region", "rn", "dataset"), all = T)
data_correlation <- data_correlation[complete.cases(data_correlation)]

saveRDS(data_correlation, paste0(PATH_SAVE_PARTITION_EVAP, "correlation_ipcc.rds"))

# KG 
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_KG.rds"))

data_correlation <- data[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
      , use = "pairwise.complete.obs"), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation", id.vars = "rn"
)), .(KG_class_1)]


data_correlation_p <- data[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)),  .(KG_class_1)]

data_correlation_p[, rn := NULL]
setnames(data_correlation_p, old = "rowname", new  = "rn")

data_correlation <- merge(data_correlation, data_correlation_p, by = c("KG_class_1", "rn", "dataset"), all = T)
data_correlation <- data_correlation[complete.cases(data_correlation)]

saveRDS(data_correlation, paste0(PATH_SAVE_PARTITION_EVAP, "correlation_KG.rds"))

# elevation
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_elevation.rds"))

data_correlation <- data[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
      , use = "pairwise.complete.obs"), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation", id.vars = "rn"
)), .(elev_class)]


data_correlation_p <- data[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = year ~ dataset, value.var = "evap_mean")[,-c(1)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)),  .(elev_class)]

data_correlation_p[, rn := NULL]
setnames(data_correlation_p, old = "rowname", new  = "rn")

data_correlation <- merge(data_correlation, data_correlation_p, by = c("elev_class", "rn", "dataset"), all = T)
data_correlation <- data_correlation[complete.cases(data_correlation)]

saveRDS(data_correlation, paste0(PATH_SAVE_PARTITION_EVAP, "correlation_elevation.rds"))

