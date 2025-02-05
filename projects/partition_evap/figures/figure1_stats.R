# max/min and Q75/Q25 of environments ----
source("source/partition_evap.R")

## Data ----
### Landcover ----
land_cover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_land_cover.rds"))
land_cover[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
land_cover[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
land_cover[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
land_cover[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

### Biomes ----
biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_biomes.rds"))
biome[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
biome[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
biome[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
biome[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

### IPCC ----
ipcc <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ipcc.rds"))
ipcc[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
ipcc[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
ipcc[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
ipcc[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

### Elevation ----
elevation <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_elevation.rds"))
elevation[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
elevation[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
elevation[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr. model"]
elevation[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

### Global ----
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))
evap_annual_vol[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
evap_annual_vol[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
evap_annual_vol[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydrological model"]
evap_annual_vol[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]

evap_annual_vol <- evap_annual_vol[!(dataset == "etmonitor" & year == 2000), ]

### Stats ----

global_mean <- evap_annual_vol[,mean(evap_volume)]
evap_annual_vol[,mean(evap_volume), dataset_type]
evap_annual_vol[, max(evap_volume)/min(evap_volume)]
evap_annual_vol[, quantile(evap_volume, 0.75)/quantile(evap_volume, 0.25)]
evap_annual_vol[, (quantile(evap_volume, 0.75)-quantile(evap_volume, 0.25))/global_mean]

land_cover[, max(environment_volume)/min(environment_volume), .(land_cover_short_class)]
land_cover[, quantile(environment_volume, 0.75)/quantile(environment_volume, 0.25), .(land_cover_short_class)]
land_cover[, (quantile(environment_volume, 0.75)-quantile(environment_volume, 0.25))/mean(environment_volume), .(land_cover_short_class)]

biome[, max(environment_volume)/min(environment_volume), .(biome_short_class)]
biome[, quantile(environment_volume, 0.75)/quantile(environment_volume, 0.25), .(biome_short_class)]
biome[, (quantile(environment_volume, 0.75)-quantile(environment_volume, 0.25))/mean(environment_volume), .(biome_short_class)]
