# Best data set per ROBIN basins
source("source/uncertainty_prec.R")

registerDoParallel(cores = N_CORES)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric.rds"))

robin_meta <- fread("~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv")
robin_coords <- readRDS("~/shared/data_projects/ithaca/twc_change/data/raw/robin_coords.rds")

## Analysis
### Prepare mask and merge
lonlat_area <- unique(robin_coords[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>% area() %>%
  tabular() %>% .[, .(lon, lat, area = value)]

robin_coords <- merge(robin_coords, lonlat_area, by = c("lon", "lat"))

prec_data <- merge(prec_data, robin_coords, by = c("lon", "lat"))

### Bootstrapping
bootstrap_data <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(robin_coords, by = "robin_id")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- nrow(x)
    dummie <- x[, .SD[sample(.N, MIN_N%/%10)], by = robin_id]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
  dummie <- prec_data[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, area_basin := sum(area), .(dataset, robin_id)
         ][, area_weights := area/area_basin
           ][, weighted_t := t_prec*area_weights]
  dummie <- dummie[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                   .(dataset, robin_id)]
  dummie$loop_idx <- idx
  return(dummie)
}

### Area Weighted Average
prec_data[, area_basin := sum(area), .(dataset, robin_id)
          ][, area_weights := area/area_basin
            ][, weighted_t := t_prec*area_weights]

prec_data <- prec_data[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, robin_id)]

## Save
fwrite(prec_data,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                     "robin_ranking.csv"))

saveRDS(bootstrap_data, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                      "robin_bootstrap.rds"))
