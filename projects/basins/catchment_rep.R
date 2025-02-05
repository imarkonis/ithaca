library(data.table)

catchment_rep <- function(data) {
  prec_data <- unique(data[, .(lon, lat, date, dataset, value)])
  
  prec_median <- prec_data[, .(median_prec = median(value, na.rm = TRUE)),
                           .(lon, lat, date)]
  
  prec_data <- merge(prec_median, prec_data, by = c("lon", "lat", "date"),
                     allow.cartesian = TRUE)
  
  prec_data <- prec_data[, .(mse_prec = mean((median_prec - value)^2,
                                             na.rm = TRUE),
                             r_prec = cor(value, median_prec,
                                          use = "pairwise.complete.obs"),
                             bias_prec = mean(value, na.rm = TRUE) - mean(median_prec, na.rm = TRUE),
                             var_prec = sd(value, na.rm = TRUE)^2,
                             median_var = sd(median_prec, na.rm = TRUE)^2),
                         .(lon, lat, dataset)]
  
  prec_data <- prec_data[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                         .(lon, lat, dataset)]
  
  prec_data <- merge(prec_data, unique(data[, .(lon, lat, dataset, pfaf_id)]),
                     by = c("lon", "lat", "dataset"), allow.cartesian = TRUE)
  
  lonlat_area <- unique(prec_data[, .(lon, lat)]) %>% .[, val := 1] %>%
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>% area() %>%
    tabular() %>% .[, .(lon, lat, area = value)]
  
  prec_data <- merge(prec_data, lonlat_area, by = c("lon", "lat"),
                     allow.cartesian = TRUE)
  
  prec_data[, area_basin := sum(area), .(dataset, pfaf_id)
            ][, area_weights := area/area_basin
              ][, weighted_t := t_prec*area_weights]
  
  prec_data <- prec_data[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, pfaf_id)]
  
  prec_data <- prec_data[, .SD[which.max(prec_t)], by = .(pfaf_id)]
  
  dummie <- data[prec_data, on = .(dataset, pfaf_id)]
  
  return(dummie)
}