#Scatter plot matrix
source("source/change_prec.R")

library(trend)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))

prec_data <- prec_data[, if (.N == 28L) .SD, by = .(lon, lat, dataset)]
prec_data[, year := year(date)]
setorder(prec_data, lon, lat, dataset, year)

dummie <- dcast(prec_data, lon + lat + dataset ~ year, value.var = "prec")

ids <- dummie[, .(lon, lat, dataset)]

dummie <- as.matrix(dummie[, !c("lon", "lat", "dataset"), with = FALSE])

obs <- prec_data[, .(slope = sens.slope(prec)$estimates), .(lon, lat, dataset)]

setkey(obs, lon, lat, dataset)
setkey(ids, lon, lat, dataset)

obs <- obs[ids, slope]

system.time({
extreme <- mc_sen_pvalue(
  dummie,
  obs,
  n_sim = 10000,
  seed = 123
)
})

p_value <- (extreme + 1) / (10000 + 1)

result <- cbind(
  ids,
  slope = obs,
  extreme = extreme,
  p_value = p_value
)
