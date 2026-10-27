#Alt Ensemble
source("source/change_prec.R")

library("Rcpp")

sourceCpp("projects/change_prec/aux_theil_sen_mann_kendall.cpp")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs.rds"))

## Analysis
prec_data <- prec_data[year <= 2024 & dataset != "ensemble"]

dummie <- prec_data[, .(year = 1998:2024), .(lon, lat, dataset)]

prec_data <- prec_data[dummie, on = .(lon, lat, dataset, year)]

rm(dummie)

gc()

prec_data <- prec_data[, .(prec = median(prec, na.rm = TRUE)),
                       .(lon, lat, year)]
prec_data$dataset <- "ensemble_new"

setorder(prec_data, lon, lat, dataset, year)

prec_data[, prec := round(prec, 1)]

stopifnot(nrow(prec_data) %% 27 == 0)

n_groups <- nrow(prec_data) / 27

n_threads <- parallel::detectCores() - 1

dummie <- sen_mk_parallel(prec = prec_data$prec,
                          n_groups = n_groups,
                          n_threads = n_threads)

idx <- seq(1L, nrow(prec_data), by = 27L)

prec_trends <- data.table(lon = prec_data$lon[idx],
                          lat = prec_data$lat[idx],
                          dataset = prec_data$dataset[idx],
                          slope = dummie[, 1],
                          p_value = dummie[, 2])

saveRDS(prec_trends,
        file = paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_yrs_slopes.rds"))

rm(list = ls())

gc()
######
#Scatter plot matrix
source("source/change_prec.R")

library("Rcpp")

sourceCpp("projects/change_prec/aux_theil_sen_mann_kendall.cpp")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))

## Analysis
prec_data <- prec_data[year(date) <= 2024 & dataset != "ensemble",
                       .(lon, lat, dataset, year = year(date), prec)]

dummie <- prec_data[, .(year = 1998:2024), .(lon, lat, dataset)]

prec_data <- prec_data[dummie, on = .(lon, lat, dataset, year)]

rm(dummie)

gc()

prec_data <- prec_data[, .(prec = median(prec, na.rm = TRUE)),
                       .(lon, lat, year)]
prec_data$dataset <- "ensemble_new"

setorder(prec_data, lon, lat, dataset, year)

stopifnot(nrow(prec_data) %% 27 == 0)

n_groups <- nrow(prec_data) / 27

n_threads <- parallel::detectCores() - 1

dummie <- sen_mk_parallel(prec = prec_data$prec,
                          n_groups = n_groups,
                          n_threads = n_threads)

idx <- seq(1L, nrow(prec_data), by = 27L)

prec_trends <- data.table(lon = prec_data$lon[idx],
                          lat = prec_data$lat[idx],
                          dataset = prec_data$dataset[idx],
                          slope = dummie[, 1],
                          p_value = dummie[, 2])

## Save data
saveRDS(prec_trends,
        file = paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_r1x_slopes.rds"))

rm(list = ls())

gc()
######
#Scatter plot matrix
source("source/change_prec.R")

library("Rcpp")

sourceCpp("projects/change_prec/aux_theil_sen_mann_kendall.cpp")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))

prec_tot <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs.rds"))

## Analysis
prec_data <- merge(prec_data[year(date) <= 2024 & dataset != "ensemble",
                             .(lon, lat, year = year(date), dataset, prec)],
                   prec_tot[year <= 2024 & dataset != "ensemble",
                            .(lon, lat, year, dataset, tot = round(prec, 1))],
                   by = c("lon", "lat", "year", "dataset"), all = TRUE)
prec_data <- prec_data[, .(lon, lat, year, dataset, epf = prec/tot)]

dummie <- prec_data[, .(year = 1998:2024), .(lon, lat, dataset)]

prec_data <- prec_data[dummie, on = .(lon, lat, dataset, year)]

rm(dummie, prec_tot)

gc()

prec_data <- prec_data[, .(epf = median(epf, na.rm = TRUE)), .(lon, lat, year)]
prec_data$dataset <- "ensemble_new"

setorder(prec_data, lon, lat, dataset, year)

stopifnot(nrow(prec_data) %% 27 == 0)

n_groups <- nrow(prec_data) / 27

n_threads <- parallel::detectCores() - 1

dummie <- sen_mk_parallel(prec = prec_data$epf,
                          n_groups = n_groups,
                          n_threads = n_threads)

idx <- seq(1L, nrow(prec_data), by = 27L)

prec_trends <- data.table(lon = prec_data$lon[idx],
                          lat = prec_data$lat[idx],
                          dataset = prec_data$dataset[idx],
                          slope = dummie[, 1],
                          p_value = dummie[, 2])

## Save data
saveRDS(prec_trends,
        file = paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_epf_slopes.rds"))

rm(list = ls())

gc()

######

# ============================================================
# Monte-Carlo significance test for circular Theil-Sen trends
# ============================================================
source("source/change_prec.R")

library("Rcpp")

sourceCpp("projects/change_prec/aux_timing_permutation.cpp")
# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))
prec_data <- prec_data[year(date) <= 2024 & dataset != "ensemble"]
prec_data <- prec_data[, `:=`(doy = lubridate::yday(date),
                           year = lubridate::year(date))]
prec_data <- prec_data[, .(prec = median(prec, na.rm = TRUE),
                           doy = median(doy, na.rm = TRUE)),
                       .(lon, lat, year)]

dummie <- prec_data[, .(year = 1998:2024), .(lon, lat)]

prec_data <- prec_data[dummie, on = .(lon, lat, year)]

rm(dummie)

gc()

prec_data$dataset <- "ensemble_new"

setorder(prec_data, lon, lat, dataset, year)

prec_data[, group_id := .GRP, .(lon, lat, dataset)]

doy_mat <- matrix(prec_data$doy, ncol = 27, byrow = TRUE)

year_mat <- matrix(prec_data$year, ncol = 27, byrow = TRUE)

group_info <- unique(prec_data[, .(group_id, lon, lat, dataset)])

setorder(group_info, group_id)

stopifnot(nrow(doy_mat) == nrow(group_info))

all(apply(year_mat, 1, function(x) identical(as.integer(x), 1998:2024)))

# ============================================================
# Monte Carlo processing in chunks
# ============================================================

run_mc_chunk <- function(start, end, B = 500, seed = 12345) {
  n <- end - start + 1
  
  out <- matrix(NA_real_, nrow = n, ncol = 4)
  
  for (i in seq_len(n)) {
    g <- start + i - 1
    
    ans <- mc_circular_sen(doy = doy_mat[g, ], year = year_mat[g, ], B = B,
                           min_perm = min(100, B),
                           early_stop = min(500, B),
                           early_p = 0.20,
                           seed = seed + g)
    
    out[i, ] <- ans
  }
  
  out
}

# ------------------------------------------------------------
# First screening pass
# ------------------------------------------------------------

N_groups <- nrow(group_info)

screen_B <- 500

chunk_size <- 5000

screen_results <- vector("list", ceiling(N_groups / chunk_size))

counter <- 1

for (start in seq(1, N_groups, by = chunk_size)) {
  end <- min(start + chunk_size - 1, N_groups)
  
  cat("Screening groups", start, "to", end, "of", N_groups, "\n")
  
  screen_results[[counter]] <- run_mc_chunk(start = start, end = end,
                                            B = screen_B, seed = 100000)
  
  counter <- counter + 1
  
  gc()
}

screen_mat <- do.call(rbind, screen_results)

screen_dt <- copy(group_info)
screen_dt[, `:=`(slope = screen_mat[, 1], exceed = screen_mat[, 2],
                 n_perm = screen_mat[, 3], p_value = screen_mat[, 4])]

candidates <- screen_dt[p_value <= 0.10]

nrow(candidates)

full_B <- 10000

candidate_ids <- candidates$group_id

full_results <- matrix(NA_real_, nrow = length(candidate_ids), ncol = 4)

for (i in seq_along(candidate_ids)) {
  
  g <- candidate_ids[i]
  
  if (i %% 100 == 0) {cat("Full MC:", i, "/", length(candidate_ids), "\n")}
  
  full_results[i, ] <- mc_circular_sen(doy = doy_mat[g, ], year = year_mat[g, ],
                                       B = full_B, min_perm = full_B,
                                       early_stop = full_B,
                                       seed = 500000 + g)
}

full_dt <- data.table(group_id = candidate_ids, slope_full = full_results[, 1],
                      exceed_full = full_results[, 2],
                      n_perm_full = full_results[, 3],
                      p_value_full = full_results[, 4])

results <- merge(screen_dt, full_dt, by = "group_id", all.x = TRUE)
results[!is.na(p_value_full), `:=`(slope = slope_full, exceed = exceed_full,
                                   n_perm = n_perm_full, p_value = p_value_full)]
results[, c("slope_full", "exceed_full", "n_perm_full", "p_value_full") := NULL]

saveRDS(results[, .(lon, lat, dataset, slope, p_value)],
        file = paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_r1x_timing_slopes.rds"))

rm(list = ls())

gc()

####
#Scatter plot matrix
source("source/change_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x.rds"))
prec_data <- prec_data[year(date) <= 2024 & dataset != "ensemble"]
prec_data <- prec_data[, .(prec = median(prec, na.rm = TRUE),
                           date = median(date, na.rm = TRUE)),
                       .(lon, lat, year(date))]
prec_data[, month_count := .N, .(lon, lat, lubridate::month(date))]
prec_data <- prec_data[, .SD[month_count == max(month_count)], .(lon, lat)]
prec_data <- prec_data[, .(month_mode = median(lubridate::yday(date))),
                       .(lon, lat)]
prec_data[, month_mode := month(as.Date("2010-01-01") + month_mode - 1)]

saveRDS(prec_data,
        file = paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_month_mode.rds"))

rm(list = ls())

gc()

#####

#Scatter plot matrix
source("source/change_prec.R")

library("Rcpp")

sourceCpp("projects/change_prec/aux_theil_sen_mann_kendall.cpp")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_wet_days.rds"))

## Analysis
prec_data <- prec_data[date <= 2024 & dataset != "ensemble",
                       .(lon, lat, dataset, year = date, prec = wet_days)]

dummie <- prec_data[, .(year = 1998:2024), .(lon, lat, dataset)]

prec_data <- prec_data[dummie, on = .(lon, lat, dataset, year)]

rm(dummie)

gc()

prec_data <- prec_data[, .(prec = median(prec, na.rm = TRUE)), .(lon, lat, year)]
prec_data$dataset <- "ensemble_new"

setorder(prec_data, lon, lat, dataset, year)

stopifnot(nrow(prec_data) %% 27 == 0)

n_groups <- nrow(prec_data) / 27

n_threads <- parallel::detectCores() - 1

dummie <- sen_mk_parallel(prec = prec_data$prec,
                          n_groups = n_groups,
                          n_threads = n_threads)

idx <- seq(1L, nrow(prec_data), by = 27L)

prec_trends <- data.table(lon = prec_data$lon[idx],
                          lat = prec_data$lat[idx],
                          dataset = prec_data$dataset[idx],
                          slope = dummie[, 1],
                          p_value = dummie[, 2])

## Save data
saveRDS(prec_trends,
        file = paste0(PATH_SAVE_CHANGE_PREC, "new_ensemble_wet_days_slopes.rds"))

