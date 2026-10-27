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

prec_data <- prec_data[year(date) <= 2024,
                       .(doy = lubridate::yday(date),
                         year = lubridate::year(date)),
                       .(lon, lat, dataset)]

dummie <- prec_data[, .(year = 1998:2024), .(lon, lat, dataset)]

prec_data <- prec_data[dummie, on = .(lon, lat, dataset, year)]

rm(dummie)

gc()

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
        file = paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x_timing_slopes.rds"))
  