weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_agreement_weights.Rds'))

uniform_entropy <- function(w, eps = 1e-12) {
  w <- w[is.finite(w) & !is.na(w) & w > 0]
  K <- length(w)
  if (K <= 1) return(NA_real_)  # or 0, but NA is cleaner for "not defined"
  w <- w / sum(w)
  H <- -sum(w * log(pmax(w, eps)))
  as.numeric(H / log(K))
}

uniform_effectiveN <- function(w, eps = 1e-12) {
  w <- w[is.finite(w) & !is.na(w) & w > 0]
  K <- length(w)
  if (K <= 1) return(0)
  w <- w / sum(w)
  H <- -sum(w * log(pmax(w, eps)))
  Neff <- exp(H)
  as.numeric((Neff - 1) / (K - 1))
}


entropy_dt <- weights_dt[
  ,
  .(
    h_weight   = uniform_entropy(weight),
    n_eff_weight = uniform_effectiveN(weight),
    n_datasets = sum(is.finite(weight) & !is.na(weight) & weight > 0)
  ),
  by = .(lon, lat, scenario)
]

# examples (replace with your actual test coords)
entropy_dt[lon == lon_test_a & lat == 35.125]
entropy_dt[lon == lon_test_b & lat == 35.125]
