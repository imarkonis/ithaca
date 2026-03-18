# ============================================================================
# Aggregate dataset agreement weights to region-biome level.
#
# This script:
# 1. Joins grid-cell weights with region and biome masks
# 2. Filters invalid cells and excluded regions
# 3. Computes mean dataset weight by scenario, region, biome, and dataset
# 4. Normalizes weights within each scenario-region-biome combination
# 5. Computes biome fractions within each scenario-region-dataset combination
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, "dataset_weights.Rds"))

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Helpers =====================================================================

normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & !is.na(x) & (x > 0)
  
  if (!any(ok)) {
    return(out)
  }
  
  s <- sum(x[ok])
  
  if (!is.finite(s) || s <= 0) {
    out[ok] <- 1 / sum(ok)
  } else {
    out[ok] <- x[ok] / s
  }
  
  out
}

# Prepare grid-level region data ==============================================

weights_region <- merge(
  weights_dt,
  twc_grid_classes[, .(lon, lat, region, biome)],
  by = c("lon", "lat"),
  all.x = TRUE
)

weights_region <- weights_region[
  is.finite(weight) &
    !is.na(weight) &
    weight >= 0 &
    !is.na(region) &
    !is.na(biome)
]

# Analysis ====================================================================

weights_region_biome <- weights_region[
  ,
  .(score = mean(weight, na.rm = TRUE)),
  by = .(scenario, region, biome, dataset)
]

weights_region_biome[
  ,
  w_region_biome := normalize_prob(score),
  by = .(scenario, region, biome)
]

weights_region_biome[, score := NULL]

biome_fraction_dt <- weights_region[
  ,
  .(n_cells = .N),
  by = .(scenario, region, biome, dataset)
]

biome_fraction_dt[
  ,
  biome_fraction := n_cells / sum(n_cells),
  by = .(scenario, region, dataset)
]

weights_region_biome <- merge(
  weights_region_biome,
  biome_fraction_dt,
  by = c("scenario", "region", "biome", "dataset"),
  all.x = TRUE
)

setcolorder(
  weights_region_biome,
  c("scenario", "region", "biome", "dataset",
    "w_region_biome", "biome_fraction", "n_cells")
)

# Outputs =====================================================================

saveRDS(
  weights_region_biome,
  file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds")
)

# Validation ==================================================================


make_prob_wide <- function(dt,
                           reg = "MED",
                           scen = "base",
                           value_col = "w_region_biome") {
  dt <- copy(dt)
  
  if ("scenario" %in% names(dt)) {
    dt <- dt[scenario == scen]
  }
  
  if ("region" %in% names(dt)) {
    dt <- dt[region == reg]
  }
  
  dt <- dt[
    ,
    .(biome, dataset, value = get(value_col))
  ][is.finite(value) & !is.na(value)]
  
  dt <- dt[, .(value = mean(value)), by = .(biome, dataset)]
  
  dcast(dt, biome ~ dataset, value.var = "value")
}

test <- make_prob_wide(weights_region_biome)
rowSums(test[, -1])
