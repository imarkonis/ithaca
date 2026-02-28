# Libraries ====================================================================

source('source/twc_change.R')

# Inputs =======================================================================

weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, 
                                'dataset_agreement_weights.Rds'))
masks <- pRecipe::pRecipe_masks()

# Helpers ======================================================================

normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & !is.na(x) & (x > 0)
  if (!any(ok)) return(out)
  s <- sum(x[ok])
  if (!is.finite(s) || s <= 0) {
    out[ok] <- 1 / sum(ok)
  } else {
    out[ok] <- x[ok] / s
  }
  out
}

# Constants & Variables ========================================================

weights_region <- merge(weights_dt, 
                        masks[land_mask == 'land', 
                              .(lon, lat, region = ipcc_short_region, 
                                biome = biome_short_class)], 
                       by = c('lon', 'lat'), all.x = TRUE) 
weights_region <- weights_region[is.finite(weight) & 
                                   !is.na(weight) & weight >= 0]
weights_region <- weights_region[!region %like% "O$"]
weights_region <- weights_region[!region %like% "BOB"]
weights_region <- weights_region[!region %like% "ARS"]

# Analysis =====================================================================

weights_region_biome <- weights_region[, .(n_cells = .N, 
                                           score = mean(weight, na.rm = TRUE)), 
                                       by = .(scenario, region, biome, dataset)]

weights_region_biome[, w_region_biome := normalize_prob(score),
                     by = .(scenario, region, biome)]
weights_region_biome[, score := NULL]

# Outputs ======================================================================

saveRDS(weights_region_biome, file.path(PATH_OUTPUT_DATA, 
                                        'weights_region_biome.Rds'))

# Validation ===================================================================

make_prob_wide <- function(weights_region_biome,
                           region = "MED",
                           scenario = "base",
                           value_col = "w_region_biome") {
  x <- copy(weights_region_biome)
  setDT(x)
  
  # keep only the slice you want
  if ("scenario" %in% names(x)) x <- x[scenario == scenario]
  if ("region" %in% names(x)) x <- x[region == region]
  
  # keep only needed cols + drop NAs
  x <- x[, .(biome, dataset, value = get(value_col))]
  x <- x[is.finite(value) & !is.na(value)]
  
  # if there are duplicates, sum them (should usually not happen, but safe)
  x <- x[, .(value = mean(value)), by = .(biome, dataset)]
  
  dcast(x, biome ~ dataset, value.var = "value", fill = 0)
}

make_prob_wide(weights_region_biome)
