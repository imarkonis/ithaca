# ============================================================================
# Best (rank 1) vs worst dataset maps for four agreement properties.
#
# Within each grid cell, datasets are ranked on four properties by their
# relative bias to the reference ensemble (lower bias = rank 1):
#   prec_mean, prec_trend, evap_mean, evap_trend.
# Output: which dataset is best and which is worst in each cell, per property.
# ============================================================================


# Inputs ======================================================================

library(maps)
library(grid)

source("source/twc_change.R")

dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, "dataset_ranks.Rds"))


# Constants ===================================================================

# property label  <-  source bias column (a loss: lower = better)
PROPERTY_BIAS <- c(
  prec_mean  = "prec_mean_bias",
  prec_trend = "prec_bias_slope",
  evap_mean  = "evap_mean_bias",
  evap_trend = "evap_bias_slope"
)


# Functions ===================================================================

# best and worst dataset per cell, ranking by bias_col ascending (1 = lowest bias)
best_worst_at <- function(dt, bias_col) {
  dt[
    is.finite(get(bias_col)),
    {
      v <- get(bias_col)
      .(position = c("best", "worst"),
        dataset  = c(dataset[which.min(v)], dataset[which.max(v)]),
        n_avail  = .N)
    },
    by = .(lon, lat)
  ]
}

build_best_worst_maps <- function(dt, property_bias) {
  rbindlist(lapply(names(property_bias), function(prop) {
    best_worst_at(dt, property_bias[[prop]])[, property := prop]
  }), use.names = TRUE)
}


# Analysis ====================================================================

# rank among the physics-eligible datasets, consistent with the weighting pipeline
components <- dataset_ranks[pe_ratio_check == TRUE & n_below_pet > 6]

bw_maps <- build_best_worst_maps(components, PROPERTY_BIAS)

bw_maps[, property := factor(property, levels = names(PROPERTY_BIAS))]
bw_maps[, position := factor(position, levels = c("best", "worst"))]


# Outputs =====================================================================

saveRDS(bw_maps, file.path(PATH_OUTPUT_DATA, "dataset_best_worst_maps.Rds"))


# Plot ========================================================================

ggplot(bw_maps) +
  geom_tile(aes(x = lon, y = lat, fill = dataset)) +
  borders("world", colour = "grey20", linewidth = 0.15) +
  facet_grid(position ~ property) +
  coord_equal(expand = FALSE) +
  labs(
    title = "Best and worst dataset by grid cell",
    subtitle = "Ranked within cell by relative-bias agreement; lower bias = best",
    x = NULL, y = NULL, fill = "Dataset"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    panel.spacing = unit(0.4, "lines")
  )

# Validate ====================================================================

bw_maps[, .N, .(dataset, position)]
