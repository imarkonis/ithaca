grid_meta <- unique(dataset_ranks[, .(lon, lat, ipcc_short_region, biome_short_class)])
setDT(grid_meta)

# attach meta to weights_dt
w0 <- merge(copy(weights_dt), grid_meta, by = c("lon", "lat"), all.x = TRUE)
w0 <- w0[is.finite(weight) & !is.na(weight) & weight >= 0]

# ============================================================
# 8) Aggregate grid to biome within IPCC region (no area)
# ============================================================
dataset_region_biome <- w0[
  ,
  .(
    n_cells = .N,
    w_rb_raw = mean(weight, na.rm = TRUE)
  ),
  by = .(scenario, ipcc_short_region, biome, dataset)
]

dataset_region_biome[, w_region_biome := normalize_final(w_rb_raw),
                     by = .(scenario, ipcc_short_region, biome)]
dataset_region_biome[, w_rb_raw := NULL]

# biome fractions within each IPCC region (cell fractions, no area)
biome_area <- w0[
  ,
  .(biome_cells = uniqueN(paste(lon, lat, sep = "_"))),
  by = .(scenario, ipcc_short_region, biome)
]
biome_area[, A_region_biome := biome_cells / sum(biome_cells),
           by = .(scenario, ipcc_short_region)]
biome_area[, biome_cells := NULL]

# ============================================================
# 9) Monte Carlo sampling of dataset worlds (scenario aware)
# ============================================================
sample_dataset_worlds <- function(w_rb, A_rb, n_sims = 2000, seed = 1L) {
  set.seed(seed)
  
  key <- w_rb[, .(dataset, prob = w_region_biome),
              by = .(scenario, ipcc_short_region, biome)]
  key <- key[is.finite(prob) & !is.na(prob) & prob > 0]
  
  rb <- unique(key[, .(scenario, ipcc_short_region, biome)])
  if (nrow(rb) == 0) stop("No region-biome probabilities available for sampling")
  
  out <- rbindlist(lapply(seq_len(n_sims), function(s) {
    sel <- key[
      ,
      .(dataset = sample(dataset, size = 1, prob = prob)),
      by = .(scenario, ipcc_short_region, biome)
    ]
    sel[, sim := s]
    sel
  }))
  
  merge(out, A_rb, by = c("scenario", "ipcc_short_region", "biome"), all.x = TRUE)
}

mc_choices <- sample_dataset_worlds(dataset_region_biome, biome_area, n_sims = 2000, seed = 42)

# ============================================================
# 10) Expected region weights and global weights (no area)
# ============================================================
expected_region_weights <- merge(
  dataset_region_biome,
  biome_area,
  by = c("scenario", "ipcc_short_region", "biome"),
  all.x = TRUE
)

expected_region_weights[, w_region_expected := w_region_biome * A_region_biome]

expected_region_weights <- expected_region_weights[
  ,
  .(w_region_expected = sum(w_region_expected, na.rm = TRUE)),
  by = .(scenario, ipcc_short_region, dataset)
]
expected_region_weights[, w_region_expected := normalize_final(w_region_expected),
                        by = .(scenario, ipcc_short_region)]

# region weights for global mixture: fraction of cells per region (scenario aware)
region_cells <- w0[, .(n_cells = uniqueN(paste(lon, lat, sep = "_"))),
                   by = .(scenario, ipcc_short_region)]
region_cells[, region_w := n_cells / sum(n_cells), by = scenario]
region_cells[, n_cells := NULL]

expected_global <- merge(
  expected_region_weights,
  region_cells,
  by = c("scenario", "ipcc_short_region"),
  all.x = TRUE
)

expected_global <- expected_global[
  ,
  .(w_global_expected = sum(w_region_expected * region_w, na.rm = TRUE)),
  by = .(scenario, dataset)
]
expected_global[, w_global_expected := normalize_final(w_global_expected), by = scenario]
setorder(expected_global, scenario, -w_global_expected)

# ============================================================
# Helpers updated for scenario
# ============================================================
make_prob_wide <- function(dataset_region_biome, region = "MED", scenario = "base") {
  x <- dataset_region_biome[scenario == scenario & ipcc_short_region == region,
                            .(biome, dataset, prob = w_region_biome)]
  dcast(x, biome ~ dataset, value.var = "prob", fill = 0)
}

make_area_wide <- function(biome_area, region = "MED", scenario = "base") {
  x <- biome_area[scenario == scenario & ipcc_short_region == region, .(biome, A_region_biome)]
  dcast(x, . ~ biome, value.var = "A_region_biome", fill = 0)
}

make_mc_wide <- function(mc_choices, region = "MED", scenario = "base", n_show = 10) {
  x <- mc_choices[scenario == scenario & ipcc_short_region == region & sim <= n_show,
                  .(biome, sim, dataset)]
  x[, sim := paste0("Ens_", sim)]
  dcast(x, biome ~ sim, value.var = "dataset")
}