# ============================================================================
# Generate Monte Carlo precipitation and evaporation ensembles by scenario
#
# This script:
# 1. Merges precipitation and evaporation products with region and biome masks
# 2. Filters grid cells to valid dataset x region x biome combinations
# 3. Samples one dataset per scenario x region x biome using regional biome weights
# 4. Writes one fst file per scenario and simulation
# 5. Saves metadata for all generated ensemble members
# ============================================================================

# Libraries ===================================================================

library(data.table)
library(fst)

source("source/twc_change.R")

# Inputs =======================================================================

prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap.Rds"))

weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds"))

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Constants & Variables --======================================================

set.seed(1979)

N_SIMS <- 200L
OUT_DIR <- file.path(PATH_OUTPUT_DATA, "mc_ensemble")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Input preparation ============================================================

weights_region <- weights_dt[
  is.finite(w_region_biome) &
    !is.na(w_region_biome) &
    w_region_biome > 0,
  .(scenario, region, biome, dataset, w_region_biome)
]

prec_evap_cf <- merge(
  prec_evap,
  twc_grid_classes[, .(lon, lat, region, biome)],
  by = c("lon", "lat"),
  all = FALSE
)

valid_keys <- unique(weights_region[, .(dataset, region, biome)])

prec_evap_cf <- prec_evap_cf[
  valid_keys,
  on = .(dataset, region, biome),
  nomatch = 0L
]

scenario_levels <- unique(weights_region$scenario)

scenario_dirs <- file.path(OUT_DIR, scenario_levels)
invisible(lapply(scenario_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
names(scenario_dirs) <- scenario_levels

# Helper ======================================================================

run_one_sim <- function(sim_id, prec_evap_cf, weights_region, scenario_dirs) {
  
  selected_dt <- weights_region[
    ,
    {
      idx <- sample.int(.N, size = 1L, prob = w_region_biome)
      .(dataset = dataset[idx])
    },
    by = .(scenario, region, biome)
  ]
  
  meta_list <- vector("list", length(scenario_dirs))
  
  for (i in seq_along(scenario_dirs)) {
    
    scenario_i <- names(scenario_dirs)[i]
    
    selected_sc <- selected_dt[
      scenario == scenario_i,
      .(region, biome, dataset)
    ]
    
    sim_dt <- prec_evap_cf[
      selected_sc,
      on = .(dataset, region, biome),
      nomatch = 0L,
      allow.cartesian = TRUE
    ][
      ,
      .(lon, lat, year, region, biome, prec, evap)
    ]
    
    out_file <- file.path(
      scenario_dirs[[i]],
      sprintf("mc_prec_evap_%s_sim_%03d.fst", scenario_i, sim_id)
    )
    
    write_fst(
      x = sim_dt,
      path = out_file,
      compress = 50
    )
    
    meta_list[[i]] <- data.table(
      sim = sim_id,
      scenario = scenario_i,
      file = out_file,
      n_rows = nrow(sim_dt)
    )
    
    rm(sim_dt)
    gc(verbose = FALSE)
  }
  
  rbindlist(meta_list)
}

## Monte Carlo simulation 

pb <- txtProgressBar(min = 0, max = N_SIMS, style = 3)

meta_list <- vector("list", N_SIMS)

for (sim_id in seq_len(N_SIMS)) {
  meta_list[[sim_id]] <- run_one_sim(
    sim_id = sim_id,
    prec_evap_cf = prec_evap_cf,
    weights_region = weights_region,
    scenario_dirs = scenario_dirs
  )
  setTxtProgressBar(pb, sim_id)
}

close(pb)

meta_dt <- rbindlist(meta_list)

# Outputs =====================================================================

saveRDS(
  meta_dt,
  file.path(PATH_OUTPUT_DATA, "mc_ensemble_metadata.Rds")
)

# Validation ==================================================================

validation_file <- meta_dt[1]
validation_dt <- as.data.table(read_fst(validation_file))

print(validation_dt)
