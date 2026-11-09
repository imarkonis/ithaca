# Libraries ====================================================================

library(data.table)
library(parallel)
library(arrow)
source("source/twc_change.R")

# Inputs =======================================================================

prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))

weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds"))
masks <- pRecipe::pRecipe_masks()

# Constants ====================================================================

set.seed(42)
n_sims <- 200

out_dir <- file.path(PATH_OUTPUT_DATA, "mc_ensemble")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

weights_region <- weights_dt[, .(
  scenario, region, biome, dataset, w_region_biome
)][
  is.finite(w_region_biome) & !is.na(w_region_biome) & w_region_biome > 0
]

prec_evap_cf <- merge(
  prec_evap,
  masks[, .(
    lon,
    lat,
    region = ipcc_short_region,
    biome  = biome_short_class
  )],
  by = c("lon", "lat"),
  all = FALSE
)

valid_keys <- unique(weights_region[, .(dataset, region, biome)])

prec_evap_cf <- prec_evap_cf[
  valid_keys,
  on = .(dataset, region, biome),
  nomatch = 0L
]

setkey(prec_evap_cf, dataset, region, biome)

run_one_sim <- function(s, prec_evap_cf, weights_region, out_dir) {
  
  sel <- weights_region[, {
    idx <- sample(.N, size = 1, prob = w_region_biome)
    .(dataset = dataset[idx])
  }, by = .(scenario, region, biome)]
  
  sel[, sim := s]
  
  out_meta <- vector("list", length(unique(sel$scenario)))
  k <- 1L
  
  for (sc in unique(sel$scenario)) {
    
    sel_sc <- sel[scenario == sc, .(region, biome, dataset)]
    setkey(sel_sc, dataset, region, biome)
    
    sim_dt <- prec_evap_cf[
      sel_sc,
      on = .(dataset, region, biome),
      nomatch = 0L,
      allow.cartesian = TRUE
    ][, .(
      lon,
      lat,
      year,
      region,
      biome,
      prec,
      evap
    )]
    
    scenario_dir <- file.path(out_dir, sc)
    dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
    
    out_file <- file.path(
      scenario_dir,
      sprintf("mc_prec_evap_%s_sim_%03d.parquet", sc, s)
    )
    
    write_parquet(sim_dt, out_file)
    
    out_meta[[k]] <- data.table(
      sim = s,
      scenario = sc,
      file = out_file,
      n_rows = nrow(sim_dt)
    )
    k <- k + 1L
    
    rm(sim_dt)
    gc()
  }
  
  rbindlist(out_meta)
}

pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
meta_list <- vector("list", n_sims)

for (s in seq_len(n_sims)) {
  meta_list[[s]] <- run_one_sim(
    s = s,
    prec_evap_cf = prec_evap_cf,
    weights_region = weights_region,
    out_dir = out_dir
  )
  setTxtProgressBar(pb, s)
}

close(pb)

# Validation ===================================================================

f <- file.path(
  PATH_OUTPUT_DATA,
  "mc_ensemble",
  "base",
  "mc_prec_evap_base_sim_199.parquet"
)

dt <- as.data.table(read_parquet(f))
dt
