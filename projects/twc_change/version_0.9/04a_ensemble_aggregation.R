# ============================================================================
# Aggregate annual weighted time series for precipitation and evaporation
# for:
#   1) original datasets
#   2) Monte Carlo ensemble members by scenario
#   3) deterministic top1 scenario per region x biome
#
# Outputs:
#   A. dataset_global_yearly_prec_evap_weighted.Rds
#      dataset, year, prec, evap
#
#   B. dataset_region_yearly_prec_evap_weighted.Rds
#      dataset, region, year, prec, evap
#
#   C. mc_global_yearly_prec_evap_weighted_by_sim.Rds
#      scenario, sim_id, year, prec, evap
#
#   D. mc_region_yearly_prec_evap_weighted_by_sim.Rds
#      scenario, sim_id, region, year, prec, evap
#
#   E. scenario_global_yearly_prec_evap_weighted_with_top1.Rds
#      scenario, year, prec, evap, n_sim
#      includes both soft MC scenarios and appended *_top1 scenarios
#
#   F. scenario_region_yearly_prec_evap_weighted_with_top1.Rds
#      scenario, region, year, prec, evap, n_sim
#      includes both soft MC scenarios and appended *_top1 scenarios
#
# Design:
#   - serial over scenarios
#   - parallel over ensemble members within each scenario
#   - Linux-friendly via parallel::mclapply()
#   - uses cell area for weighted means
#   - avoids expensive reshaping
# ============================================================================

# Libraries ==================================================================

library(data.table)
library(arrow)
library(parallel)
library(foreach)
library(doParallel)

source("source/twc_change.R")

# Thread controls =============================================================

old_dt_threads <- data.table::getDTthreads()
data.table::setDTthreads(1)

Sys.setenv(
  OMP_NUM_THREADS         = "1",
  OPENBLAS_NUM_THREADS    = "1",
  MKL_NUM_THREADS         = "1",
  VECLIB_MAXIMUM_THREADS  = "1",
  ARROW_NUM_THREADS       = "1"
)

# Inputs =====================================================================

prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap.Rds"))
grid_cell_weights <- readRDS(file.path(PATH_OUTPUT_DATA, "grid_area_weights.Rds"))
weights_region_biome <- readRDS(file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds"))
masks <- pRecipe::pRecipe_masks()

mc_root <- file.path(PATH_OUTPUT_DATA, "mc_ensemble")
scenario_dirs <- list.dirs(mc_root, full.names = TRUE, recursive = FALSE)
scenario_names <- basename(scenario_dirs)

# Constants & Variables ========================================================

# A modest worker count is usually better for parquet workloads
n_workers <- max(1L, detectCores() - 2L)

# Helpers ======================================================================

if (!"area" %in% names(grid_cell_weights)) {
  stop("grid_area_weights.Rds must contain an 'area' column.")
}

cell_area <- unique(as.data.table(grid_cell_weights)[, .(lon, lat, area)])
setkey(cell_area, lon, lat)

region_biome_lookup <- as.data.table(
  masks[
    land_mask == "land",
    .(
      lon,
      lat,
      region = factor(ipcc_short_region),
      biome  = factor(biome_short_class)
    )
  ]
)
setkey(region_biome_lookup, lon, lat)

weighted_mean_area <- function(x, area) {
  ok <- is.finite(x) & is.finite(area) & !is.na(x) & !is.na(area)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * area[ok]) / sum(area[ok])
}

aggregate_yearly_ts <- function(dt, group_cols) {
  dt[
    ,
    .(
      prec = weighted_mean_area(prec, area),
      evap = weighted_mean_area(evap, area)
    ),
    by = group_cols
  ]
}

process_mc_file <- function(f, scenario, cell_area) {
  sim_id <- tools::file_path_sans_ext(basename(f))
  
  dt <- as.data.table(read_parquet(
    f,
    col_select = c("lon", "lat", "year", "region", "prec", "evap")
  ))
  
  dt <- merge(
    dt,
    cell_area,
    by = c("lon", "lat"),
    all.x = TRUE,
    sort = FALSE
  )
  
  global_yearly <- aggregate_yearly_ts(
    dt = dt,
    group_cols = c("year")
  )[
    ,
    `:=`(scenario = scenario, sim_id = sim_id)
  ]
  setcolorder(global_yearly, c("scenario", "sim_id", "year", "prec", "evap"))
  
  region_yearly <- aggregate_yearly_ts(
    dt = dt,
    group_cols = c("region", "year")
  )[
    ,
    `:=`(scenario = scenario, sim_id = sim_id)
  ]
  setcolorder(region_yearly, c("scenario", "sim_id", "region", "year", "prec", "evap"))
  
  list(global = global_yearly, region = region_yearly)
}

# Analysis =====================================================================

## 1) DATASET AGGREGATION ======================================================

dataset_dt <- as.data.table(copy(prec_evap))[
  ,
  .(lon, lat, year, dataset, prec, evap)
]

# add region and biome from masks
dataset_dt <- merge(
  dataset_dt,
  region_biome_lookup,
  by = c("lon", "lat"),
  all.x = TRUE,
  sort = FALSE
)

# add area
dataset_dt <- merge(
  dataset_dt,
  cell_area,
  by = c("lon", "lat"),
  all.x = TRUE,
  sort = FALSE
)

# checks
if (dataset_dt[, any(is.na(region))]) {
  warning("Some dataset grid cells have NA region after joining masks.")
}
if (dataset_dt[, any(is.na(biome))]) {
  warning("Some dataset grid cells have NA biome after joining masks.")
}
if (dataset_dt[, any(is.na(area))]) {
  warning("Some dataset grid cells have NA area after joining grid_cell_weights.")
}

# dataset global yearly
dataset_global_yearly <- aggregate_yearly_ts(
  dt = dataset_dt,
  group_cols = c("dataset", "year")
)
setcolorder(dataset_global_yearly, c("dataset", "year", "prec", "evap"))

# dataset regional yearly
dataset_region_yearly <- aggregate_yearly_ts(
  dt = dataset_dt,
  group_cols = c("dataset", "region", "year")
)
setcolorder(dataset_region_yearly, c("dataset", "region", "year", "prec", "evap"))

saveRDS(
  dataset_global_yearly,
  file.path(PATH_OUTPUT_DATA, "dataset_global_yearly_prec_evap.Rds")
)

saveRDS(
  dataset_region_yearly,
  file.path(PATH_OUTPUT_DATA, "dataset_region_yearly_prec_evap.Rds")
)

## 1b) DETERMINISTIC TOP1 SCENARIO =============================================

# highest-weight dataset per scenario x region x biome
top1_region_biome <- as.data.table(weights_region_biome)[
  ,
  .SD[which.max(w_region_biome)],
  by = .(scenario, region, biome)
][, .(scenario, region, biome, dataset, w_region_biome)]

# select cells belonging to the chosen top1 dataset in each scenario-region-biome
top1_cells <- merge(
  dataset_dt,
  top1_region_biome,
  by = c("region", "biome", "dataset"),
  all = FALSE,
  allow.cartesian = TRUE,
  sort = FALSE
)

top1_global_yearly <- top1_cells[
  ,
  .(
    prec = weighted_mean_area(prec, area),
    evap = weighted_mean_area(evap, area),
    n_sim = 1L
  ),
  by = .(scenario, year)
][order(scenario, year)]

top1_region_yearly <- top1_cells[
  ,
  .(
    prec = weighted_mean_area(prec, area),
    evap = weighted_mean_area(evap, area),
    n_sim = 1L
  ),
  by = .(scenario, region, year)
][order(scenario, region, year)]

# rename to coexist with soft scenarios
top1_global_yearly[, scenario := paste0(scenario, "_top1")]
top1_region_yearly[, scenario := paste0(scenario, "_top1")]

# dataset_dt no longer needed after top1 is built
rm(top1_cells)
gc()

## 2) MC AGGREGATION ===========================================================
# Recommended version:
#   - parallel over files with foreach + doParallel (PSOCK workers)
#   - safer than mclapply() for arrow::read_parquet()
#   - processes one scenario at a time
#   - saves each scenario immediately to disk
#   - keeps memory lower by not storing all scenarios in RAM
#   - captures and reports failed files

n_workers <- max(1L, parallel::detectCores() - 2L)
batch_size <- 20L

dir.create(file.path(PATH_OUTPUT_DATA, "mc_aggregated"), showWarnings = FALSE, recursive = TRUE)

cl <- parallel::makeCluster(n_workers)
doParallel::registerDoParallel(cl)

parallel::clusterEvalQ(cl, {
  library(data.table)
  library(arrow)
  NULL
})

parallel::clusterExport(
  cl,
  varlist = c(
    "process_mc_file",
    "aggregate_yearly_ts",
    "weighted_mean_area",
    "cell_area"
  ),
  envir = environment()
)

for (sc in scenario_names) {
  message("Scenario: ", sc)
  
  sc_dir <- file.path(mc_root, sc)
  sc_files <- list.files(
    sc_dir,
    pattern = "\\.parquet$",
    full.names = TRUE
  )
  
  if (length(sc_files) == 0L) {
    warning("No parquet files found in: ", sc_dir)
    next
  }
  
  batch_index <- split(
    seq_along(sc_files),
    ceiling(seq_along(sc_files) / batch_size)
  )
  
  global_batches <- vector("list", length(batch_index))
  region_batches <- vector("list", length(batch_index))
  failed_batches <- vector("list", length(batch_index))
  
  for (b in seq_along(batch_index)) {
    idx <- batch_index[[b]]
    message("  batch ", b, " / ", length(batch_index),
            " (files ", min(idx), "-", max(idx), ")")
    
    res_list <- foreach(
      f = sc_files[idx],
      .packages = c("data.table", "arrow"),
      .errorhandling = "pass"
    ) %dopar% {
      tryCatch(
        process_mc_file(
          f = f,
          scenario = sc,
          cell_area = cell_area
        ),
        error = function(e) {
          structure(
            list(
              file = f,
              msg = conditionMessage(e)
            ),
            class = "mc_error"
          )
        }
      )
    }
    
    ok <- vapply(
      res_list,
      function(x) is.list(x) &&
        !inherits(x, "mc_error") &&
        all(c("global", "region") %in% names(x)),
      logical(1)
    )
    
    if (!all(ok)) {
      failed_batches[[b]] <- rbindlist(
        lapply(res_list[!ok], as.data.table),
        fill = TRUE,
        use.names = TRUE
      )
      message("    failed in batch: ", sum(!ok), " / ", length(ok))
    }
    
    global_batches[[b]] <- rbindlist(
      lapply(res_list[ok], `[[`, "global"),
      fill = TRUE,
      use.names = TRUE
    )
    
    region_batches[[b]] <- rbindlist(
      lapply(res_list[ok], `[[`, "region"),
      fill = TRUE,
      use.names = TRUE
    )
    
    rm(res_list)
    gc()
  }
  
  sc_global <- rbindlist(global_batches, fill = TRUE, use.names = TRUE)
  sc_region <- rbindlist(region_batches, fill = TRUE, use.names = TRUE)
  sc_failed <- rbindlist(failed_batches, fill = TRUE, use.names = TRUE)
  
  message("  successful members global: ", uniqueN(sc_global$sim_id))
  message("  successful members region: ", uniqueN(sc_region$sim_id))
  
  saveRDS(
    sc_global,
    file.path(PATH_OUTPUT_DATA, "mc_aggregated", paste0("ensemble_global_yearly_prec_evap_", sc, ".Rds"))
  )
  
  saveRDS(
    sc_region,
    file.path(PATH_OUTPUT_DATA, "mc_aggregated", paste0("ensemble_region_yearly_prec_evap_", sc, ".Rds"))
  )
  
  if (nrow(sc_failed) > 0L) {
    saveRDS(
      sc_failed,
      file.path(PATH_OUTPUT_DATA, "mc_aggregated", paste0("failed_mc_files_", sc, ".Rds"))
    )
    message("  failed members saved: ", nrow(sc_failed))
  }
  
  rm(global_batches, region_batches, failed_batches, sc_global, sc_region, sc_failed)
  gc()
}

parallel::stopCluster(cl)

## Bind saved scenario outputs =================================================

mc_global_yearly_by_sim <- rbindlist(
  lapply(
    scenario_names,
    function(sc) {
      f <- file.path(
        PATH_OUTPUT_DATA,
        "mc_aggregated",
        paste0("ensemble_global_yearly_prec_evap_", sc, ".Rds")
      )
      if (file.exists(f)) readRDS(f) else NULL
    }
  ),
  fill = TRUE,
  use.names = TRUE
)

mc_region_yearly_by_sim <- rbindlist(
  lapply(
    scenario_names,
    function(sc) {
      f <- file.path(
        PATH_OUTPUT_DATA,
        "mc_aggregated",
        paste0("ensemble_region_yearly_prec_evap_", sc, ".Rds")
      )
      if (file.exists(f)) readRDS(f) else NULL
    }
  ),
  fill = TRUE,
  use.names = TRUE
)

saveRDS(
  mc_global_yearly_by_sim,
  file.path(PATH_OUTPUT_DATA, "ensemble_global_yearly_prec_evap.Rds")
)

saveRDS(
  mc_region_yearly_by_sim,
  file.path(PATH_OUTPUT_DATA, "ensemble_region_yearly_prec_evap.Rds")
)

## Optional diagnostics ========================================================

print(
  mc_global_yearly_by_sim[
    ,
    .(n_rows = .N, n_unique_sim = uniqueN(sim_id)),
    by = scenario
  ]
)

print(
  mc_region_yearly_by_sim[
    ,
    .(n_rows = .N, n_unique_sim = uniqueN(sim_id)),
    by = scenario
  ]
)

## 3) SCENARIO-LEVEL MEAN TIME SERIES ==========================================

mc_global_yearly_scenario <- mc_global_yearly_by_sim[
  ,
  .(
    prec = mean(prec, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE),
    n_sim = .N
  ),
  by = .(scenario, year)
]
setcolorder(mc_global_yearly_scenario, c("scenario", "year", "prec", "evap", "n_sim"))

mc_region_yearly_scenario <- mc_region_yearly_by_sim[
  ,
  .(
    prec = mean(prec, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE),
    n_sim = .N
  ),
  by = .(scenario, region, year)
]
setcolorder(mc_region_yearly_scenario, c("scenario", "region", "year", "prec", "evap", "n_sim"))

## 4) APPEND TOP1 INTO SCENARIO OUTPUTS ========================================

mc_plus_top1_global <- rbindlist(
  list(
    mc_global_yearly_scenario[, .(scenario, year, prec, evap, n_sim)],
    top1_global_yearly[, .(scenario, year, prec, evap, n_sim)]
  ),
  use.names = TRUE,
  fill = TRUE
)[order(scenario, year)]

mc_plus_top1_region <- rbindlist(
  list(
    mc_region_yearly_scenario[, .(scenario, region, year, prec, evap, n_sim)],
    top1_region_yearly[, .(scenario, region, year, prec, evap, n_sim)]
  ),
  use.names = TRUE,
  fill = TRUE
)[order(scenario, region, year)]

saveRDS(
  mc_plus_top1_global,
  file.path(PATH_OUTPUT_DATA, "scenario_global_yearly_prec_evap.Rds")
)

saveRDS(
  mc_plus_top1_region,
  file.path(PATH_OUTPUT_DATA, "scenario_region_yearly_prec_evap.Rds")
)

## 5) QUICK PREVIEW ============================================================

print(dataset_global_yearly)
print(dataset_region_yearly)
print(mc_global_yearly_by_sim)
print(mc_region_yearly_by_sim)
print(mc_plus_top1_global)
print(mc_plus_top1_region)

## 6) CLEANUP ==================================================================

rm(dataset_dt)
gc()

data.table::setDTthreads(old_dt_threads)