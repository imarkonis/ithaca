# ============================================================================
# Aggregate annual weighted precipitation and evaporation time series
#
# This script:
# 1. Aggregates original datasets to global and regional yearly series
# 2. Builds deterministic top1 scenario series from highest weight choices
# 3. Aggregates Monte Carlo ensemble members saved as fst files
# 4. Computes scenario mean yearly series from Monte Carlo members
# 5. Appends top1 scenario series to the scenario outputs
# ============================================================================

# Libraries ===================================================================

library(data.table)
library(fst)
library(parallel)
library(foreach)
library(doParallel)

source("source/twc_change.R")

# Inputs ======================================================================

prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap.Rds"))

weights_region_biome <- readRDS(
  file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Constants & Variables ========================================================

mc_root <- file.path(PATH_OUTPUT_DATA, "mc_ensemble")

old_dt_threads <- data.table::getDTthreads()

data.table::setDTthreads(1)

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1"
)

N_WORKERS <- max(1L, min(8L, parallel::detectCores() - 2L))
BATCH_SIZE <- 50L

# Helpers =====================================================================

grid_lookup <- unique(
  as.data.table(twc_grid_classes)[
    ,
    .(lon, lat, region, biome, area)
  ]
)
setkey(grid_lookup, lon, lat)

if (grid_lookup[, any(!is.finite(area) | is.na(area))]) {
  stop("twc_grid_classes must contain finite 'area' values.")
}

weighted_mean_area <- function(x, area) {
  ok <- is.finite(x) & !is.na(x) & is.finite(area) & !is.na(area)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
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

get_sim_id <- function(path) {
  as.integer(
    sub(
      ".*_sim_([0-9]+)$",
      "\\1",
      tools::file_path_sans_ext(basename(path))
    )
  )
}

process_mc_file <- function(file_path, scenario, grid_lookup) {
  sim_id <- get_sim_id(file_path)
  
  dt <- fst::read_fst(
    path = file_path,
    columns = c("lon", "lat", "year", "region", "prec", "evap"),
    as.data.table = TRUE
  )
  
  dt <- merge(
    dt,
    grid_lookup[, .(lon, lat, area)],
    by = c("lon", "lat"),
    all.x = TRUE,
    sort = FALSE
  )
  
  if (dt[, any(!is.finite(area) | is.na(area))]) {
    stop(sprintf(
      "Missing area after joining twc_grid_classes for file: %s",
      basename(file_path)
    ))
  }
  
  global_yearly <- aggregate_yearly_ts(
    dt = dt,
    group_cols = "year"
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

# Analysis ====================================================================

## Dataset aggregation =========================================================

dataset_dt <- merge(
  as.data.table(prec_evap)[, .(lon, lat, year, dataset, prec, evap)],
  grid_lookup,
  by = c("lon", "lat"),
  all = FALSE,
  sort = FALSE
)

dataset_global_yearly <- aggregate_yearly_ts(
  dt = dataset_dt,
  group_cols = c("dataset", "year")
)[order(dataset, year)]

dataset_region_yearly <- aggregate_yearly_ts(
  dt = dataset_dt,
  group_cols = c("dataset", "region", "year")
)[order(dataset, region, year)]

## Deterministic top performer scenario ========================================

top1_region_biome <- as.data.table(weights_region_biome)[
  is.finite(w_region_biome) & !is.na(w_region_biome),
  .SD[which.max(w_region_biome)],
  by = .(scenario, region, biome)
][
  ,
  .(scenario, region, biome, dataset)
]

top1_dt <- merge(
  dataset_dt,
  top1_region_biome,
  by = c("region", "biome", "dataset"),
  all = FALSE,
  allow.cartesian = TRUE,
  sort = FALSE
)

top1_global_yearly <- top1_dt[
  ,
  .(
    prec = weighted_mean_area(prec, area),
    evap = weighted_mean_area(evap, area),
    n_sim = 1L
  ),
  by = .(scenario, year)
][order(scenario, year)]

top1_region_yearly <- top1_dt[
  ,
  .(
    prec = weighted_mean_area(prec, area),
    evap = weighted_mean_area(evap, area),
    n_sim = 1L
  ),
  by = .(scenario, region, year)
][order(scenario, region, year)]

top1_global_yearly[, scenario := paste0(scenario, "_top1")]
top1_region_yearly[, scenario := paste0(scenario, "_top1")]

rm(top1_dt)
gc()

## Monte Carlo aggregation =====================================================

scenario_names <- basename(
  list.dirs(mc_root, recursive = FALSE, full.names = TRUE)
)

mc_global_list <- vector("list", length(scenario_names))
mc_region_list <- vector("list", length(scenario_names))
failed_list <- vector("list", length(scenario_names))

names(mc_global_list) <- scenario_names
names(mc_region_list) <- scenario_names
names(failed_list) <- scenario_names

cl <- parallel::makeCluster(N_WORKERS)
doParallel::registerDoParallel(cl)

on.exit({
  try(parallel::stopCluster(cl), silent = TRUE)
  data.table::setDTthreads(old_dt_threads)
}, add = TRUE)

parallel::clusterEvalQ(cl, {
  library(data.table)
  library(fst)
  
  data.table::setDTthreads(1)
  
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1"
  )
  
  NULL
})

parallel::clusterExport(
  cl,
  varlist = c(
    "grid_lookup",
    "weighted_mean_area",
    "aggregate_yearly_ts",
    "get_sim_id",
    "process_mc_file"
  ),
  envir = environment()
)

for (sc in scenario_names) {
  message("Scenario: ", sc)
  
  sc_files <- list.files(
    file.path(mc_root, sc),
    pattern = "\\.fst$",
    full.names = TRUE
  )
  
  if (length(sc_files) == 0L) {
    warning("No fst files found in: ", file.path(mc_root, sc))
    next
  }
  
  batch_index <- split(
    seq_along(sc_files),
    ceiling(seq_along(sc_files) / BATCH_SIZE)
  )
  
  global_batches <- vector("list", length(batch_index))
  region_batches <- vector("list", length(batch_index))
  failed_batches <- vector("list", length(batch_index))
  
  for (b in seq_along(batch_index)) {
    idx <- batch_index[[b]]
    
    message(
      "  batch ", b, " / ", length(batch_index),
      " (files ", min(idx), " to ", max(idx), ")"
    )
    
    res_list <- foreach(
      file_path = sc_files[idx],
      .packages = c("data.table", "fst"),
      .errorhandling = "pass"
    ) %dopar% {
      tryCatch(
        process_mc_file(
          file_path = file_path,
          scenario = sc,
          grid_lookup = grid_lookup
        ),
        error = function(e) {
          structure(
            list(
              file = file_path,
              msg = conditionMessage(e)
            ),
            class = "mc_error"
          )
        }
      )
    }
    
    ok <- vapply(
      res_list,
      function(x) {
        is.list(x) &&
          !inherits(x, "mc_error") &&
          all(c("global", "region") %in% names(x))
      },
      logical(1)
    )
    
    global_batches[[b]] <- rbindlist(
      lapply(res_list[ok], `[[`, "global"),
      use.names = TRUE,
      fill = TRUE
    )
    
    region_batches[[b]] <- rbindlist(
      lapply(res_list[ok], `[[`, "region"),
      use.names = TRUE,
      fill = TRUE
    )
    
    failed_batches[[b]] <- rbindlist(
      lapply(res_list[!ok], as.data.table),
      use.names = TRUE,
      fill = TRUE
    )
    
    if (nrow(failed_batches[[b]]) > 0L) {
      message("    failed in batch: ", nrow(failed_batches[[b]]))
    }
    
    rm(res_list)
    gc()
  }
  
  mc_global_list[[sc]] <- rbindlist(
    global_batches,
    use.names = TRUE,
    fill = TRUE
  )
  
  mc_region_list[[sc]] <- rbindlist(
    region_batches,
    use.names = TRUE,
    fill = TRUE
  )
  
  failed_list[[sc]] <- rbindlist(
    failed_batches,
    use.names = TRUE,
    fill = TRUE
  )
  
  message("  successful global members: ", uniqueN(mc_global_list[[sc]]$sim_id))
  message("  successful region members: ", uniqueN(mc_region_list[[sc]]$sim_id))
  
  if (nrow(failed_list[[sc]]) > 0L) {
    message("  failed members: ", nrow(failed_list[[sc]]))
  }
  
  rm(global_batches, region_batches, failed_batches)
  gc()
}

parallel::stopCluster(cl)

mc_global_yearly_by_sim <- rbindlist(
  mc_global_list,
  use.names = TRUE,
  fill = TRUE
)

mc_region_yearly_by_sim <- rbindlist(
  mc_region_list,
  use.names = TRUE,
  fill = TRUE
)

mc_failed <- rbindlist(
  failed_list,
  use.names = TRUE,
  fill = TRUE,
  idcol = "scenario"
)

## Scenario mean yearly series =================================================

mc_global_yearly_scenario <- mc_global_yearly_by_sim[
  ,
  .(
    prec = mean(prec, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE),
    n_sim = uniqueN(sim_id)
  ),
  by = .(scenario, year)
]

mc_region_yearly_scenario <- mc_region_yearly_by_sim[
  ,
  .(
    prec = mean(prec, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE),
    n_sim = uniqueN(sim_id)
  ),
  by = .(scenario, region, year)
]

## Append top1 to scenario outputs =============================================

scenario_global_yearly <- rbindlist(
  list(
    mc_global_yearly_scenario[, .(scenario, year, prec, evap, n_sim)],
    top1_global_yearly[, .(scenario, year, prec, evap, n_sim)]
  ),
  use.names = TRUE,
  fill = TRUE
)[order(scenario, year)]

scenario_region_yearly <- rbindlist(
  list(
    mc_region_yearly_scenario[, .(scenario, region, year, prec, evap, n_sim)],
    top1_region_yearly[, .(scenario, region, year, prec, evap, n_sim)]
  ),
  use.names = TRUE,
  fill = TRUE
)[order(scenario, region, year)]

# Outputs =====================================================================

saveRDS(
  dataset_global_yearly,
  file.path(PATH_OUTPUT_DATA, "dataset_global_yearly_prec_evap.Rds")
)

saveRDS(
  dataset_region_yearly,
  file.path(PATH_OUTPUT_DATA, "dataset_region_yearly_prec_evap.Rds")
)

saveRDS(
  mc_global_yearly_by_sim,
  file.path(PATH_OUTPUT_DATA, "mc_global_yearly_prec_evap.Rds")
)

saveRDS(
  mc_region_yearly_by_sim,
  file.path(PATH_OUTPUT_DATA, "mc_region_yearly_prec_evap.Rds")
)

if (nrow(mc_failed) > 0L) {
  saveRDS(
    mc_failed,
    file.path(PATH_OUTPUT_DATA, "mc_failed_files.Rds")
  )
}

saveRDS(
  scenario_global_yearly,
  file.path(PATH_OUTPUT_DATA, "scenario_global_yearly_prec_evap.Rds")
)

saveRDS(
  scenario_region_yearly,
  file.path(PATH_OUTPUT_DATA, "scenario_region_yearly_prec_evap.Rds")
)

# Validation ==================================================================

print(
  mc_global_yearly_by_sim[
    ,
    .(n_rows = .N, n_unique_sim = uniqueN(sim_id)),
    by = scenario
  ][order(scenario)]
)

print(
  mc_region_yearly_by_sim[
    ,
    .(n_rows = .N, n_unique_sim = uniqueN(sim_id)),
    by = scenario
  ][order(scenario)]
)

if (nrow(mc_failed) > 0L) {
  print(mc_failed)
}

# Cleanup =====================================================================

rm(dataset_dt, mc_global_list, mc_region_list, failed_list)
gc()

data.table::setDTthreads(old_dt_threads)