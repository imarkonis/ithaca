# ============================================================================
# Gridded Monte Carlo ensemble summaries for derived P/E diagnostics
#
# Reads per-member .fst files and computes per grid cell × sim_id:
#   - Sen slope and Mann-Kendall p-value for:
#       availability = P - E
#       flux          = (P + E) / 2
#   - full-period correlations:
#       cor(P, E)
#       cor(P - E, (P + E) / 2)
#   - period-specific correlations:
#       1982-2001
#       2002-2021
#
# Output:
#   member_level_grid_derived_summary.Rds
#   sim_id_reference_derived.Rds
# ============================================================================

library(trend)
library(fst)
library(foreach)
library(doParallel)

source("source/twc_change.R")

# Constants ===================================================================

YEAR_SPLIT <- 2002L
CELL_COLS  <- c("lon", "lat")
N_WORKERS  <- parallel::detectCores(logical = FALSE)
BATCH_SIZE <- 50L

# Helpers =====================================================================

safe_sen <- function(y) {
  y <- as.numeric(y[is.finite(y)])
  if (length(y) < 3L) return(NA_real_)
  out <- try(trend::sens.slope(y), silent = TRUE)
  if (inherits(out, "try-error")) NA_real_ else as.numeric(out$estimates)
}

safe_mk_p <- function(y) {
  y <- as.numeric(y[is.finite(y)])
  if (length(y) < 3L) return(NA_real_)
  out <- try(trend::mk.test(y), silent = TRUE)
  if (inherits(out, "try-error")) NA_real_ else as.numeric(out$p.value)
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  if (sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  cor(x[ok], y[ok])
}

# Per-file worker =============================================================

summarize_grid_member <- function(file_path, scenario, sim_id_orig, year_split) {
  
  dt <- as.data.table(fst::read_fst(file_path))
  
  dt[, `:=`(
    avail  = prec - evap,
    flux   = (prec + evap) / 2,
    period = fifelse(year < year_split, "early", "late")
  )]
  
  setorderv(dt, c("lon", "lat", "year"))
  
  full_stats <- dt[, .(
    avail_sen      = safe_sen(avail),
    avail_mk_p     = safe_mk_p(avail),
    flux_sen       = safe_sen(flux),
    flux_mk_p      = safe_mk_p(flux),
    cor_prec_evap  = safe_cor(prec, evap),
    cor_avail_flux = safe_cor(avail, flux)
  ), by = .(lon, lat)]
  
  early_stats <- dt[period == "early", .(
    cor_prec_evap_1982_2001  = safe_cor(prec, evap),
    cor_avail_flux_1982_2001 = safe_cor(avail, flux)
  ), by = .(lon, lat)]
  
  late_stats <- dt[period == "late", .(
    cor_prec_evap_2002_2021  = safe_cor(prec, evap),
    cor_avail_flux_2002_2021 = safe_cor(avail, flux)
  ), by = .(lon, lat)]
  
  out <- Reduce(
    function(a, b) merge(a, b, by = c("lon", "lat"), all = TRUE),
    list(full_stats, early_stats, late_stats)
  )
  
  out[, `:=`(
    scenario = scenario,
    sim_id_original = sim_id_orig
  )]
  
  out[]
}

# Setup cluster ===============================================================

mc_root <- file.path(PATH_OUTPUT_DATA, "mc_ensemble")

scenario_names <- basename(
  list.dirs(mc_root, recursive = FALSE, full.names = TRUE)
)

cl <- parallel::makeCluster(N_WORKERS)
doParallel::registerDoParallel(cl)
on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

parallel::clusterEvalQ(cl, {
  library(data.table)
  library(fst)
  library(trend)
  data.table::setDTthreads(1L)
  Sys.setenv(
    OMP_NUM_THREADS        = "1",
    OPENBLAS_NUM_THREADS   = "1",
    MKL_NUM_THREADS        = "1",
    VECLIB_MAXIMUM_THREADS = "1"
  )
  NULL
})

parallel::clusterExport(
  cl,
  varlist = c(
    "summarize_grid_member",
    "safe_sen",
    "safe_mk_p",
    "safe_cor",
    "YEAR_SPLIT"
  ),
  envir = environment()
)

# Main loop ===================================================================

grid_list   <- vector("list", length(scenario_names))
failed_list <- vector("list", length(scenario_names))
names(grid_list)   <- scenario_names
names(failed_list) <- scenario_names

for (sc in scenario_names) {
  
  message("Scenario: ", sc)
  
  sc_files <- list.files(
    file.path(mc_root, sc),
    pattern    = "\\.fst$",
    full.names = TRUE
  )
  
  if (length(sc_files) == 0L) {
    warning("No fst files found in: ", file.path(mc_root, sc))
    next
  }
  
  n_files     <- length(sc_files)
  batch_index <- split(seq_len(n_files), ceiling(seq_len(n_files) / BATCH_SIZE))
  n_batches   <- length(batch_index)
  
  sc_grid_batches   <- vector("list", n_batches)
  sc_failed_batches <- vector("list", n_batches)
  
  for (b in seq_len(n_batches)) {
    
    idx <- batch_index[[b]]
    
    message(sprintf(
      "  batch %d / %d  (files %d to %d)  %3d%%",
      b, n_batches, min(idx), max(idx),
      round(100 * max(idx) / n_files)
    ))
    
    res_list <- foreach(
      file_path   = sc_files[idx],
      sim_id_orig = idx,
      .packages   = c("data.table", "fst", "trend"),
      .errorhandling = "pass"
    ) %dopar% {
      tryCatch(
        summarize_grid_member(
          file_path   = file_path,
          scenario    = sc,
          sim_id_orig = sim_id_orig,
          year_split  = YEAR_SPLIT
        ),
        error = function(e) structure(
          list(file = file_path, msg = conditionMessage(e)),
          class = "mc_error"
        )
      )
    }
    
    ok <- vapply(res_list, is.data.table, logical(1))
    
    sc_grid_batches[[b]] <- rbindlist(
      res_list[ok],
      use.names = TRUE,
      fill = TRUE
    )
    
    sc_failed_batches[[b]] <- rbindlist(
      lapply(res_list[!ok], as.data.table),
      use.names = TRUE,
      fill = TRUE
    )
    
    if (nrow(sc_failed_batches[[b]]) > 0L) {
      message("    failed in batch: ", nrow(sc_failed_batches[[b]]))
    }
    
    rm(res_list)
    gc()
  }
  
  grid_list[[sc]] <- rbindlist(
    sc_grid_batches,
    use.names = TRUE,
    fill = TRUE
  )
  
  failed_list[[sc]] <- rbindlist(
    sc_failed_batches,
    use.names = TRUE,
    fill = TRUE
  )
  
  message(sprintf(
    "  successful members: %d  |  failed: %d",
    uniqueN(grid_list[[sc]]$sim_id_original),
    nrow(failed_list[[sc]])
  ))
  
  rm(sc_grid_batches, sc_failed_batches)
  gc()
}

parallel::stopCluster(cl)

# Combine and recode sim_id ===================================================

grid_all <- rbindlist(grid_list, use.names = TRUE, fill = TRUE)

sim_id_ref <- unique(grid_all[, .(scenario, sim_id_original)])
setorderv(sim_id_ref, c("scenario", "sim_id_original"))
sim_id_ref[, sim_id := seq_len(.N)]

grid_all <- merge(
  grid_all,
  sim_id_ref,
  by = c("scenario", "sim_id_original")
)

grid_all[, c("scenario", "sim_id_original") := NULL]

setcolorder(grid_all, c(CELL_COLS, "sim_id"))
setorderv(grid_all, c(CELL_COLS, "sim_id"))

# Save ========================================================================

saveRDS(
  grid_all,
  file.path(PATH_OUTPUT_DATA, "member_level_grid_derived_summary.Rds")
)

saveRDS(
  sim_id_ref,
  file.path(PATH_OUTPUT_DATA, "sim_id_reference_derived.Rds")
)

message(sprintf(
  "Done. %d rows × %d cols  |  %d unique cells  |  %d sim_ids",
  nrow(grid_all),
  ncol(grid_all),
  uniqueN(grid_all[, ..CELL_COLS]),
  uniqueN(grid_all$sim_id)
))