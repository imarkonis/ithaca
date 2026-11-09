# ============================================================================
# Gridded Monte Carlo ensemble summaries for P and E
#
# Reads per-member .fst files (same structure as mc pipeline), computes
# per grid cell × sim_id:
#   - overall mean and SD (full period)
#   - period means (1982–2001, 2002–2021)
#   - Sen slope and Mann-Kendall p-value (full series)
#
# Output:
#   member_level_grid_summary.Rds  — lon, lat, sim_id + metrics (no scenario)
#   sim_id_reference.Rds           — sim_id → scenario + original sim_id
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

# Per-file worker -------------------------------------------------------------

summarize_grid_member <- function(file_path, scenario, sim_id_orig, year_split) {
  
  dt <- as.data.table(fst::read_fst(file_path))
  
  dt[, period := fifelse(year < year_split, "early", "late")]
  
  full_stats <- dt[, .(
    prec_mean = mean(prec, na.rm = TRUE),
    prec_sd   = sd(prec,   na.rm = TRUE),
    evap_mean = mean(evap, na.rm = TRUE),
    evap_sd   = sd(evap,   na.rm = TRUE)
  ), by = .(lon, lat)]
  
  early <- dt[period == "early", .(
    prec_mean_1982_2001 = mean(prec, na.rm = TRUE),
    evap_mean_1982_2001 = mean(evap, na.rm = TRUE)
  ), by = .(lon, lat)]
  
  late <- dt[period == "late", .(
    prec_mean_2002_2021 = mean(prec, na.rm = TRUE),
    evap_mean_2002_2021 = mean(evap, na.rm = TRUE)
  ), by = .(lon, lat)]
  
  setorderv(dt, c("lon", "lat", "year"))
  trend_stats <- dt[, .(
    prec_sen  = safe_sen(prec),
    prec_mk_p = safe_mk_p(prec),
    evap_sen  = safe_sen(evap),
    evap_mk_p = safe_mk_p(evap)
  ), by = .(lon, lat)]
  
  out <- Reduce(
    function(a, b) merge(a, b, by = c("lon", "lat")),
    list(full_stats, early, late, trend_stats)
  )
  
  out[, `:=`(scenario = scenario, sim_id_original = sim_id_orig)]
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
  varlist = c("summarize_grid_member", "safe_sen", "safe_mk_p", "YEAR_SPLIT"),
  envir   = environment()
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
      "  batch %d / %d  (files %d–%d)  %3d%%",
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
      res_list[ok], use.names = TRUE, fill = TRUE
    )
    sc_failed_batches[[b]] <- rbindlist(
      lapply(res_list[!ok], as.data.table), use.names = TRUE, fill = TRUE
    )
    
    if (nrow(sc_failed_batches[[b]]) > 0L)
      message("    failed in batch: ", nrow(sc_failed_batches[[b]]))
    
    rm(res_list); gc()
  }
  
  grid_list[[sc]]   <- rbindlist(sc_grid_batches,   use.names = TRUE, fill = TRUE)
  failed_list[[sc]] <- rbindlist(sc_failed_batches, use.names = TRUE, fill = TRUE)
  
  message(sprintf("  successful members: %d  |  failed: %d",
                  uniqueN(grid_list[[sc]]$sim_id_original),
                  nrow(failed_list[[sc]])))
  
  rm(sc_grid_batches, sc_failed_batches); gc()
}

parallel::stopCluster(cl)

# Combine and recode sim_id ===================================================

grid_all <- rbindlist(grid_list, use.names = TRUE, fill = TRUE)

sim_id_ref <- unique(grid_all[, .(scenario, sim_id_original)])
setorderv(sim_id_ref, c("scenario", "sim_id_original"))
sim_id_ref[, sim_id := seq_len(.N)]

grid_all <- merge(grid_all, sim_id_ref, by = c("scenario", "sim_id_original"))
grid_all[, c("scenario", "sim_id_original") := NULL]

setcolorder(grid_all, c(CELL_COLS, "sim_id"))
setorderv(grid_all, c(CELL_COLS, "sim_id"))

# Save ========================================================================

saveRDS(grid_all,   file.path(PATH_OUTPUT_DATA, "member_level_grid_summary.Rds"))
saveRDS(sim_id_ref, file.path(PATH_OUTPUT_DATA, "sim_id_reference.Rds"))

message(sprintf(
  "Done. %d rows × %d cols  |  %d unique cells  |  %d sim_ids",
  nrow(grid_all),
  ncol(grid_all),
  uniqueN(grid_all[, ..CELL_COLS]),
  uniqueN(grid_all$sim_id)
))