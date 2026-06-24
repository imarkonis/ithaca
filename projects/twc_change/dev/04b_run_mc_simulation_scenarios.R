# ============================================================================
# Generate Monte Carlo annual precipitation and evaporation ensembles
#
# This script:
# 1. Creates shared random draws per simulation x IPCC region x biome
# 2. Converts scenario weights to cumulative probability intervals
# 3. Selects one dataset per simulation x scenario x region x biome
# 4. Joins selections to existing area-weighted dataset values
# 5. Aggregates to region x biome, IPCC region, and global annual P and E values
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

weights_dt <- readRDS(
  file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds")
)

dataset_region_biome_year <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_biome_year.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Constants & Variables =======================================================

set.seed(1979)

N_SIMS <- 100L

# Functions ===================================================================

get_cell_area_column <- function(dt) {
  
  possible_area_cols <- c(
    "cell_area",
    "area",
    "area_km2",
    "area_m2",
    "area_sum"
  )
  
  area_cols <- intersect(possible_area_cols, names(dt))
  
  if (length(area_cols) == 0L) {
    return(NA_character_)
  }
  
  area_cols[1]
}

prepare_region_biome_area <- function(twc_grid_classes) {
  
  grid_dt <- as.data.table(copy(twc_grid_classes))
  
  required_cols <- c("lon", "lat", "region", "biome")
  missing_cols <- setdiff(required_cols, names(grid_dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "twc_grid_classes is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  area_col <- get_cell_area_column(grid_dt)
  
  if (is.na(area_col)) {
    
    message(
      "No explicit cell-area column found. ",
      "Using cos(latitude) as relative area weight."
    )
    
    grid_dt[
      ,
      cell_area := cos(lat * pi / 180)
    ]
    
  } else {
    
    message("Using existing area column: ", area_col)
    
    if (area_col != "cell_area") {
      setnames(
        grid_dt,
        old = area_col,
        new = "cell_area"
      )
    }
  }
  
  grid_dt[
    ,
    `:=`(
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  grid_dt <- grid_dt[
    is.finite(lon) &
      is.finite(lat) &
      !is.na(region) &
      !is.na(biome) &
      is.finite(cell_area) &
      cell_area > 0,
    .(lon, lat, region, biome, cell_area)
  ]
  
  grid_dt <- unique(
    grid_dt,
    by = c("lon", "lat")
  )
  
  grid_dt[
    ,
    .(
      area_weight = sum(cell_area, na.rm = TRUE)
    ),
    by = .(region, biome)
  ]
}

make_weight_cdf <- function(weights_dt) {
  
  weights_region <- as.data.table(copy(weights_dt))
  
  required_cols <- c(
    "scenario",
    "region",
    "biome",
    "dataset",
    "w_region_biome"
  )
  
  missing_cols <- setdiff(required_cols, names(weights_region))
  
  if (length(missing_cols) > 0L) {
    stop(
      "weights_dt is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  weights_region <- weights_region[
    is.finite(w_region_biome) &
      !is.na(w_region_biome) &
      w_region_biome > 0,
    .(scenario, region, biome, dataset, w_region_biome)
  ]
  
  weights_region[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      dataset = as.character(dataset)
    )
  ]
  
  weights_region[
    ,
    w_region_biome := w_region_biome / sum(w_region_biome),
    by = .(scenario, region, biome)
  ]
  
  weight_check <- weights_region[
    ,
    .(
      w_sum = sum(w_region_biome),
      n_datasets = .N
    ),
    by = .(scenario, region, biome)
  ]
  
  bad_weights <- weight_check[
    abs(w_sum - 1) > 1e-10
  ]
  
  if (nrow(bad_weights) > 0L) {
    print(bad_weights)
    stop("Some scenario x region x biome weights do not sum to 1.")
  }
  
  setorder(
    weights_region,
    scenario,
    region,
    biome,
    dataset
  )
  
  weight_cdf <- weights_region[
    ,
    {
      p_high <- cumsum(w_region_biome)
      p_high[.N] <- 1
      
      p_low <- c(0, p_high[-.N])
      
      .(
        dataset = dataset,
        w_region_biome = w_region_biome,
        p_low = p_low,
        p_high = p_high
      )
    },
    by = .(scenario, region, biome)
  ]
  
  weight_cdf
}

make_mc_random <- function(weight_cdf, n_sims) {
  
  region_biome_draws <- unique(
    weight_cdf[, .(region, biome)]
  )
  
  mc_random <- region_biome_draws[
    ,
    .(sim = seq_len(n_sims)),
    by = .(region, biome)
  ]
  
  mc_random[
    ,
    u := runif(.N)
  ]
  
  setkey(
    mc_random,
    region,
    biome,
    sim
  )
  
  mc_random
}

make_mc_selection <- function(weight_cdf, mc_random) {
  
  weight_cdf <- as.data.table(copy(weight_cdf))
  mc_random <- as.data.table(copy(mc_random))
  
  weight_cdf[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      dataset = as.character(dataset)
    )
  ]
  
  mc_random[
    ,
    `:=`(
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  mc_selection <- weight_cdf[
    mc_random,
    on = .(region, biome),
    allow.cartesian = TRUE
  ][
    u >= p_low & u < p_high,
    .(
      sim,
      scenario,
      region,
      biome,
      dataset,
      u,
      w_region_biome,
      p_low,
      p_high
    )
  ]
  
  scenario_region_biome <- unique(
    weight_cdf[, .(scenario, region, biome)]
  )
  
  sim_values <- unique(mc_random$sim)
  
  expected_selection <- scenario_region_biome[
    ,
    .(sim = sim_values),
    by = .(scenario, region, biome)
  ][
    ,
    .(sim, scenario, region, biome)
  ]
  
  selection_count <- mc_selection[
    ,
    .N,
    by = .(sim, scenario, region, biome)
  ]
  
  selection_check <- merge(
    expected_selection,
    selection_count,
    by = c("sim", "scenario", "region", "biome"),
    all.x = TRUE
  )
  
  selection_check[
    is.na(N),
    N := 0L
  ]
  
  bad_selection <- selection_check[N != 1L]
  
  if (nrow(bad_selection) > 0L) {
    print(bad_selection)
    stop(
      "Some sim x scenario x region x biome combinations do not have exactly one selected dataset."
    )
  }
  
  setkey(
    mc_selection,
    sim,
    scenario,
    region,
    biome
  )
  
  mc_selection
}

make_mc_region_biome_year <- function(dataset_region_biome_year, mc_selection) {
  
  dt <- as.data.table(copy(dataset_region_biome_year))
  
  required_cols <- c(
    "dataset",
    "region",
    "biome",
    "year",
    "prec",
    "evap"
  )
  
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "dataset_region_biome_year is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  dt[
    ,
    `:=`(
      dataset = as.character(dataset),
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  aggregate_keys <- unique(
    dt[, .(dataset, region, biome)]
  )
  
  missing_selected_values <- mc_selection[
    !aggregate_keys,
    on = .(dataset, region, biome)
  ]
  
  if (nrow(missing_selected_values) > 0L) {
    print(missing_selected_values)
    stop(
      "Some selected dataset x region x biome combinations are missing from the aggregated data."
    )
  }
  
  out <- dt[
    mc_selection,
    on = .(dataset, region, biome),
    allow.cartesian = TRUE,
    nomatch = 0L
  ][
    ,
    .(
      sim,
      scenario,
      region,
      biome,
      dataset,
      year,
      prec,
      evap
    )
  ]
  
  setkey(
    out,
    sim,
    scenario,
    region,
    biome,
    year
  )
  
  out
}

make_mc_region_year <- function(mc_region_biome_year, region_biome_area) {
  
  dt <- region_biome_area[
    copy(mc_region_biome_year),
    on = .(region, biome),
    nomatch = 0L
  ]
  
  out <- dt[
    ,
    .(
      prec = sum(prec * area_weight, na.rm = TRUE) /
        sum(area_weight[is.finite(prec)], na.rm = TRUE),
      evap = sum(evap * area_weight, na.rm = TRUE) /
        sum(area_weight[is.finite(evap)], na.rm = TRUE)
    ),
    by = .(sim, scenario, region, year)
  ]
  
  setkey(
    out,
    sim,
    scenario,
    region,
    year
  )
  
  out
}

make_mc_global_year <- function(mc_region_biome_year, region_biome_area) {
  
  dt <- region_biome_area[
    copy(mc_region_biome_year),
    on = .(region, biome),
    nomatch = 0L
  ]
  
  out <- dt[
    ,
    .(
      prec = sum(prec * area_weight, na.rm = TRUE) /
        sum(area_weight[is.finite(prec)], na.rm = TRUE),
      evap = sum(evap * area_weight, na.rm = TRUE) /
        sum(area_weight[is.finite(evap)], na.rm = TRUE)
    ),
    by = .(sim, scenario, year)
  ]
  
  setkey(
    out,
    sim,
    scenario,
    year
  )
  
  out
}

# Analysis ====================================================================

region_biome_area <- prepare_region_biome_area(
  twc_grid_classes = twc_grid_classes
)

weight_cdf <- make_weight_cdf(
  weights_dt = weights_dt
)

mc_random <- make_mc_random(
  weight_cdf = weight_cdf,
  n_sims = N_SIMS
)

mc_selection <- make_mc_selection(
  weight_cdf = weight_cdf,
  mc_random = mc_random
)

mc_selection_lean <- mc_selection[
  ,
  .(
    sim,
    scenario,
    region,
    biome,
    dataset,
    u
  )
]

selection_matrix <- dcast(
  mc_selection_lean,
  sim + region + biome + u ~ scenario,
  value.var = "dataset"
)

mc_region_biome_year <- make_mc_region_biome_year(
  dataset_region_biome_year = dataset_region_biome_year,
  mc_selection = mc_selection_lean
)

mc_region_year <- make_mc_region_year(
  mc_region_biome_year = mc_region_biome_year,
  region_biome_area = region_biome_area
)

mc_global_year <- make_mc_global_year(
  mc_region_biome_year = mc_region_biome_year,
  region_biome_area = region_biome_area
)

# Outputs =====================================================================

saveRDS(
  mc_random,
  file.path(PATH_OUTPUT_DATA, "mc_random_region_biome.Rds")
)

saveRDS(
  mc_selection_lean,
  file.path(PATH_OUTPUT_DATA, "mc_selection.Rds")
)

saveRDS(
  selection_matrix,
  file.path(PATH_OUTPUT_DATA, "mc_selection_matrix.Rds")
)

saveRDS(
  mc_region_biome_year,
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_year.Rds")
)

saveRDS(
  mc_region_year,
  file.path(PATH_OUTPUT_DATA, "mc_region_year.Rds")
)

saveRDS(
  mc_global_year,
  file.path(PATH_OUTPUT_DATA, "mc_global_year.Rds")
)

# Validation ==================================================================

cat("\nSaved Monte Carlo ensemble outputs to:\n")
cat(OUT_DIR, "\n")

cat("\nWeight CDF check:\n")
print(
  weight_cdf[
    ,
    .(
      first_low = min(p_low),
      last_high = max(p_high),
      n_datasets = .N
    ),
    by = .(scenario, region, biome)
  ][
    abs(first_low - 0) > 1e-12 |
      abs(last_high - 1) > 1e-12
  ]
)

cat("\nSelection count check:\n")
print(
  mc_selection_lean[
    ,
    .N,
    by = .(sim, scenario, region, biome)
  ][
    N != 1L
  ]
)

cat("\nNumber of simulations:\n")
print(mc_selection_lean[, uniqueN(sim)])

cat("\nNumber of scenarios:\n")
print(mc_selection_lean[, uniqueN(scenario)])

cat("\nSelection preview:\n")
print(
  mc_selection_lean[
    order(sim, region, biome, scenario)
  ][
    1:50
  ]
)

cat("\nSelection matrix preview:\n")
print(
  selection_matrix[
    1:50
  ]
)

cat("\nRegion x biome annual output structure:\n")
str(mc_region_biome_year)

cat("\nIPCC region annual output structure:\n")
str(mc_region_year)

cat("\nGlobal annual output structure:\n")
str(mc_global_year)

cat("\nGlobal annual output preview:\n")
print(
  mc_global_year[
    order(sim, scenario, year)
  ][
    1:50
  ]
)

cat("\nGlobal first and last year by scenario for sim 1:\n")

global_year_range <- range(
  mc_global_year$year,
  na.rm = TRUE
)

print(
  mc_global_year[
    sim == 1L &
      year %in% global_year_range,
    .(
      sim,
      scenario,
      year,
      prec,
      evap
    )
  ][
    order(scenario, year)
  ]
)

cat("\nMissing values in region x biome annual output:\n")
print(
  mc_region_biome_year[
    ,
    .(
      n_missing_prec = sum(!is.finite(prec)),
      n_missing_evap = sum(!is.finite(evap))
    )
  ]
)

cat("\nMissing values in IPCC region annual output:\n")
print(
  mc_region_year[
    ,
    .(
      n_missing_prec = sum(!is.finite(prec)),
      n_missing_evap = sum(!is.finite(evap))
    )
  ]
)

cat("\nMissing values in global annual output:\n")
print(
  mc_global_year[
    ,
    .(
      n_missing_prec = sum(!is.finite(prec)),
      n_missing_evap = sum(!is.finite(evap))
    )
  ]
)

cat("\nFinished Monte Carlo ensemble generation.\n")