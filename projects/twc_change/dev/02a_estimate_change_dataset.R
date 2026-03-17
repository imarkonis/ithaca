# ============================================================================
# Prepare precipitation, evaporation, and PET change products for twc_change
#
# This script:
# 1. Builds annual precipitation and evaporation pairs for core datasets
# 2. Computes period means for 1982 to 2001 and 2002 to 2021
# 3. Derives precipitation and evaporation change classes
# 4. Processes PET products and exports period summaries and change products
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Helpers =====================================================================

get_period_label <- function(year_vec, split_year) {
  factor(
    ifelse(year_vec < split_year, "1982_2001", "2002_2021"),
    levels = c("1982_2001", "2002_2021"),
    ordered = TRUE
  )
}

build_change_sign <- function(x) {
  fifelse(
    x > 0, "pos",
    fifelse(x < 0, "neg", NA_character_)
  )
}

# Input datasets ===============================================================

prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap.Rds"))

pet_raw <- read_fst(
  file.path(PATH_OUTPUT_RAW, "other/merra2_mswx_pet_mm_1980_2024_yearly.fst"),
  as.data.table = TRUE
)

# Constants & Variables =======================================================

period_split_year <- END_PERIOD_1

# Analysis ====================================================================

prec_evap[, period := get_period_label(year, END_PERIOD_1)]

setcolorder(
  prec_evap,
  c("lon", "lat", "year", "period", "dataset", "prec", "evap")
)


########################### HERE


prec_evap_means <- prec_evap_wide[
  ,
  .(
    prec = mean(prec, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE)
  ),
  by = .(lon, lat, dataset, period)
]

prec_evap_change <- dcast(
  prec_evap_means,
  lon + lat + dataset ~ period,
  value.var = c("prec", "evap")
)

prec_evap_change[
  ,
  `:=`(
    prec_change = prec_aft_2001 - prec_bef_2001,
    evap_change = evap_aft_2001 - evap_bef_2001
  )
]

prec_evap_change[
  ,
  `:=`(
    prec_sign = build_change_sign(prec_change),
    evap_sign = build_change_sign(evap_change)
  )
]

prec_evap_change[
  ,
  prec_evap := fcase(
    prec_sign == "pos" & evap_sign == "pos", "prec_pos-evap_pos",
    prec_sign == "pos" & evap_sign == "neg", "prec_pos-evap_neg",
    prec_sign == "neg" & evap_sign == "pos", "prec_neg-evap_pos",
    prec_sign == "neg" & evap_sign == "neg", "prec_neg-evap_neg",
    default = NA_character_
  )
]

prec_evap_change[
  ,
  `:=`(
    prec_sign = factor(prec_sign, levels = c("neg", "pos")),
    evap_sign = factor(evap_sign, levels = c("neg", "pos")),
    prec_evap = factor(
      prec_evap,
      levels = c(
        "prec_pos-evap_pos",
        "prec_pos-evap_neg",
        "prec_neg-evap_pos",
        "prec_neg-evap_neg"
      )
    )
  )
]

prec_evap_change <- prec_evap_change[complete.cases(prec_evap_change)]

saveRDS(
  prec_evap_change,
  file = file.path(PATH_OUTPUT_DATA, "prec_evap_change.Rds")
)

# PET ========================================================================

pet <- copy(pet_raw)

pet <- pet[date <= END_PERIOD_2]
pet[, year := year(date)]
pet[, period := get_period_label(year, period_split_year)]

setnames(
  pet,
  old = c("x", "y", "source"),
  new = c("lon", "lat", "dataset")
)

pet[, date := NULL]

pet <- melt(
  pet,
  id.vars = c("lon", "lat", "year", "dataset", "period"),
  variable.name = "method",
  value.name = "value"
)

saveRDS(
  pet,
  file = file.path(PATH_OUTPUT_DATA, "pet.Rds")
)

pet_mean <- pet[
  ,
  .(value = mean(value, na.rm = TRUE)),
  by = .(lon, lat, dataset, method)
]

saveRDS(
  pet_mean,
  file = file.path(PATH_OUTPUT_DATA, "pet_mean.Rds")
)

pet_ensemble_periods <- pet[
  ,
  .(pet = mean(value, na.rm = TRUE)),
  by = .(lon, lat, period, dataset, method)
]

saveRDS(
  pet_ensemble_periods,
  file = file.path(PATH_OUTPUT_DATA, "pet_ensemble_periods.Rds")
)

pet_stats <- pet[
  ,
  .(
    pet_mean = round(mean(value, na.rm = TRUE), 1),
    pet_min = min(value, na.rm = TRUE),
    pet_max = max(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ),
  by = .(lon, lat, period)
]

pet_stats <- pet_stats[n > 200]

pet_mean_wide <- dcast(
  pet_stats,
  lon + lat ~ period,
  value.var = "pet_mean"
)

pet_min_wide <- dcast(
  pet_stats,
  lon + lat ~ period,
  value.var = "pet_min"
)

pet_max_wide <- dcast(
  pet_stats,
  lon + lat ~ period,
  value.var = "pet_max"
)

setnames(
  pet_mean_wide,
  old = c("bef_2001", "aft_2001"),
  new = c("pet_mean_bef_2001", "pet_mean_aft_2001")
)

setnames(
  pet_min_wide,
  old = c("bef_2001", "aft_2001"),
  new = c("pet_min_bef_2001", "pet_min_aft_2001")
)

setnames(
  pet_max_wide,
  old = c("bef_2001", "aft_2001"),
  new = c("pet_max_bef_2001", "pet_max_aft_2001")
)

pet_final <- Reduce(
  f = function(x, y) merge(x, y, by = c("lon", "lat"), all = TRUE),
  x = list(pet_mean_wide, pet_min_wide, pet_max_wide)
)

pet_final[
  ,
  pet_change := pet_mean_aft_2001 - pet_mean_bef_2001
]

setcolorder(
  pet_final,
  c(
    "lon", "lat",
    "pet_min_bef_2001", "pet_mean_bef_2001", "pet_max_bef_2001",
    "pet_min_aft_2001", "pet_mean_aft_2001", "pet_max_aft_2001",
    "pet_change"
  )
)

# Outputs =====================================================================

saveRDS(
  prec_evap_wide,
  file = file.path(PATH_OUTPUT_DATA, "prec_evap.Rds")
)

saveRDS(
  pet_final,
  file = file.path(PATH_OUTPUT_DATA, "pet_change.Rds")
)

# Validate ====================================================================

pet_final[
  ,
  rel_range := round(
    (pet_max_bef_2001 - pet_min_bef_2001) / pet_mean_bef_2001,
    2
  )
]

ggplot(pet_final) +
  geom_point(aes(x = lon, y = lat, colour = rel_range))

pet_final[, rel_range := NULL]