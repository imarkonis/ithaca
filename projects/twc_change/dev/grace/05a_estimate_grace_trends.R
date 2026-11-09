# ============================================================================
# Estimate GRACE regional slopes after IPCC-region aggregation
#
# This script:
# 1. Aggregates GRACE values to annual regional time series
# 2. Estimates regional GRACE trends using:
#    a) Sen's slope
#    b) Mann-Kendall significance test
# 3. Flags regions with statistically significant GRACE trends
# 4. Saves:
#    a) annual regional GRACE time series
#    b) regional GRACE slope and significance summaries
# ============================================================================

# Libraries ==================================================================

source("source/twc_change.R")

library(trend)

# Inputs ======================================================================

grace_yearly <- readRDS(file.path(PATH_OUTPUT_RAW_OTHER, "grace_yearly.Rds"))
twc_grid_classes <- readRDS(file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds"))

# Constants & Variables =======================================================

P_THRES <- 0.05

# Functions ==================================================================

estimate_slope <- function(dt) {
  
  dt <- dt[!is.na(grace_mean)]
  dt <- dt[order(year)]
  
  if (nrow(dt) < 5) {
    return(data.table(
      slope = NA_real_,
      mk_p_value = NA_real_,
      n_years = nrow(dt),
      year_start = ifelse(nrow(dt) > 0, min(dt$year), NA_integer_),
      year_end = ifelse(nrow(dt) > 0, max(dt$year), NA_integer_)
    ))
  }
  
  sen <- sens.slope(dt$grace_mean)
  mk <- mk.test(dt$grace_mean)
  
  data.table(
    slope = as.numeric(sen$estimates),
    mk_p_value = mk$p.value
  )
}

# Analysis ===================================================================

region_meta <- twc_grid_classes[, .(lon, lat, region)]

grace_region_grid <- merge(
  grace_yearly,
  region_meta,
  by = c("lon", "lat"),
  all.x = FALSE,
  all.y = FALSE
)

## Regional annual aggregation ================================================

grace_region_yearly <- grace_region_grid[
  !is.na(value), .(grace_median = median(value, na.rm = TRUE)),
  by = .(region, year)
]

setorder(grace_region_yearly, region, year)

## Slope estimation ===========================================================

grace_region_slopes <- grace_region_yearly[, estimate_slope(.SD), by = region]
grace_region_slopes[, significant := mk_p_value < P_THRES]
setorder(grace_region_slopes, region)

# Outputs =====================================================================

saveRDS(
  grace_region_yearly,
  file.path(PATH_OUTPUT_DATA, "grace_region_yearly.Rds")
)

saveRDS(
  grace_region_slopes,
  file.path(PATH_OUTPUT_DATA, "grace_region_slopes.Rds")
)

# Validation ==================================================================

print(grace_region_slopes)

ggplot(data = grace_region_yearly) +
  geom_line(aes(x = year, y = grace_mean, col = region)) +
  theme(legend.position = "none")
