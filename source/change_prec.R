## packages
# generic
library(data.table)

# plotting
library(ggplot2)
library(ggpubr)
library(rnaturalearth)
library(scales)
library(sf)
library(stars)

# geospatial
library(pRecipe)
library(raster)

# parallel
library(doParallel)

## Paths
PATH_DATA <- "~/shared/data/"
PATH_DATA_REVIEW <- "~/shared/data_review/"
PATH_SAVE <- "~/shared/data_projects/ithaca/"
PATH_PREC_SIM <- paste0(PATH_DATA, "sim/precip/raw/")
PATH_PREC_OBS <- paste0(PATH_DATA, "obs/precip/raw/")

## Datasets
PREC_GLOBAL_DATASETS <- c("20cr", "chirps", "cmap", "cmorph", "cpc",
                          "cru-ts", "era20c", "era5", "era5-land", "em-earth",
                          "fldas", "ghcn", "gldas-clsm", "gldas-noah",
                          "gldas-vic", "gpcc", "gpcp", "gpm-imerg", "gsmap",
                          "jra55", "merra2", "mswep", "ncep-doe", "ncep-ncar",
                          "persiann", "precl", "terraclimate", "trmm-3b43",
                          "udel")

PREC_REPS <- c("cpc-global", "era5-land", "gpcp-v1-3", "gpm-imerg-v7",
               "jra-3q", "merra-2", "mswep-v2-8", "ncep-doe")

# Types
PREC_DATASETS_OBS <- c("cpc", "cru-ts", "em-earth", "ghcn", "gpcc", "precl",
                       "udel")
PREC_DATASETS_REANAL <- c("20cr", "era20c", "era5", "era5-land", "jra55",
                          "merra2", "ncep-doe", "ncep-ncar")
PREC_DATASETS_REMOTE <- c("chirps", "cmap", "cmorph","gpcp", "gpm-imerg",
                          "gsmap", "mswep", "persiann", "trmm-3b43")
PREC_DATASETS_HYDROL <- c("fldas", "gldas-clsm", "gldas-noah",
                          "gldas-vic", "terraclimate")

### Output
PATH_SAVE_CHANGE_PREC <- paste0(PATH_SAVE, "change_prec/")
PATH_SAVE_CHANGE_PREC_RAW <- paste0(PATH_SAVE, "change_prec/raw/")
PATH_SAVE_CHANGE_PREC_SPATIAL <- paste0(PATH_SAVE,
                                             "change_prec/spatial/")
PATH_SAVE_CHANGE_PREC_FIGURES <- paste0(PATH_SAVE,
                                             "change_prec/figures/")
PATH_SAVE_CHANGE_PREC_TABLES <- paste0(PATH_SAVE,
                                            "change_prec/tables/")
PATH_SAVE_CHANGE_PREC_TEMP <- paste0(PATH_SAVE,
                                       "change_prec/temp/")

dir.create(PATH_SAVE_CHANGE_PREC, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGE_PREC_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGE_PREC_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGE_PREC_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGE_PREC_TABLES, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGE_PREC_TEMP, showWarnings = FALSE)

# Functions
# ============================================================
# Bivariate colour utilities for any two variables:
# u-axis and v-axis
# Compatible with data.frame and data.table
# ============================================================

#-------------------------------------------------------------
# 1) Internal helper: rescale around zero to [-1, 1]
#-------------------------------------------------------------
rescale_mid_signed <- function(x, limits, squish = TRUE) {
  if (length(limits) != 2 || any(!is.finite(limits))) {
    stop("`limits` must be a numeric vector of length 2 with finite values.")
  }
  if (limits[1] >= limits[2]) {
    stop("`limits[1]` must be smaller than `limits[2]`.")
  }
  
  out <- scales::rescale_mid(x, to = c(-1, 1), mid = 0, from = limits)
  
  if (squish) {
    out <- pmax(-1, pmin(1, out))
  }
  
  out
}

#-------------------------------------------------------------
# 2) Internal helper: interpolate between two colours
#-------------------------------------------------------------
mix_two_cols <- function(col1, col2, t, space = "Lab") {
  t <- pmax(0, pmin(1, t))
  ramp <- grDevices::colorRamp(c(col1, col2), space = space)
  rgb_mat <- ramp(t)
  grDevices::rgb(rgb_mat[, 1], rgb_mat[, 2], rgb_mat[, 3], maxColorValue = 255)
}

#-------------------------------------------------------------
# 3) Main vectorized colour engine
#-------------------------------------------------------------
bivariate_colour_plane <- function(
    u, v,
    u_limits = NULL,
    v_limits = NULL,
    col_uu_vv = "#08306B",  # +u, +v
    col_uu_vn = "#4292C6",  # +u, -v
    col_0  = "#ffffbf",     #  0,  0
    col_un_vn = "#F4A261",  # -u, -v
    col_un_vv = "#D94801",  # -u, +v
    space = "Lab",
    squish = TRUE,
    na_colour = NA_character_
) {
  if (length(u) != length(v)) {
    stop("`u` and `v` must have the same length.")
  }
  
  if (is.null(u_limits)) {
    umax <- max(abs(u), na.rm = TRUE)
    u_limits <- c(-umax, umax)
  }
  if (is.null(v_limits)) {
    vmax <- max(abs(v), na.rm = TRUE)
    v_limits <- c(-vmax, vmax)
  }
  
  u_std <- rescale_mid_signed(u, u_limits, squish = squish)
  v_std <- rescale_mid_signed(v, v_limits, squish = squish)
  
  out <- rep(na_colour, length(u))
  
  ok <- is.finite(u_std) & is.finite(v_std)
  if (!any(ok)) return(out)
  
  amp <- sqrt(u_std^2 + v_std^2) / sqrt(2)
  amp <- pmin(amp, 1)
  
  idx_uu_vv <- ok & u_std >= 0 & v_std >= 0
  idx_uu_vn <- ok & u_std >= 0 & v_std <  0
  idx_un_vn <- ok & u_std <  0 & v_std <  0
  idx_un_vv <- ok & u_std <  0 & v_std >= 0
  
  out[idx_uu_vv] <- mix_two_cols(col_0, col_uu_vv, amp[idx_uu_vv], space = space)
  out[idx_uu_vn] <- mix_two_cols(col_0, col_uu_vn, amp[idx_uu_vn], space = space)
  out[idx_un_vn] <- mix_two_cols(col_0, col_un_vn, amp[idx_un_vn], space = space)
  out[idx_un_vv] <- mix_two_cols(col_0, col_un_vv, amp[idx_un_vv], space = space)
  
  out
}

#-------------------------------------------------------------
# 4) Add colour column to a data.frame or data.table
#-------------------------------------------------------------
add_bivariate_colour <- function(
    data,
    u,
    v,
    u_limits = NULL,
    v_limits = NULL,
    output = "fill_col",
    col_uu_vv = "#08306B",
    col_uu_vn = "#4292C6",
    col_0  = "#ffffbf",
    col_un_vn = "#F4A261",
    col_un_vv = "#D94801",
    space = "Lab",
    squish = TRUE,
    na_colour = NA_character_,
    copy_data = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or data.table.")
  }
  if (!is.character(u) || length(u) != 1 || !u %in% names(data)) {
    stop("`u` must be a single existing column name.")
  }
  if (!is.character(v) || length(v) != 1 || !v %in% names(data)) {
    stop("`v` must be a single existing column name.")
  }
  if (!is.character(output) || length(output) != 1) {
    stop("`output` must be a single column name.")
  }
  
  out_data <- if (copy_data) data else data
  
  out_data[[output]] <- bivariate_colour_plane(
    u = out_data[[u]],
    v = out_data[[v]],
    u_limits = u_limits,
    v_limits = v_limits,
    col_uu_vv = col_uu_vv,
    col_uu_vn = col_uu_vn,
    col_0 = col_0,
    col_un_vn = col_un_vn,
    col_un_vv = col_un_vv,
    space = space,
    squish = squish,
    na_colour = na_colour
  )
  
  out_data
}

#-------------------------------------------------------------
# 5) Build a reusable 2D legend as a ggplot object
#-------------------------------------------------------------
bivariate_legend_ggplot <- function(
    u_limits,
    v_limits,
    u_lab = "u-axis",
    v_lab = "v-axis",
    n = 200,
    col_uu_vv = "#08306B",
    col_uu_vn = "#4292C6",
    col_0  = "#ffffbf",
    col_un_vn = "#F4A261",
    col_un_vv = "#D94801",
    space = "Lab",
    squish = TRUE
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required.")
  }
  
  leg_df <- expand.grid(
    u = seq(u_limits[1], u_limits[2], length.out = n),
    v = seq(v_limits[1], v_limits[2], length.out = n)
  )
  
  leg_df$fill_col <- bivariate_colour_plane(
    u = leg_df$u,
    v = leg_df$v,
    u_limits = u_limits,
    v_limits = v_limits,
    col_uu_vv = col_uu_vv,
    col_uu_vn = col_uu_vn,
    col_0 = col_0,
    col_un_vn = col_un_vn,
    col_un_vv = col_un_vv,
    space = space,
    squish = squish
  )
  
  ggplot2::ggplot(leg_df, ggplot2::aes(x = u, y = v, fill = fill_col)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(x = u_lab, y = v_lab) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = "grey70"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      axis.ticks = ggplot2::element_line(colour = "grey40")
    )
}

## Constants
#Graphs
GOLDEN_RATIO <- (1 + sqrt(5))/2

# Time
PERIOD_START <- as.Date("1998-01-01")
PERIOD_END <- as.Date("2025-12-31")

# Data
MIN_N_DATASETS <- 6

## Variable names
PREC_NAME <- "prec"

PREC_NAME_SHORT <- "tp"

## Parallelization
N_CORES <- 56

## Other
M2_TO_KM2 <- 10 ^ (-6)
MM_TO_M <- 10 ^ (-3)
MM_TO_KM <- 10 ^ (-6)
