
source("source/twc_change.R")

# Inputs ======================================================================

mc_region_slopes_2002_2021 <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_slopes_2002_2021.Rds")
)

grace_region_slopes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "grace_region_slopes.Rds")
)

mc_region_slopes_2002_2021 <- as.data.table(mc_region_slopes_2002_2021)
grace_region_slopes <- as.data.table(grace_region_slopes)

# Constants & Variables =======================================================

P_THRES <- 0.05

twc_grace_palette <- c(
  "Wet + Accel" = "#B2182B",
  "Wet + Decel" = "#EF8A62",
  "Dry + Accel" = "#67A9CF",
  "Dry + Decel" = "#2166AC",
  "Weak / mixed" = "grey85"
)

# Functions ===================================================================

safe_sign <- function(x) {
  fifelse(x > 0, 1L, fifelse(x < 0, -1L, 0L))
}

# Analysis ====================================================================

## Prepare GRACE comparison table ---------------------------------------------

grace_compare <- grace_region_slopes[
  ,
  .(
    region,
    grace_slope = slope,
    grace_mk_p = mk_p_value,
    grace_sig_95 = mk_p_value <= P_THRES,
    grace_sign = safe_sign(slope)
  )
]

## Join Monte Carlo slopes with GRACE -----------------------------------------

mc_grace <- merge(
  mc_region_slopes_2002_2021[
    ,
    .(
      scenario,
      sim_id,
      region,
      avail_slope,
      avail_mk_p,
      flux_slope,
      flux_mk_p
    )
  ],
  grace_compare,
  by = "region"
)

mc_grace[
  ,
  `:=`(
    avail_sign = safe_sign(avail_slope),
    flux_sign = safe_sign(flux_slope),
    avail_sig_95 = avail_mk_p <= P_THRES,
    flux_sig_95 = flux_mk_p <= P_THRES
  )
]

## Region-level classification -------------------------------------------------

mc_region_grace_class <- mc_grace[
  ,
  .(
    grace_slope = first(grace_slope),
    grace_mk_p = first(grace_mk_p),
    grace_sig_95 = first(grace_sig_95),
    
    avail_slope_mean = mean(avail_slope, na.rm = TRUE),
    flux_slope_mean = mean(flux_slope, na.rm = TRUE),
    
    avail_grace_agreement = mean(avail_sign == grace_sign, na.rm = TRUE),
    flux_grace_agreement = mean(flux_sign == grace_sign, na.rm = TRUE),
    
    wet_fraction = mean(avail_slope > 0, na.rm = TRUE),
    accel_fraction = mean(flux_slope > 0, na.rm = TRUE),
    
    wet_sig_fraction = mean(avail_slope > 0 & avail_sig_95, na.rm = TRUE),
    accel_sig_fraction = mean(flux_slope > 0 & flux_sig_95, na.rm = TRUE)
  ),
  by = region
]

mc_region_grace_class[
  ,
  grace_flux_class := fcase(
    grace_slope > 0 & flux_slope_mean > 0, "Wet + Accel",
    grace_slope > 0 & flux_slope_mean < 0, "Wet + Decel",
    grace_slope < 0 & flux_slope_mean > 0, "Dry + Accel",
    grace_slope < 0 & flux_slope_mean < 0, "Dry + Decel",
    default = "Weak / mixed"
  )
]

mc_region_grace_class[
  ,
  grace_flux_class := factor(
    grace_flux_class,
    levels = names(twc_grace_palette)
  )
]

# Outputs =====================================================================

saveRDS(
  mc_region_grace_class,
  file.path(PATH_OUTPUT_DATA, "mc_region_grace_flux_classes_2002_2021.Rds")
)

# Validation ==================================================================

## Hex map: joint GRACE and acceleration class ---------------------------------

build_region_hex_map(
  mc_region_grace_class[
    ,
    .(
      region,
      value = grace_flux_class
    )
  ],
  color_scale = scale_fill_manual(
    values = twc_grace_palette,
    na.value = "grey90",
    name = NULL
  ),
  title = "GRACE wetting/drying and flux acceleration/deceleration"
)

## Hex map: availability agreement with GRACE ----------------------------------

build_region_hex_map(
  mc_region_grace_class[
    ,
    .(
      region,
      value = avail_grace_agreement
    )
  ],
  title = "Availability agreement with GRACE"
)

## Scatter: GRACE slope vs flux slope, availability agreement as size ----------

ggplot(mc_region_grace_class) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_point(
    aes(
      x = grace_slope,
      y = flux_slope_mean,
      fill = grace_flux_class,
      size = avail_grace_agreement
    ),
    shape = 21,
    colour = "grey25",
    alpha = 0.9
  ) +
  ggrepel::geom_text_repel(
    aes(
      x = grace_slope,
      y = flux_slope_mean,
      label = region
    ),
    size = 2.6,
    max.overlaps = Inf
  ) +
  scale_fill_manual(
    values = twc_grace_palette,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(2, 6),
    limits = c(0, 1),
    name = "P − E agreement"
  ) +
  labs(
    x = "GRACE TWS slope",
    y = "Mean flux slope, (P + E) / 2",
    title = "How regional acceleration relates to GRACE wetting/drying"
  ) +
  theme_bw()