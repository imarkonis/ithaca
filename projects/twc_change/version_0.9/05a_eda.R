source('source/twc_change.R')

library(dplyr)
library(scales)
        
prec_evap_change <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'prec_evap_change.Rds'))
pet_change <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'pet_change.Rds'))
avail_flux_change <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))

prec_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_ensemble_stats.Rds'))
evap_ensemble_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'evap_ensemble_stats.Rds'))

# ============================================================
# Discrete (binned) maps for prec_change & evap_change
# - fixed range: [-100, 100]
# - extra bins for < -100 and > 100
# - uses your trans_cols (9 discrete colors)
# ============================================================

# --- your 9-color palette (kept exactly) ---
trans_cols <- c(
  "e-e" = "#08306B",
  "w-e" = "#2171B5",
  "w-u" = "#6BAED6",
  "u-e" = "#9ECAE1",  # cold group
  "u-u" = "#BDBDBD",  # neutral
  "u-w" = "#F4A582",
  "e-u" = "#EF8A62",
  "e-w" = "#D6604D",
  "w-w" = "#B2182B"
)

# ---- prep data ----
pe_df <- as.data.table(prec_evap_change)[
  , .(lon, lat, dataset, prec_change, evap_change)
] %>%
  as_tibble() %>%
  mutate(dataset = factor(dataset))

# ---- binning helper: 9 classes mapped to your 9 palette keys ----
# Breaks: (-Inf, -100], (-100,-50], (-50,-20], (-20,-5], (-5,5], (5,20], (20,50], (50,100], (100, Inf)
bin_change <- function(x) {
  cut(
    x,
    breaks = c(-Inf, -100, -50, -20, -5, 5, 20, 50, 100, Inf),
    labels = c("e-e","w-e","w-u","u-e","u-u","u-w","e-u","e-w","w-w"),
    include.lowest = TRUE,
    right = TRUE
  )
}

pe_df <- pe_df %>%
  mutate(
    prec_bin = bin_change(prec_change),
    evap_bin = bin_change(evap_change)
  )

# ============================================================
# 1) GLOBAL maps per dataset
# ============================================================

my_plot <- ggplot(pe_df, aes(lon, lat, fill = prec_bin)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ dataset, ncol = 3, scales = "fixed") +
  scale_fill_manual(
    values = trans_cols,
    drop = FALSE,
    name = "ΔP (mm)",
    labels = c(
      "≤ -100", "(-100,-50]", "(-50,-20]", "(-20,-5]",
      "(-5,5]", "(5,20]", "(20,50]", "(50,100]", "> 100"
    )
  ) +
  labs(title = "Precipitation change (after − before 2001)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")
save_plot(my_plot, "prec_change_dataset_global.png", w = 13, h = 8)

my_plot <- ggplot(pe_df, aes(lon, lat, fill = evap_bin)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ dataset, ncol = 3, scales = "fixed") +
  scale_fill_manual(
    values = trans_cols,
    drop = FALSE,
    name = "ΔE (mm)",
    labels = c(
      "≤ -100", "(-100,-50]", "(-50,-20]", "(-20,-5]",
      "(-5,5]", "(5,20]", "(20,50]", "(50,100]", "> 100"
    )
  ) +
  labs(title = "Evaporation change (after − before 2001)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

save_plot(my_plot, "evap_change_dataset_global.png", w = 13, h = 8)

# ============================================================
# 2) Tropics + High lat: 10 maps (5 rows x 2 cols), facet_wrap, scales="free"
# ============================================================

make_band_facets_binned <- function(df, bin_col, title_txt, legend_name) {
  
  band_df <- bind_rows(
    df %>% filter(lat >= -30, lat <= 0)  %>% mutate(band = "Tropics (-30 to 0)"),
    df %>% filter(lat >=  40, lat <= 60) %>% mutate(band = "High lat (40 to 60)")
  ) %>%
    mutate(
      band = factor(band, levels = c("Tropics (-30 to 0)", "High lat (40 to 60)")),
      # enforce facet order: each dataset row, then band column
      dataset = factor(dataset, levels = c("ERA5L","FLDAS","MERRA","GLEAM","TERRA")),
      facet = factor(
        paste(dataset, band, sep = " | "),
        levels = as.vector(t(outer(
          levels(dataset),
          levels(band),
          paste, sep = " | "
        )))
      )
    )
  
  ggplot(band_df, aes(lon, lat, fill = .data[[bin_col]])) +
    geom_raster() +
    coord_quickmap(expand = FALSE) +
    facet_wrap(~ facet, ncol = 2, scales = "free_y") +
    scale_fill_manual(
      values = trans_cols,
      drop = FALSE,
      name = legend_name,
      labels = c(
        "≤ -100", "(-100,-50]", "(-50,-20]", "(-20,-5]",
        "(-5,5]", "(5,20]", "(20,50]", "(50,100]", "> 100"
      )
    ) +
    labs(title = title_txt, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

make_band_facets_binned(pe_df, "prec_bin",
                        "Precipitation change bins by latitude band (dataset × band)",
                        "ΔP bin")

make_band_facets_binned(pe_df, "evap_bin",
                        "Evaporation change bins by latitude band (dataset × band)",
                        "ΔE bin")



# ============================================================
# Maps for avail_change & flux_change classified into 4 classes
# (Global maps per dataset + Tropics/High-lat 10 maps via facet_wrap)
# Input: avail_flux_change (data.table with lon, lat, dataset, avail_change, flux_change)
# Uses your PALETTES$water_cycle_change for colors
# ============================================================


# ---- 1) Prep ----
af_df <- as.data.table(avail_flux_change)[
  , .(lon, lat, dataset, avail_change, flux_change)
] %>%
  as_tibble() %>%
  mutate(dataset = factor(dataset))

# ---- 2) Classify into 4 conditions (same logic you wrote) ----
cond_levels <- c("Wetter - Accelerated",
                 "Wetter - Deccelerated",
                 "Drier - Accelerated",
                 "Drier - Deccelerated")

af_df <- af_df %>%
  mutate(
    Conditions = factor("Unknown", levels = c("Unknown", cond_levels)),
    Conditions = case_when(
      flux_change > 0 & avail_change > 0 ~ "Wetter - Accelerated",
      flux_change > 0 & avail_change < 0 ~ "Drier - Accelerated",
      flux_change < 0 & avail_change > 0 ~ "Wetter - Deccelerated",
      flux_change < 0 & avail_change < 0 ~ "Drier - Deccelerated",
      TRUE ~ "Unknown"
    ),
    Conditions = factor(Conditions, levels = cond_levels)  # drop "Unknown" from legend ordering
  )

# ---- 3) Colors (expects a named vector) ----
# Example expected structure:
# PALETTES$water_cycle_change <- c(
#   "Wetter - Accelerated"  = "...",
#   "Wetter - Deccelerated" = "...",
#   "Drier - Accelerated"   = "...",
#   "Drier - Deccelerated"  = "..."
# )
cond_cols <- PALETTES$water_cycle_change

# ============================================================
# A) GLOBAL maps per dataset
# ============================================================

ggplot(af_df, aes(lon, lat, fill = Conditions)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ dataset, ncol = 3, scales = "fixed") +
  scale_fill_manual(values = cond_cols, drop = FALSE) +
  labs(
    title = "Water-cycle change regime (avail vs flux)",
    subtitle = "Classes from sign(avail_change) × sign(flux_change)",
    x = NULL, y = NULL, fill = "Conditions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ============================================================
# B) Tropics + High lat: 10 maps (5 rows x 2 cols) via facet_wrap, scales="free"
# ============================================================

band_df <- bind_rows(
  af_df %>% filter(lat >= -30, lat <= 0)  %>% mutate(band = "Tropics (-30 to 0)"),
  af_df %>% filter(lat >=  40, lat <= 60) %>% mutate(band = "High lat (40 to 60)")
) %>%
  mutate(
    band = factor(band, levels = c("Tropics (-30 to 0)", "High lat (40 to 60)")),
    dataset = factor(dataset, levels = c("ERA5L","FLDAS","MERRA","GLEAM","TERRA")), # edit if needed
    facet = factor(
      paste(dataset, band, sep = " | "),
      levels = as.vector(t(outer(
        levels(dataset),
        levels(band),
        paste, sep = " | "
      )))
    )
  )

ggplot(band_df, aes(lon, lat, fill = Conditions)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ facet, ncol = 2, scales = "free") +
  scale_fill_manual(values = cond_cols, drop = FALSE) +
  labs(
    title = "Water-cycle change regime by latitude band (dataset × band)",
    x = NULL, y = NULL, fill = "Conditions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )






# ============================================================
# FIGURE SET 1 & 2 (global maps) from prec_evap_change
# 1) Quadrant map (WA/WD/DA/DD) with agreement mask (≥4/5)
# 2) Availability driver attribution:
#    - D_S = |ΔAET| / (|ΔP|+|ΔAET|)
#    - sign-structure map (compound drying / AET-led / P-led / etc.)
# ============================================================

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# -----------------------------
# Inputs assumed to exist:
#   prec_evap_change (data.table): lon, lat, dataset, prec_change, evap_change
#   PALETTES$water_cycle_change (named colors for the 4 quadrant classes)
# -----------------------------

# ---- 0) Prep base table ----
pe <- as.data.table(prec_evap_change)[
  , .(lon, lat, dataset, dP = prec_change, dAET = evap_change)
][
  , `:=`(
    dS = dP - dAET,                 # ΔS = Δ(P-AET)
    dF = 0.5 * (dP + dAET)          # ΔF = Δ((P+AET)/2)
  )
]

# Restrict to your 5 datasets if needed (edit list)
use_ds <- c("ERA5L","FLDAS","MERRA","GLEAM","TERRA")
pe <- pe[dataset %in% use_ds]
pe[, dataset := factor(dataset, levels = use_ds)]

# ============================================================
# 1) Quadrant (Conditions) + Agreement masks (sign + class)
# ============================================================

# Per-dataset quadrant class from signs of ΔS and ΔF
cond_levels <- c("Wetter - Accelerated",
                 "Wetter - Deccelerated",
                 "Drier - Accelerated",
                 "Drier - Deccelerated")

pe[, Conditions := fifelse(dF > 0 & dS > 0, "Wetter - Accelerated",
                           fifelse(dF < 0 & dS > 0, "Wetter - Deccelerated",
                                   fifelse(dF > 0 & dS < 0, "Drier - Accelerated",
                                           fifelse(dF < 0 & dS < 0, "Drier - Deccelerated", NA_character_))))]

pe[, Conditions := factor(Conditions, levels = cond_levels)]

# ---- Agreement: (a) sign agreement for ΔS and ΔF, (b) quadrant agreement ----
# Sign agreement: count datasets with sign>0 vs <0, take max
agree_dt <- pe[!is.na(Conditions),
               .(
                 n_ds = .N,
                 agree_sign_dS = pmax(sum(dS > 0), sum(dS < 0)),
                 agree_sign_dF = pmax(sum(dF > 0), sum(dF < 0)),
                 # quadrant agreement: majority count of the same class
                 agree_class = max(tabulate(match(Conditions, cond_levels), nbins = length(cond_levels))),
                 # majority (mode) class label
                 class_mode = {
                   tt <- table(Conditions)
                   names(tt)[which.max(tt)]
                 }
               ),
               by = .(lon, lat)
]

agree_dt[, `:=`(
  robust_sign  = (agree_sign_dS >= 4 & agree_sign_dF >= 4),
  robust_class = (agree_class >= 4),
  class_mode   = factor(class_mode, levels = cond_levels)
)]

agree_dt[, robust_both := robust_sign & robust_class]


# ---- Colors for the 4-class quadrant map ----
cond_cols <- PALETTES$water_cycle_change
# Ensure it is named by the same labels as cond_levels
# Example expected:
# cond_cols <- c("Wetter - Accelerated"="...", "Wetter - Deccelerated"="...",
#                "Drier - Accelerated"="...", "Drier - Deccelerated"="...")

# ---- Figure 1A: Majority quadrant map, greyed where NOT robust (≥4/5) ----
p_quadrant_majority <- ggplot(agree_dt, aes(lon, lat, fill = class_mode, alpha = robust_both)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  scale_fill_manual(values = cond_cols, drop = FALSE, name = "Quadrant (mode)") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.25), guide = "none") +
  labs(
    title = "Quadrant map from ΔS=Δ(P−AET) and ΔF=Δ((P+AET)/2)",
    subtitle = "Color = majority class across datasets; transparency = robust agreement (≥4/5 for sign of ΔS and ΔF AND for class)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

# ---- Figure 1B (optional): Per-dataset quadrant maps (no agreement) ----
p_quadrant_by_ds <- ggplot(pe, aes(lon, lat, fill = Conditions)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ dataset, ncol = 3, scales = "fixed") +
  scale_fill_manual(values = cond_cols, drop = FALSE, name = "Quadrant") +
  labs(
    title = "Quadrant class per dataset",
    subtitle = "ΔS = Δ(P−AET), ΔF = Δ((P+AET)/2)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"),
        legend.position = "bottom")

# Print (or save) Fig 1
p_quadrant_majority
# p_quadrant_by_ds

# ============================================================
# 2) Dominant-driver attribution for availability
#    D_S = |ΔAET| / (|ΔP| + |ΔAET|)
#    plus sign-structure categories
# ============================================================

pe[, D_S := abs(dAET) / (abs(dP) + abs(dAET))]
pe[is.nan(D_S), D_S := NA_real_]   # handle 0/0

# ---- Sign-structure categories for ΔS interpretation ----
# (You can tune thresholds if you want; this is the strict sign logic.)
pe[, S_driver_class := fifelse(dS < 0 & dP < 0 & dAET > 0, "Compound drying (ΔP<0, ΔAET>0)",
                               fifelse(dS < 0 & dP >= 0 & dAET > 0, "AET-led drying (ΔP≥0, ΔAET>0)",
                                       fifelse(dS < 0 & dP < 0 & dAET <= 0, "P-led drying (ΔP<0, ΔAET≤0)",
                                               fifelse(dS > 0 & dP > 0 & dAET < 0, "Compound wetting (ΔP>0, ΔAET<0)",
                                                       fifelse(dS > 0 & dP <= 0 & dAET < 0, "AET-led wetting (ΔP≤0, ΔAET<0)",
                                                               fifelse(dS > 0 & dP > 0 & dAET >= 0, "P-led wetting (ΔP>0, ΔAET≥0)",
                                                                       NA_character_))))))]

driver_levels <- c(
  "Compound drying (ΔP<0, ΔAET>0)",
  "AET-led drying (ΔP≥0, ΔAET>0)",
  "P-led drying (ΔP<0, ΔAET≤0)",
  "Compound wetting (ΔP>0, ΔAET<0)",
  "AET-led wetting (ΔP≤0, ΔAET<0)",
  "P-led wetting (ΔP>0, ΔAET≥0)"
)
pe[, S_driver_class := factor(S_driver_class, levels = driver_levels)]

# ---- Figure 2A: D_S maps per dataset (0=P-dominated, 1=AET-dominated) ----
p_DS_by_ds <- ggplot(pe, aes(lon, lat, fill = D_S)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ dataset, ncol = 3, scales = "fixed") +
  scale_fill_gradient(limits = c(0, 1), oob = squish, labels = percent, name = expression(D[S])) +
  labs(
    title = expression("Dominant-driver index for " * Delta * "S = " * Delta * "(P-AET)"),
    subtitle = expression(D[S] == frac("|"*Delta*"AET|","|"*Delta*"P|"+"|"*Delta*"AET|") ~ " (1=AET-dominated, 0=P-dominated)"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"),
        legend.position = "bottom")

# ---- Figure 2B: Driver sign-structure maps per dataset (categorical) ----
p_driverclass_by_ds <- ggplot(pe, aes(lon, lat, fill = S_driver_class)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ dataset, ncol = 3, scales = "fixed") +
  labs(
    title = expression("Sign-structure attribution for " * Delta * "S = " * Delta * "(P-AET)"),
    subtitle = "Classifies whether ΔS changes are P-led, AET-led, or compound (by sign patterns)",
    x = NULL, y = NULL, fill = "Attribution"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"),
        legend.position = "bottom")

# Print (or save) Fig 2
p_DS_by_ds
p_driverclass_by_ds

# ============================================================
# OPTIONAL: Add robustness mask to Fig 2 using agree_dt$robust_both
# (e.g., grey out non-robust for ΔS & ΔF/class)
# ============================================================
pe_rob <- merge(pe, agree_dt[, .(lon, lat, robust_both)], by = c("lon","lat"), all.x = TRUE)


