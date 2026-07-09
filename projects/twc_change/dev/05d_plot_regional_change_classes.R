# ============================================================================
# Plot IPCC hexagon maps for regional TWC storyline likelihood classes
# ============================================================================

source("source/twc_change.R")

# ============================================================================
# Inputs
# ============================================================================

region_storyline_clean <- readRDS(
  file.path(PATH_OUTPUT_DATA, "region_storyline_clean.Rds")
)

ipcc_hexagon <- data.table(
  read.csv("/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv")
)

ipcc_hexagon <- ipcc_hexagon[
  Acronym %in% region_storyline_clean$region
]

# ============================================================================
# Constants & Variables
# ============================================================================

COL_NO_CHANGE <- "#D9D9D9"

  
# Acceleration
COL_ACC_1 <- "#EDE7F6"
COL_ACC_2 <- "#C5CAE9"
COL_ACC_3 <- "#9FA8DA"
      
COL_DEC_1 <- "#F6E8C3"
COL_DEC_2 <- "#E6B89C"
COL_DEC_3 <- "#C97B63"
          
        # Availability
        COL_WET_1 <- "#D7EEEA"
          COL_WET_2 <- "#9FD3CC"
            COL_WET_3 <- "#5AB4AC"
              
            COL_DRY_1 <- "#F2E3B6"
              COL_DRY_2 <- "#DFC27D"
                COL_DRY_3 <- "#C9A64B"
                  
                # Compound
                COL_WA_1 <- "#C8DDF1"
                  COL_WA_2 <- "#7FB0DD"
                    COL_WA_3 <- "#4C78A8"
                      
                    COL_WD_1 <- "#CBEAE6"
                      COL_WD_2 <- "#8FD0C7"
                        COL_WD_3 <- "#4EAFA6"
                          
                        COL_DA_1 <- "#E39A9A"
                          COL_DA_2 <- "#D7301F"
                            COL_DA_3 <- "#990000"
                              
                            COL_DD_1 <- "#F7C97C"
                              COL_DD_2 <- "#F39C12"
                                COL_DD_3 <- "#E67E22"

accel_cols <- c(
  "accelerating_confident"   = COL_ACC_3,
  "accelerating_most_likely" = COL_ACC_2,
  "accelerating_likely"      = COL_ACC_1,
  "decelerating_confident"   = COL_DEC_3,
  "decelerating_most_likely" = COL_DEC_2,
  "decelerating_likely"      = COL_DEC_1,
  "no_change"                = COL_NO_CHANGE
)

avail_cols <- c(
  "wetter_confident"      = COL_WET_3,
  "wetter_most_likely"    = COL_WET_2,
  "wetter_likely"         = COL_WET_1,
  "drier_confident"       = COL_DRY_3,
  "drier_most_likely"     = COL_DRY_2,
  "drier_likely"          = COL_DRY_1,
  "no_change"             = COL_NO_CHANGE
)

compound_cols <- c(
  "wetter-accelerated_confident"   = COL_WA_3,
  "wetter-accelerated_most_likely" = COL_WA_2,
  "wetter-accelerated_likely"      = COL_WA_1,
  "wetter-decelerated_confident"   = COL_WD_3,
  "wetter-decelerated_most_likely" = COL_WD_2,
  "wetter-decelerated_likely"      = COL_WD_1,
  "drier-accelerated_confident"    = COL_DA_3,
  "drier-accelerated_most_likely"  = COL_DA_2,
  "drier-accelerated_likely"       = COL_DA_1,
  "drier-decelerated_confident"    = COL_DD_3,
  "drier-decelerated_most_likely"  = COL_DD_2,
  "drier-decelerated_likely"       = COL_DD_1,
  "no_change"                      = COL_NO_CHANGE
)

accel_breaks <- c(
  "accelerating_confident",
  "accelerating_most_likely",
  "accelerating_likely",
  "decelerating_confident",
  "decelerating_most_likely",
  "decelerating_likely",
  "no_change"
)

avail_breaks <- c(
  "wetter_confident",
  "wetter_most_likely",
  "wetter_likely",
  "drier_confident",
  "drier_most_likely",
  "drier_likely",
  "no_change"
)

compound_breaks <- c(
  "wetter-accelerated_confident",
  "wetter-accelerated_most_likely",
  "wetter-accelerated_likely",
  "wetter-decelerated_confident",
  "wetter-decelerated_most_likely",
  "wetter-decelerated_likely",
  "drier-accelerated_confident",
  "drier-accelerated_most_likely",
  "drier-accelerated_likely",
  "drier-decelerated_confident",
  "drier-decelerated_most_likely",
  "drier-decelerated_likely",
  "no_change"
)

accel_labels <- c(
  "accelerating_confident"   = "Accel. (⬣⬣⬣)",
  "accelerating_most_likely" = "Accel. (⬣⬣⬡)",
  "accelerating_likely"      = "Accel. (⬣⬡⬡)",
  "decelerating_confident"   = "Decel. (⬣⬣⬣)",
  "decelerating_most_likely" = "Decel. (⬣⬣⬡)",
  "decelerating_likely"      = "Decel. (⬣⬡⬡)",
  "no_change"                = "No clear signal"
)

avail_labels <- c(
  "wetter_confident"      = "Wetter (⬣⬣⬣)",
  "wetter_most_likely"    = "Wetter (⬣⬣⬡)",
  "wetter_likely"         = "Wetter (⬣⬡⬡)",
  "drier_confident"       = "Drier (⬣⬣⬣)",
  "drier_most_likely"     = "Drier (⬣⬣⬡)",
  "drier_likely"          = "Drier (⬣⬡⬡)",
  "no_change"             = "No clear signal"
)

compound_labels <- c(
  "wetter-accelerated_confident"   = "Wet & Accel. (⬣⬣⬣)",
  "wetter-accelerated_most_likely" = "Wet & Accel. (⬣⬣⬡)",
  "wetter-accelerated_likely"      = "Wet & Accel. (⬣⬡⬡)",
  "wetter-decelerated_confident"   = "Wet & Decel. (⬣⬣⬣)",
  "wetter-decelerated_most_likely" = "Wet & Decel. (⬣⬣⬡)",
  "wetter-decelerated_likely"      = "Wet & Decel. (⬣⬡⬡)",
  "drier-accelerated_confident"    = "Dry & Accel. (⬣⬣⬣)",
  "drier-accelerated_most_likely"  = "Dry & Accel. (⬣⬣⬡)",
  "drier-accelerated_likely"       = "Dry & Accel. (⬣⬡⬡)",
  "drier-decelerated_confident"    = "Dry & Decel. (⬣⬣⬣)",
  "drier-decelerated_most_likely"  = "Dry & Decel. (⬣⬣⬡)",
  "drier-decelerated_likely"       = "Dry & Decel. (⬣⬡⬡)",
  "no_change"                      = "No clear signal"
)

# ============================================================================
# Functions
# ============================================================================

make_fill_key <- function(dt,
                          class_col,
                          likelihood_col,
                          fill_levels) {
  
  dt <- copy(as.data.table(dt))
  
  dt[, class_value := as.character(get(class_col))]
  dt[, likelihood_value := as.character(get(likelihood_col))]
  
  dt[
    is.na(class_value) |
      is.na(likelihood_value) |
      class_value == "no_change" |
      likelihood_value == "no_change",
    fill_key := "no_change"
  ]
  
  dt[
    !is.na(class_value) &
      !is.na(likelihood_value) &
      class_value != "no_change" &
      likelihood_value != "no_change",
    fill_key := paste0(class_value, "_", likelihood_value)
  ]
  
  dt[!fill_key %in% fill_levels, fill_key := "no_change"]
  dt[, fill_key := factor(fill_key, levels = fill_levels)]
  
  dt[, c("class_value", "likelihood_value") := NULL]
  
  dt[]
}

shift_ipcc_hexagons <- function(dt) {
  
  dt <- copy(as.data.table(dt))
  
  rows_aus <- which(dt$Acronym %in% c("NAU", "CAU", "EAU", "SAU"))
  rows_nz  <- which(dt$Acronym == "NZ")
  rows_mdg <- which(dt$Acronym == "MDG")
  rows_gic <- which(dt$Acronym == "GIC")
  
  dt$long[rows_gic] <- dt$long[rows_gic] - 7
  dt$lat[rows_gic]  <- dt$lat[rows_gic] - 4
  dt$V1[rows_gic]   <- dt$V1[rows_gic] - 7
  dt$V2[rows_gic]   <- dt$V2[rows_gic] - 4
  
  dt$long[rows_mdg] <- dt$long[rows_mdg] - 7
  dt$lat[rows_mdg]  <- dt$lat[rows_mdg] - 3
  dt$V1[rows_mdg]   <- dt$V1[rows_mdg] - 7
  dt$V2[rows_mdg]   <- dt$V2[rows_mdg] - 3
  
  dt$long[rows_aus] <- dt$long[rows_aus] + 5
  dt$lat[rows_aus]  <- dt$lat[rows_aus] + 12
  dt$V1[rows_aus]   <- dt$V1[rows_aus] + 5
  dt$V2[rows_aus]   <- dt$V2[rows_aus] + 12
  
  dt$long[rows_nz] <- dt$long[rows_nz] + 10
  dt$lat[rows_nz]  <- dt$lat[rows_nz] + 9
  dt$V1[rows_nz]   <- dt$V1[rows_nz] + 10
  dt$V2[rows_nz]   <- dt$V2[rows_nz] + 9
  
  dt[]
}

prepare_hex_map_data <- function(map_dt,
                                 fill_levels) {
  
  hex_dt <- copy(as.data.table(ipcc_hexagon))
  map_dt <- copy(as.data.table(map_dt))
  
  map_dt <- map_dt[
    ,
    .(
      Acronym = region,
      fill_key
    )
  ]
  
  out <- merge(
    hex_dt,
    map_dt,
    by = "Acronym",
    all.x = TRUE
  )
  
  out[is.na(fill_key), fill_key := "no_change"]
  out[, fill_key := factor(fill_key, levels = fill_levels)]
  
  out <- shift_ipcc_hexagons(out)
  
  out[]
}

get_used_breaks <- function(dt,
                            fill_breaks) {
  
  used <- unique(as.character(dt$fill_key))
  used <- used[!is.na(used)]
  
  fill_breaks[fill_breaks %in% used]
}

assign_label_colour <- function(dt) {
  
  dt <- copy(as.data.table(dt))
  
  dark_keys <- c(
    "accelerating_confident",
    "decelerating_confident",
    "wetter_confident",
    "drier_confident",
    "wetter-accelerated_confident",
    "wetter-decelerated_confident",
    "drier-accelerated_confident",
    "drier-decelerated_confident",
    "wetter-accelerated_most_likely",
    "drier-accelerated_most_likely",
    "drier-decelerated_most_likely"
  )
  
  dt[, label_col := ifelse(
    as.character(fill_key) %in% dark_keys,
    "white",
    "black"
  )]
  
  dt[]
}

base_map_theme <- function() {
  
  theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 12
      ),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
      legend.key.width = unit(0.80, "cm"),
      legend.key.height = unit(0.62, "cm"),
      legend.spacing.x = unit(0.22, "cm"),
      legend.text = element_text(size = 8),
      plot.margin = margin(2, 2, 2, 2)
    )
}

plot_hex_map <- function(dt,
                         fill_cols,
                         fill_breaks,
                         fill_labels,
                         title_text) {
  
  dt <- copy(as.data.table(dt))
  dt <- assign_label_colour(dt)
  
  ggplot(dt) +
    geom_polygon(
      aes(
        x = long,
        y = lat,
        group = group,
        fill = fill_key
      ),
      colour = "grey40",
      linewidth = 0.35
    ) +
    geom_text(
      aes(
        x = V1,
        y = V2,
        label = Acronym,
        colour = label_col
      ),
      size = 2.7,
      show.legend = FALSE
    ) +
    coord_equal(expand = FALSE) +
    scale_fill_manual(
      values = fill_cols,
      breaks = fill_breaks,
      labels = fill_labels[fill_breaks],
      drop = FALSE
    ) +
    scale_colour_identity() +
    guides(
      fill = guide_legend(
        ncol = 3,
        byrow = TRUE,
        title = NULL,
        override.aes = list(
          colour = "grey40",
          linewidth = 0.35
        )
      )
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    base_map_theme()
}

# ============================================================================
# Analysis
# ============================================================================

region_dom_accel <- make_fill_key(
  dt = region_storyline_clean,
  class_col = "accel_class",
  likelihood_col = "accel_likelihood",
  fill_levels = names(accel_cols)
)

region_dom_avail <- make_fill_key(
  dt = region_storyline_clean,
  class_col = "avail_class",
  likelihood_col = "avail_likelihood",
  fill_levels = names(avail_cols)
)

region_dom_compound <- make_fill_key(
  dt = region_storyline_clean,
  class_col = "compound_class",
  likelihood_col = "compound_likelihood",
  fill_levels = names(compound_cols)
)

region_dom_compound_marginal <- make_fill_key(
  dt = region_storyline_clean,
  class_col = "compound_marginal_class",
  likelihood_col = "compound_marginal_likelihood",
  fill_levels = names(compound_cols)
)

map_accel_hex <- prepare_hex_map_data(
  map_dt = region_dom_accel,
  fill_levels = names(accel_cols)
)

map_avail_hex <- prepare_hex_map_data(
  map_dt = region_dom_avail,
  fill_levels = names(avail_cols)
)

map_compound_hex <- prepare_hex_map_data(
  map_dt = region_dom_compound,
  fill_levels = names(compound_cols)
)

map_compound_marginal_hex <- prepare_hex_map_data(
  map_dt = region_dom_compound_marginal,
  fill_levels = names(compound_cols)
)

accel_breaks_used <- get_used_breaks(
  dt = map_accel_hex,
  fill_breaks = accel_breaks
)

avail_breaks_used <- get_used_breaks(
  dt = map_avail_hex,
  fill_breaks = avail_breaks
)

compound_breaks_used <- get_used_breaks(
  dt = map_compound_hex,
  fill_breaks = compound_breaks
)

compound_marginal_breaks_used <- get_used_breaks(
  dt = map_compound_marginal_hex,
  fill_breaks = compound_breaks
)

p_accel <- plot_hex_map(
  dt = map_accel_hex,
  fill_cols = accel_cols,
  fill_breaks = accel_breaks_used,
  fill_labels = accel_labels,
  title_text = "Acceleration"
)

p_avail <- plot_hex_map(
  dt = map_avail_hex,
  fill_cols = avail_cols,
  fill_breaks = avail_breaks_used,
  fill_labels = avail_labels,
  title_text = "Availability"
)

p_compound <- plot_hex_map(
  dt = map_compound_hex,
  fill_cols = compound_cols,
  fill_breaks = compound_breaks_used,
  fill_labels = compound_labels,
  title_text = "Compound change"
)

p_compound_marginal <- plot_hex_map(
  dt = map_compound_marginal_hex,
  fill_cols = compound_cols,
  fill_breaks = compound_marginal_breaks_used,
  fill_labels = compound_labels,
  title_text = "Compound change from marginal modes"
)

p_main_3 <- wrap_plots(
  p_accel,
  p_avail,
  p_compound,
  ncol = 2
)

p_main_4 <- wrap_plots(
  p_accel,
  p_avail,
  p_compound,
  p_compound_marginal,
  ncol = 2
)

print(p_main_3)
print(p_main_4)

# ============================================================================
# Outputs
# ============================================================================

saveRDS(
  map_accel_hex,
  file.path(PATH_OUTPUT_DATA, "map_ipcc_hexagon_storyline_acceleration.Rds")
)

saveRDS(
  map_avail_hex,
  file.path(PATH_OUTPUT_DATA, "map_ipcc_hexagon_storyline_availability.Rds")
)

saveRDS(
  map_compound_hex,
  file.path(PATH_OUTPUT_DATA, "map_ipcc_hexagon_storyline_compound.Rds")
)

saveRDS(
  map_compound_marginal_hex,
  file.path(PATH_OUTPUT_DATA, "map_ipcc_hexagon_storyline_compound_marginal.Rds")
)

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "map_ipcc_hexagon_twc_storylines_clean_three_panels.png"
  ),
  plot = p_main_3,
  width = 11,
  height = 12.4,
  units = "in",
  dpi = 600
)

ggsave(
  filename = file.path(
    PATH_FIGURES,
    "map_ipcc_hexagon_twc_storylines_clean_four_panels.png"
  ),
  plot = p_main_4,
  width = 11,
  height = 12.4,
  units = "in",
  dpi = 600
)

# ============================================================================
# Validation
# ============================================================================

cat("\nInput file:\n")
print(file.path(PATH_OUTPUT_DATA, "region_storyline_clean.Rds"))

cat("\nOutput figure files:\n")
print(file.path(PATH_FIGURES, "map_ipcc_hexagon_twc_storylines_clean_three_panels.png"))
print(file.path(PATH_FIGURES, "map_ipcc_hexagon_twc_storylines_clean_four_panels.png"))

cat("\nClean storyline table dimensions:\n")
print(dim(region_storyline_clean))

cat("\nAcceleration classes used:\n")
print(
  region_dom_accel[
    ,
    .N,
    by = .(accel_class, accel_likelihood, fill_key)
  ][
    order(accel_class, accel_likelihood)
  ]
)

cat("\nAvailability classes used:\n")
print(
  region_dom_avail[
    ,
    .N,
    by = .(avail_class, avail_likelihood, fill_key)
  ][
    order(avail_class, avail_likelihood)
  ]
)

cat("\nCompound classes used:\n")
print(
  region_dom_compound[
    ,
    .N,
    by = .(compound_class, compound_likelihood, fill_key)
  ][
    order(compound_class, compound_likelihood)
  ]
)

cat("\nCompound marginal classes used:\n")
print(
  region_dom_compound_marginal[
    ,
    .N,
    by = .(
      compound_marginal_class,
      compound_marginal_likelihood,
      fill_key
    )
  ][
    order(compound_marginal_class, compound_marginal_likelihood)
  ]
)

cat("\nHex regions without storyline match:\n")
print(
  setdiff(
    unique(ipcc_hexagon$Acronym),
    unique(region_storyline_clean$region)
  )
)

cat("\nStoryline regions without hexagon match:\n")
print(
  setdiff(
    unique(region_storyline_clean$region),
    unique(ipcc_hexagon$Acronym)
  )
)