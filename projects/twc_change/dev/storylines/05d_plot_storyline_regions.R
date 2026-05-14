# ============================================================================
# Plot IPCC hexagon map — dominant mechanistic storyline per region
# ============================================================================

library(patchwork)
source("source/twc_change.R")

# Inputs ======================================================================

all_storylines <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "all_storylines_probability.Rds"))
)

ipcc_hexagon <- data.table(
  read.csv("/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv")
)

# Constants ===================================================================

LIKELIHOOD_LEVELS <- c("no_change", "likely", "most_likely", "confident")

LABELS <- c(
  "1_arctic_boreal_amplification"        = "1. Arctic/boreal ampl.",
  "2_poleward_stormtrack_wetting"        = "2. Storm-track wetting",
  "3_monsoon_amplification"              = "3. Monsoon ampl.",
  "4_humid_tropical_intensification"     = "4. Humid tropical intens.",
  "5_subtropical_circulation_drying"     = "5. Subtropical drying",
  "6_land_atm_coupling_amplification"    = "6. Land-atm. coupling",
  "7_deforestation_induced_deceleration" = "7. Deforestation decel.",
  "8_dryland_soilmoisture_collapse"      = "8. Dryland collapse"
)

# Palette =====================================================================

COL_S1 <- c("#C8DDF1", "#7FB0DD", "#2166AC")
COL_S2 <- c("#C8EAE5", "#6DBDB4", "#01665E")
COL_S3 <- c("#C7E9C0", "#74C476", "#238B45")
COL_S4 <- c("#D4EEF7", "#7BC8E2", "#2B8CBE")
COL_S5 <- c("#FDCC8A", "#FC8D59", "#D7301F")
COL_S6 <- c("#F6E8C3", "#DFC27D", "#8C510A")
COL_S7 <- c("#E8C5C5", "#D07070", "#990000")
COL_S8 <- c("#FDE0C8", "#F4A261", "#E76F51")

cols <- c(
  "1_arctic_boreal_amplification_likely"             = COL_S1[1],
  "1_arctic_boreal_amplification_most_likely"        = COL_S1[2],
  "1_arctic_boreal_amplification_confident"          = COL_S1[3],
  "2_poleward_stormtrack_wetting_likely"             = COL_S2[1],
  "2_poleward_stormtrack_wetting_most_likely"        = COL_S2[2],
  "2_poleward_stormtrack_wetting_confident"          = COL_S2[3],
  "3_monsoon_amplification_likely"                   = COL_S3[1],
  "3_monsoon_amplification_most_likely"              = COL_S3[2],
  "3_monsoon_amplification_confident"                = COL_S3[3],
  "4_humid_tropical_intensification_likely"          = COL_S4[1],
  "4_humid_tropical_intensification_most_likely"     = COL_S4[2],
  "4_humid_tropical_intensification_confident"       = COL_S4[3],
  "5_subtropical_circulation_drying_likely"          = COL_S5[1],
  "5_subtropical_circulation_drying_most_likely"     = COL_S5[2],
  "5_subtropical_circulation_drying_confident"       = COL_S5[3],
  "6_land_atm_coupling_amplification_likely"         = COL_S6[1],
  "6_land_atm_coupling_amplification_most_likely"    = COL_S6[2],
  "6_land_atm_coupling_amplification_confident"      = COL_S6[3],
  "7_deforestation_induced_deceleration_likely"      = COL_S7[1],
  "7_deforestation_induced_deceleration_most_likely" = COL_S7[2],
  "7_deforestation_induced_deceleration_confident"   = COL_S7[3],
  "8_dryland_soilmoisture_collapse_likely"           = COL_S8[1],
  "8_dryland_soilmoisture_collapse_most_likely"      = COL_S8[2],
  "8_dryland_soilmoisture_collapse_confident"        = COL_S8[3],
  "no_change"                                        = "#D9D9D9"
)

# Breaks: confident | most_likely | likely per storyline row ==================

breaks <- c(
  "1_arctic_boreal_amplification_confident",
  "1_arctic_boreal_amplification_most_likely",
  "1_arctic_boreal_amplification_likely",
  "2_poleward_stormtrack_wetting_confident",
  "2_poleward_stormtrack_wetting_most_likely",
  "2_poleward_stormtrack_wetting_likely",
  "3_monsoon_amplification_confident",
  "3_monsoon_amplification_most_likely",
  "3_monsoon_amplification_likely",
  "4_humid_tropical_intensification_confident",
  "4_humid_tropical_intensification_most_likely",
  "4_humid_tropical_intensification_likely",
  "5_subtropical_circulation_drying_confident",
  "5_subtropical_circulation_drying_most_likely",
  "5_subtropical_circulation_drying_likely",
  "6_land_atm_coupling_amplification_confident",
  "6_land_atm_coupling_amplification_most_likely",
  "6_land_atm_coupling_amplification_likely",
  "7_deforestation_induced_deceleration_confident",
  "7_deforestation_induced_deceleration_most_likely",
  "7_deforestation_induced_deceleration_likely",
  "8_dryland_soilmoisture_collapse_confident",
  "8_dryland_soilmoisture_collapse_most_likely",
  "8_dryland_soilmoisture_collapse_likely",
  "no_change"
)

legend_labels <- c(
  setNames(
    paste0(
      rep(LABELS, each = 3),
      c(" (\u2b23\u2b23\u2b23)", " (\u2b23\u2b23\u2b21)", " (\u2b23\u2b21\u2b21)")
    ),
    breaks[breaks != "no_change"]
  ),
  "no_change" = "No clear signal"
)

# Helpers =====================================================================

classify_likelihood_story <- function(p) {
  fcase(
    is.na(p) | p < 0.40, "no_change",
    p < 0.60,             "likely",
    p < 0.80,             "most_likely",
    default               = "confident"
  )
}

shift_ipcc_hexagons <- function(dt) {
  dt <- copy(as.data.table(dt))
  
  rows_aus <- which(dt$Acronym %in% c("NAU", "CAU", "EAU", "SAU"))
  rows_nz  <- which(dt$Acronym == "NZ")
  rows_mdg <- which(dt$Acronym == "MDG")
  rows_gic <- which(dt$Acronym == "GIC")
  
  dt$long[rows_gic] <- dt$long[rows_gic] - 7
  dt$lat[rows_gic]  <- dt$lat[rows_gic]  - 4
  dt$V1[rows_gic]   <- dt$V1[rows_gic]   - 7
  dt$V2[rows_gic]   <- dt$V2[rows_gic]   - 4
  
  dt$long[rows_mdg] <- dt$long[rows_mdg] - 7
  dt$lat[rows_mdg]  <- dt$lat[rows_mdg]  - 3
  dt$V1[rows_mdg]   <- dt$V1[rows_mdg]   - 7
  dt$V2[rows_mdg]   <- dt$V2[rows_mdg]   - 3
  
  dt$long[rows_aus] <- dt$long[rows_aus] + 5
  dt$lat[rows_aus]  <- dt$lat[rows_aus]  + 12
  dt$V1[rows_aus]   <- dt$V1[rows_aus]   + 5
  dt$V2[rows_aus]   <- dt$V2[rows_aus]   + 12
  
  dt$long[rows_nz] <- dt$long[rows_nz] + 10
  dt$lat[rows_nz]  <- dt$lat[rows_nz]  + 9
  dt$V1[rows_nz]   <- dt$V1[rows_nz]   + 10
  dt$V2[rows_nz]   <- dt$V2[rows_nz]   + 9
  
  dt
}

assign_label_colour <- function(dt) {
  dt <- copy(as.data.table(dt))
  dark_keys <- grep("_confident$|_most_likely$", names(cols), value = TRUE)
  dt[, label_col := ifelse(as.character(fill_key) %in% dark_keys, "white", "black")]
  dt
}

# Build dominant storyline per region =========================================

all_storylines[, likelihood := classify_likelihood_story(p_story)]
all_storylines[, likelihood := factor(likelihood, levels = LIKELIHOOD_LEVELS, ordered = TRUE)]

likelihood_rank_map <- c(
  "no_change"   = 1L,
  "likely"      = 2L,
  "most_likely" = 3L,
  "confident"   = 4L
)

all_storylines[, likelihood_rank := likelihood_rank_map[as.character(likelihood)]]

dom_story <- all_storylines[
  order(region, -likelihood_rank, -p_story, storyline)
][, .SD[1], by = region]

dom_story[likelihood == "no_change", storyline := NA_character_]

dom_story[, fill_key := fifelse(
  is.na(storyline) | likelihood == "no_change",
  "no_change",
  paste0(storyline, "_", as.character(likelihood))
)]

dom_story[, fill_key := as.character(fill_key)]

# Join to hexagons ============================================================

hex_dt <- as.data.table(ipcc_hexagon)

map_dt <- merge(
  hex_dt,
  dom_story[, .(Acronym = region, fill_key, storyline, likelihood)],
  by    = "Acronym",
  all.x = TRUE          # keep ALL hexagon regions
)

map_dt[is.na(fill_key), fill_key := "no_change"]
map_dt[, fill_key := as.character(fill_key)]

map_dt <- shift_ipcc_hexagons(map_dt)
map_dt[, group_unique := paste(Acronym, group, sep = "__")]
map_dt <- assign_label_colour(map_dt)

# Plot ========================================================================

p <- ggplot(map_dt) +
  geom_polygon(
    aes(x = long, y = lat, group = group_unique, fill = fill_key),
    colour    = "grey40",
    linewidth = 0.35
  ) +
  geom_text(
    aes(x = V1, y = V2, label = Acronym, colour = label_col),
    size        = 2.7,
    show.legend = FALSE
  ) +
  coord_equal(expand = FALSE) +
  scale_fill_manual(
    values   = cols,
    breaks   = breaks,
    labels   = legend_labels,
    na.value = "#D9D9D9",    # unmatched regions → same grey as no_change
    name     = NULL
  ) +
  scale_colour_identity() +
  guides(
    fill = guide_legend(
      ncol         = 3,    # confident | most_likely | likely per row
      byrow        = TRUE,
      title        = NULL,
      override.aes = list(colour = "grey40", linewidth = 0.35)
    )
  ) +
  labs(
    title    = "Dominant mechanistic storyline per IPCC region",
    subtitle = "Most likely storyline where regions overlap · Intensity = likelihood level",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    plot.title        = element_text(face = "bold", hjust = 0.5, size = 13,
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(hjust = 0.5, size = 9, colour = "grey40",
                                     margin = margin(b = 8)),
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.margin     = margin(t = 6, r = 2, b = 2, l = 2),
    legend.key.width  = unit(0.80, "cm"),
    legend.key.height = unit(0.62, "cm"),
    legend.spacing.x  = unit(0.18, "cm"),
    legend.spacing.y  = unit(0.10, "cm"),
    legend.text       = element_text(size = 7.5),
    plot.margin       = margin(4, 4, 4, 4)
  )

print(p)

# Save ========================================================================

ggsave(
  filename = file.path(PATH_FIGURES, "map_ipcc_hexagon_dominant.png"),
  plot     = p,
  width    = 14,
  height   = 8.5,
  units    = "in",
  dpi      = 600
)