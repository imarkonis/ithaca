# ============================================================================
# Global hotspots of terrestrial water-cycle change and storyline gaps
#
# Panel A: Hotspot map — fill = driver, amber diamond = storyline gap
# Panel B: Dumbbell plot — flux and availability relative change per region
# ============================================================================

# Libraries ===================================================================


library(patchwork)

source("source/twc_change.R")

# Inputs ======================================================================

region_summary <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "member_level_region_summary.Rds"))
)

all_storylines <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_DATA, "all_storylines_probability.Rds"))
)

ipcc_hexagon <- data.table(
  read.csv("/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv")
)

# Constants & Variables =======================================================

N_TOP_FLUX       <- 0L     # top N regions by |flux_rel|
N_TOP_AVAIL      <- 20L     # top N regions by |avail_rel|
DRIVER_RATIO     <- 1.5     # |ΔP/P| > RATIO × |ΔE/E| → P-driven, vice versa
EXPLAINED_THRESH <- 0.60    # p_story threshold for "storyline-consistent"

STORYLINE_LABELS <- c(
  "1_arctic_boreal_amplification"        = "Arctic/boreal ampl.",
  "2_poleward_stormtrack_wetting"        = "Storm-track wetting",
  "3_monsoon_amplification"              = "Monsoon ampl.",
  "4_humid_tropical_intensification"     = "Humid tropical intens.",
  "5_subtropical_circulation_drying"     = "Subtropical drying",
  "6_land_atm_coupling_amplification"    = "Land-atm. coupling",
  "7_deforestation_induced_deceleration" = "Deforestation decel.",
  "8_dryland_soilmoisture_collapse"      = "Dryland collapse"
)

DRIVER_COLS <- c(
  "P-driven"  = "#9FD3CC",
  "E-driven"  = "#F2E3B6",
  "compound"  = "#D4A5C9"
) 
DRIVER_LABELS <- c(
  "P-driven" = "Precipitation",
  "E-driven" = "Evapotranspiration",
  "compound" = "P/E coupled"
)

THEORY_STROKE <- c("explained"  = "grey15",  "unresolved" = "dark red")
THEORY_SHAPE  <- c("explained"  = 21,        "unresolved" = 23)
THEORY_LABELS <- c("explained"  = "Explained",
                   "unresolved" = "Unresolved")

# Helpers =====================================================================

classify_driver <- function(prec_rel, evap_rel, ratio) {
  ap <- abs(prec_rel)
  ae <- abs(evap_rel)
  fcase(
    ap > ratio * ae, "P-driven",
    ae > ratio * ap, "E-driven",
    default          = "compound"
  )
}

shift_ipcc_hexagons <- function(dt) {
  dt <- copy(as.data.table(dt))
  shift_rows <- function(rows, dx, dy) {
    dt$long[rows] <<- dt$long[rows] + dx
    dt$lat[rows]  <<- dt$lat[rows]  + dy
    dt$V1[rows]   <<- dt$V1[rows]   + dx
    dt$V2[rows]   <<- dt$V2[rows]   + dy
  }
  shift_rows(which(dt$Acronym == "GIC"),                        -7,  -4)
  shift_rows(which(dt$Acronym == "MDG"),                        -7,  -3)
  shift_rows(which(dt$Acronym %in% c("NAU","CAU","EAU","SAU")),  5,  12)
  shift_rows(which(dt$Acronym == "NZ"),                         10,   9)
  dt
}

# Analysis ====================================================================

# Step 1. Aggregate to one row per region ------------------------------------
dt <- region_summary[
  region != "GLOBAL" &
    is.finite(prec_rel_change) &
    is.finite(evap_rel_change)
]

regional <- dt[,
               .(
                 prec_rel_med  = median(prec_rel_change[prec_rel_valid],   na.rm = TRUE),
                 evap_rel_med  = median(evap_rel_change[evap_rel_valid],   na.rm = TRUE),
                 avail_rel_med = median(avail_rel_change[avail_rel_valid], na.rm = TRUE),
                 flux_rel_med  = median(flux_rel_change[flux_rel_valid],   na.rm = TRUE),
                 n_member      = .N
               ),
               by = region
]

# Step 2. Driver classification: direct |ΔP/P| vs |ΔE/E| comparison ---------
regional[, driver := classify_driver(prec_rel_med, evap_rel_med, DRIVER_RATIO)]

# Diagnostic: print driver distribution so we can see if E-driven exists
message("Driver distribution across all regions:")
print(regional[, .N, by = driver][order(-N)])

# Step 3. Rank and select hotspots -------------------------------------------
regional[, rank_flux  := rank(-abs(flux_rel_med),  na.last = "keep",
                              ties.method = "first")]
regional[, rank_avail := rank(-abs(avail_rel_med), na.last = "keep",
                              ties.method = "first")]

regional[, is_hotspot   := (rank_flux <= N_TOP_FLUX) | (rank_avail <= N_TOP_AVAIL)]
regional[is.na(is_hotspot), is_hotspot := FALSE]

regional[, hotspot_axis := fcase(
  rank_flux <= N_TOP_FLUX & rank_avail <= N_TOP_AVAIL, "both",
  rank_flux <= N_TOP_FLUX,                              "flux",
  rank_avail <= N_TOP_AVAIL,                            "avail"
)]

# Step 4. Region names — pull from all_storylines, all rows not just dominant -
# This gives region_full for every region that appears in any storyline
region_names <- unique(
  all_storylines[!is.na(region_full) & region_full != "",
                 .(region, region_full)]
)

# Step 5. Dominant storyline and theory status --------------------------------
dom_sl <- all_storylines[order(region, -p_story)][, .SD[1], by = region][
  , .(region, dom_storyline = storyline, p_story)
]

regional <- merge(regional, dom_sl,     by = "region", all.x = TRUE)
regional <- merge(regional, region_names, by = "region", all.x = TRUE)

# fallback: use acronym if still no name
regional[is.na(region_full) | region_full == "", region_full := region]

regional[, theory_status := fifelse(
  !is.na(p_story) & p_story >= EXPLAINED_THRESH,
  "explained", "unresolved"
)]

# Step 6. Panel B table — ranked by max of the two absolute relative changes --
hotspot_dt <- regional[is_hotspot == TRUE][
  order(-pmax(abs(flux_rel_med), abs(avail_rel_med), na.rm = TRUE))
]

hotspot_dt[, region_label := factor(
  paste0(region_full, " (", region, ")"),
  levels = rev(paste0(region_full, " (", region, ")"))
)]

hotspot_dt[, sl_label := fcase(
  theory_status == "unresolved", "Storyline gap",
  !is.na(dom_storyline),         STORYLINE_LABELS[dom_storyline],
  default = "\u2014"
)]

# Dumbbell: one row per region, two x positions (flux and avail)
dumb_dt <- rbindlist(list(
  hotspot_dt[, .(region, region_label, driver, theory_status, sl_label,
                 axis  = "flux",
                 value = flux_rel_med)],
  hotspot_dt[, .(region, region_label, driver, theory_status, sl_label,
                 axis  = "avail",
                 value = avail_rel_med)]
))

# connector segment: one row per region
conn_dt <- hotspot_dt[, .(
  region_label,
  x_flux  = flux_rel_med,
  x_avail = avail_rel_med
)]

# Plot ========================================================================

# --- Panel A: Map ------------------------------------------------------------

# compute range from RAW hex before any shifting
x_range <- range(ipcc_hexagon$long, na.rm = TRUE) + c(-2, 15)
y_range <- range(ipcc_hexagon$lat,  na.rm = TRUE) + c(-2, 2)

map_dt <- merge(
  as.data.table(ipcc_hexagon),
  regional[, .(Acronym = region, is_hotspot, driver, theory_status)],
  by    = "Acronym",
  all.x = TRUE
)
map_dt[is.na(is_hotspot), `:=`(is_hotspot    = FALSE,
                               driver        = NA_character_,
                               theory_status = "none")]
map_dt <- shift_ipcc_hexagons(map_dt)
map_dt[, group_unique := paste(Acronym, group, sep = "__")]

centroids <- map_dt[
  is_hotspot == TRUE & !is.na(Acronym),
  .(V1           = mean(V1, na.rm = TRUE),
    V2           = mean(V2, na.rm = TRUE),
    driver        = driver[1],
    theory_status = theory_status[1]),
  by = Acronym
]

pa <- ggplot() +
  
  geom_polygon(
    data      = map_dt[is_hotspot == FALSE],
    aes(x = long, y = lat, group = group_unique),
    fill      = "grey93",
    colour    = "grey80",
    linewidth = 0.20
  ) +
  
  # explained hotspots
  geom_polygon(
    data      = map_dt[is_hotspot == TRUE & theory_status == "explained"],
    aes(x = long, y = lat, group = group_unique, fill = driver),
    colour    = "grey20",
    linewidth = 0.80
  ) +
  
  # unresolved hotspots — red border
  geom_polygon(
    data      = map_dt[is_hotspot == TRUE & theory_status == "unresolved"],
    aes(x = long, y = lat, group = group_unique, fill = driver),
    colour    = "darkred",
    linewidth = 0.80
  ) +
  
  geom_text(
    data    = centroids,
    aes(x = V1, y = V2, label = Acronym),
    colour  = "grey20", size = 2.2, fontface = "bold"
  ) +
  
# 1. Hotspot type legend 
  scale_fill_manual(
    values   = DRIVER_COLS,
    labels   = DRIVER_LABELS,
    name     = "Dominant driver",
    na.value = "grey70",
    drop     = FALSE,
    guide    = guide_legend(
      override.aes = list(colour = "black", linewidth = 0.5)
    )
  ) +
  coord_equal(expand = FALSE, xlim = x_range, ylim = y_range) +
  labs(tag = "a") +
  theme_void(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_text(face = "bold", size = 8.5),
    legend.text      = element_text(size = 8),
    legend.key.size  = unit(0.45, "cm"),
    legend.spacing.x = unit(0.25, "cm"),
    plot.tag         = element_text(face = "bold", size = 13),
    plot.margin      = margin(4, 4, 0, 4)
  )

# --- Panel B: Dumbbell -------------------------------------------------------

# x-axis span with padding for left label column
x_min <- min(c(dumb_dt$value), na.rm = TRUE)
x_max <- max(c(dumb_dt$value), na.rm = TRUE)
x_pad <- (x_max - x_min) * 0.08

pb <- ggplot() +
  
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.35,
             linetype = "dashed") +
  
  # connector line between flux and avail dots
  geom_segment(
    data = conn_dt,
    aes(x = x_flux, xend = x_avail, y = region_label, yend = region_label),
    colour    = "grey75",
    linewidth = 0.5
  ) +
  
  # dots: shape = axis (flux = circle, avail = square)
  # fill = driver, colour = theory_status
  geom_point(
    data  = dumb_dt,
    aes(x      = value,
        y      = region_label,
        fill   = driver,
        colour = theory_status,
        shape  = axis),
    size   = 3.8,
    stroke = 0.85,
    alpha  = 0.93
  ) +
  
  # storyline label — right-aligned at far left
  geom_text(
    data     = unique(dumb_dt[, .(region_label, sl_label)]),
    aes(x    = x_min - x_pad, y = region_label, label = sl_label),
    hjust    = 1,
    size     = 2.7,
    colour   = "grey35",
    fontface = "italic"
  ) +
  
  scale_fill_manual(
    values = DRIVER_COLS,
    labels = DRIVER_LABELS,
    name   = "Dominant driver",
    drop   = FALSE,
    guide  = guide_legend(
      override.aes = list(shape = 21, colour = "grey20", size = 4)
    )
  ) +
  scale_colour_manual(
    values = THEORY_STROKE,
    labels = THEORY_LABELS,
    name   = "Theory status"
  ) +
  scale_shape_manual(
    values = c("flux" = 21, "avail" = 22),
    labels = c("flux"  = "Flux  \u0394(P+E)/2",
               "avail" = "Availability  \u0394(P\u2212E)"),
    name   = "Metric"
  ) +
  scale_x_continuous(
    name   = "Relative change (%)",
    labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x), "%"),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(xlim = c(x_min - x_pad * 5, x_max + x_pad)) +
  labs(y = NULL, tag = "b") +
  guides(
    colour = guide_legend(override.aes = list(fill = "white",
                                              shape = 21, size = 4))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "#FAFAFA", colour = NA),
    panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.28),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.28),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 8.5, colour = "grey15"),
    axis.text.x        = element_text(size = 8,   colour = "grey40"),
    axis.title.x       = element_text(size = 8.5, colour = "grey30",
                                      margin = margin(t = 5)),
    legend.position    = "right",
    legend.title       = element_text(face = "bold", size = 8.5),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.45, "cm"),
    legend.spacing.y   = unit(0.18, "cm"),
    plot.tag           = element_text(face = "bold", size = 13),
    plot.margin        = margin(4, 10, 4, 4)
  )

# Compose =====================================================================

fig <- pa / pb +
  plot_layout(heights = c(1, 1.6)) +
  theme = theme(
    plot.title      = element_text(face = "bold", size = 12.5,
                                   margin = margin(b = 3)),
    plot.subtitle   = element_text(size = 8, colour = "grey40",
                                   margin = margin(b = 4)),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# Outputs =====================================================================

ggsave(
  filename = file.path(PATH_FIGURES, "hotspot_storyline_gap_figure.png"),
  plot = fig, width = 13, height = 13, dpi = 600
)

ggsave(
  filename = file.path(PATH_FIGURES, "hotspot_storyline_gap_figure.pdf"),
  plot = fig, width = 13, height = 13, device = cairo_pdf
)

# Diagnostic: driver distribution among hotspots only ========================

message("\nDriver distribution among selected hotspots:")
print(hotspot_dt[, .N, by = driver])

print(hotspot_dt[order(-pmax(abs(flux_rel_med), abs(avail_rel_med), na.rm=TRUE)),
                 .(region, region_full, driver, theory_status,
                   flux_rel_med, avail_rel_med, prec_rel_med, evap_rel_med,
                   rank_flux, rank_avail, dom_storyline, p_story)
])
