source('source/twc_change.R')

library(dplyr)
library(scales)

avail_flux_change <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_grid.rds'))

prec_evap_change <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'prec_evap_change.Rds'))
aet_change <- prec_evap_change[, .(lon, lat, dataset, evap_change, evap_bef_2001, evap_aft_2001)]
prec_change <- prec_evap_change[, .(lon, lat, dataset, prec_change, prec_bef_2001, prec_aft_2001)]

pet_change <- readRDS(file = paste0(PATH_OUTPUT_DATA, 'pet_change.Rds'))

pet_aet_prec <- merge(pet_change, aet_change, by = c('lon', 'lat'))

pet_aet_prec <- merge(pet_change, avail_flux_change)
pet_aet_prec <- merge(pet_aet_prec, prec_evap_change, by = c('lon', 'lat', 'dataset'))

table(pet_aet_prec[, pet_change > evap_change, dataset])

pet_aet_prec[, limited_bef_2001 := factor('uncertain')]
pet_aet_prec[pet_min_bef_2001 > prec_bef_2001, limited_bef_2001 := factor('water')]
pet_aet_prec[pet_max_bef_2001 < prec_bef_2001, limited_bef_2001 := factor('energy')]
pet_aet_prec[, table(limited_bef_2001)]

pet_aet_prec[, limited_aft_2001 := factor('uncertain')]
pet_aet_prec[pet_min_aft_2001 > prec_aft_2001, limited_aft_2001 := factor('water')]
pet_aet_prec[pet_max_aft_2001 < prec_aft_2001, limited_aft_2001 := factor('energy')]
pet_aet_prec[, table(limited_aft_2001)]

pet_aet_prec[limited_bef_2001 == 'water', table(limited_aft_2001)]
pet_aet_prec[limited_bef_2001 == 'energy', table(limited_aft_2001)]

pet_aet_prec[, limited_change := factor('u-u')]
pet_aet_prec[limited_bef_2001 == 'uncertain' & limited_aft_2001 == 'water', limited_change := factor('u-w')]
pet_aet_prec[limited_bef_2001 == 'uncertain' & limited_aft_2001 == 'energy', limited_change := factor('u-e')]
pet_aet_prec[limited_bef_2001 == 'water' & limited_aft_2001 == 'water', limited_change := factor('w-w')]
pet_aet_prec[limited_bef_2001 == 'water' & limited_aft_2001 == 'uncertain', limited_change := factor('w-u')]
pet_aet_prec[limited_bef_2001 == 'water' & limited_aft_2001 == 'energy', limited_change := factor('w-e')]
pet_aet_prec[limited_bef_2001 == 'energy' & limited_aft_2001 == 'energy', limited_change := factor('e-e')]
pet_aet_prec[limited_bef_2001 == 'energy' & limited_aft_2001 == 'water', limited_change := factor('e-w')]
pet_aet_prec[limited_bef_2001 == 'energy' & limited_aft_2001 == 'uncertain', limited_change := factor('e-u')]


#Plots
df <- as.data.table(pet_aet_prec)[, .(n = .N), by = .(dataset, limited_change)]
df <- as_tibble(df)

legend_levels <- c("e-e", "w-e", "w-u", "u-e", "u-u", "u-w", "e-u", "e-w", "w-w")

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

df <- df %>%
  mutate(
    dataset = as.factor(dataset),
    limited_change = as.character(limited_change)
  ) %>%
  tidyr::complete(dataset, limited_change = legend_levels, fill = list(n = 0)) %>%
  group_by(dataset) %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  mutate(trans = factor(limited_change, levels = legend_levels))

segment_bounds <- function(d, key) {
  d %>%
    arrange(dataset, trans) %>%      # trans factor levels define stack order
    group_by(dataset) %>%
    mutate(cum = cumsum(p),
           start = cum - p,
           end   = cum) %>%
    ungroup() %>%
    filter(trans == key) %>%
    transmute(dataset, start, end)
}

line_ee <- df %>% filter(trans == "e-e") %>% transmute(dataset, y = 1 - p)
line_ww <- df %>% filter(trans == "w-w") %>% transmute(dataset, y = p)

uu <- segment_bounds(df, "u-u") %>%
  mutate(
    start = 1 - start,
    end   = 1 - end
  )

line_uu_start <- uu %>% transmute(dataset, y = start)
line_uu_end   <- uu %>% transmute(dataset, y = end)

line_df <- bind_rows(line_ee, line_ww, line_uu_start, line_uu_end)

ggplot(df, aes(x = dataset, y = p, fill = trans)) +
  geom_col(width = 0.8) +
  geom_segment(
    data = line_df,
    aes(
      x = as.numeric(dataset) - 0.4, xend = as.numeric(dataset) + 0.4,
      y = y, yend = y
    ),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.9
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = trans_cols, drop = FALSE) +
  labs(
    title = "Transition composition per dataset",
    subtitle = "w = water-limited, u = uncertain, e = energy-limited",
    x = NULL, y = "Share of transitions", fill = "Transition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


# ============================================================
# 5 global maps: 5 datasets 
# ============================================================

map_df <- as.data.table(pet_aet_prec)[
  ,
  .(lon, lat, dataset, limited_change)
] |> 
  as_tibble() |>
  mutate(
    dataset = factor(dataset),
    limited_change = factor(as.character(limited_change), levels = legend_levels)
  )

ggplot(map_df, aes(x = lon, y = lat, fill = limited_change)) +
  geom_raster() +  # use geom_tile() if your grid isn't perfectly regular
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ dataset, ncol = 2) +
  scale_fill_manual(values = trans_cols, drop = FALSE) +
  labs(
    title = "Limited-regime transitions (before → after 2001)",
    x = NULL, y = NULL, fill = "Transition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )


# ============================================================
# 10 focus maps: 5 datasets x 2 latitude bands
# ============================================================

band_df <- bind_rows(
  map_df %>%
    filter(lat >= -30, lat <= 10) %>%
    mutate(band = "Tropics (-30 to 10)"),
  map_df %>%
    filter(lat >= 40, lat <= 70) %>%
    mutate(band = "High lat (40 to 70)")
) %>%
  mutate(
    dataset = factor(dataset, levels = c("ERA5L","FLDAS","MERRA","GLEAM","TERRA")),
    band    = factor(band, levels = c("Tropics (-30 to 10)", "High lat (40 to 70)")),
    facet   = factor(paste(dataset, band, sep = " | "),
                     levels = as.vector(t(outer(
                       c("ERA5L","FLDAS","MERRA","GLEAM","TERRA"),
                       c("Tropics (-30 to 10)","High lat (40 to 70)"),
                       paste, sep = " | "
                     ))))
  )

ggplot(band_df, aes(x = lon, y = lat, fill = limited_change)) +
  geom_raster() +
  coord_quickmap(expand = FALSE) +
  facet_wrap(~ facet, ncol = 2, scales = "free") +
  scale_fill_manual(values = trans_cols, drop = FALSE) +
  labs(
    title = "Hydrological regime transitions",
    x = NULL, y = NULL, fill = "Transition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
