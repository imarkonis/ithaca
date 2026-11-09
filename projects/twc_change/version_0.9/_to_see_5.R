library(data.table)
install.packages('patchwork')
library(patchwork)

plot_dt <- merge(
  twc_dataset[scenario == "base"],
  REGION_CLASS,
  by = "region",
  all.x = TRUE
)

plot_dt[, outcome_class := fifelse(
  avail_med >= 0 & flux_med >= 0, "wetter_accelerated",
  fifelse(
    avail_med < 0 & flux_med >= 0, "drier_accelerated",
    fifelse(
      avail_med >= 0 & flux_med < 0, "wetter_decelerated",
      "drier_decelerated"
    )
  )
)]

plot_dt[, outcome_class := factor(
  outcome_class,
  levels = c(
    "wetter_accelerated",
    "drier_accelerated",
    "wetter_decelerated",
    "drier_decelerated"
  )
)]

p1 <- plot_avail_flux(
  dt = plot_dt,
  color_var = "hemisphere",
  color_values = c(north = "#3C78D8", south = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80)
)

p1

p2 <- plot_prec_evap(
  dt = plot_dt,
  color_var = "hemisphere",
  color_values = c(north = "#3C78D8", south = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80)
)

p2

p3 <- plot_avail_flux(
  dt = plot_dt,
  facet_var = "circulation",
  color_var = "hemisphere",
  color_values = c(north = "#3C78D8", south = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80),
  show_labels = FALSE
)

p3

p4 <- plot_prec_evap(
  dt = plot_dt,
  facet_var = "circulation",
  color_var = "hemisphere",
  color_values = c(north = "#3C78D8", south = "#D95F5F"),
  xlim = c(-80, 80),
)

p4

p5 <- plot_avail_flux(
  dt = plot_dt,
  facet_var = "hydrobelt",
  color_var = "hemisphere",
  color_values = c(north = "#3C78D8", south = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80),
  show_labels = FALSE
)

p5

p6 <- plot_prec_evap(
  dt = plot_dt,
  facet_var = "hydrobelt",
  color_var = "hemisphere",
  color_values = c(north = "#3C78D8", south = "#D95F5F"),
  xlim = c(-80, 80),
  ylim = c(-80, 80)
)

p6

quad_tab <- plot_dt[, .N, by = .(circulation, outcome_class)]

p9 <- ggplot(quad_tab, aes(x = circulation, y = N, fill = outcome_class)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Fraction of regions", fill = "Outcome class") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p9

cent_circ <- plot_dt[, .(
  avail = median(avail_med, na.rm = TRUE),
  flux  = median(flux_med, na.rm = TRUE),
  p     = median(prec_med, na.rm = TRUE),
  e     = median(evap_med, na.rm = TRUE)
), by = .(circulation, hemisphere)]

p10a <- ggplot(cent_circ, aes(avail, flux, label = circulation, col = hemisphere)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_point(size = 3) +
  geom_text(nudge_y = 3) +
  coord_equal(xlim = c(-40, 40), ylim = c(-40, 40)) +
  labs(x = expression(Delta(P-E)), y = expression(Delta((P+E)/2))) +
  theme_bw(base_size = 12)

p10b <- ggplot(cent_circ, aes(p, e, label = circulation, col = hemisphere)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_point(size = 3) +
  geom_text(nudge_y = 3) +
  coord_equal(xlim = c(-40, 40), ylim = c(-40, 40)) +
  labs(x = expression(Delta(P)), y = expression(Delta(E))) +
  theme_bw(base_size = 12)


p10a 
p10b
cent_circ <- plot_dt[, .(
  avail = median(avail_med, na.rm = TRUE),
  flux  = median(flux_med, na.rm = TRUE),
  p     = median(prec_med, na.rm = TRUE),
  e     = median(evap_med, na.rm = TRUE)
)]

p10a <- ggplot(cent_circ, aes(avail, flux, label = hydrobelt)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_point(size = 3) +
  geom_text(nudge_y = 3) +
  coord_equal(xlim = c(-80, 80), ylim = c(-80, 80)) +
  labs(x = expression(Delta(P-E)), y = expression(Delta((P+E)/2))) +
  theme_bw(base_size = 12)

p10b <- ggplot(cent_circ, aes(p, e, label = hydrobelt)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_abline(slope = 1, intercept = 0, color = "grey55") +
  geom_abline(slope = -1, intercept = 0, color = "grey75") +
  geom_point(size = 3) +
  geom_text(nudge_y = 3) +
  coord_equal(xlim = c(-20, 20), ylim = c(-20, 20)) +
  labs(x = expression(Delta(P)), y = expression(Delta(E))) +
  theme_bw(base_size = 12)

p10a
p10b
