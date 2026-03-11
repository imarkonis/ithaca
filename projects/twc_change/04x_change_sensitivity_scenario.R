library(data.table)
library(ggplot2)

# s must have: scenario, region, x_med, y_med
base <- s[scenario == "base", .(region, x0 = x_med, y0 = y_med)]
drel <- merge(s, base, by = "region")
drel <- drel[scenario != "base"]

# stabilizer to avoid division by ~0 (robust choice)
eps_x <- max(1e-6, quantile(abs(base$x0), 0.10, na.rm = TRUE))
eps_y <- max(1e-6, quantile(abs(base$y0), 0.10, na.rm = TRUE))

drel[, rel_dx := (x_med - x0) / pmax(abs(x0), eps_x)]
drel[, rel_dy := (y_med - y0) / pmax(abs(y0), eps_y)]

p_rel <- ggplot(drel, aes(rel_dx, rel_dy)) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_point(size = 1.8, alpha = 0.9) +
  facet_wrap(~ scenario) +
  coord_equal() +
  labs(
    x = "Relative Δ availability (scenario − base) / |base|",
    y = "Relative Δ flux (scenario − base) / |base|"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

p_rel