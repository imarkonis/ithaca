library(scales)

setDT(expected_region_weights)

# sanity
stopifnot(all(c("scenario","region","dataset","w_region_expected") %in% names(expected_region_weights)))

# optional filter to reduce clutter
regions_keep <- c("MED","WCE","NEU")
ew <- expected_region_weights[region %in% regions_keep]

ggplot(ew, aes(x = scenario, y = dataset, fill = w_region_expected)) +
  geom_tile() +
  facet_wrap(~ region, scales = "free_y") +
  scale_fill_viridis_c(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = NULL, fill = "Weight") +
  theme_minimal(base_size = 12)


rank_dt <- ew[
  ,
  .(w = w_region_expected),
  by = .(region, scenario, dataset)
]
rank_dt[, rank := frank(-w, ties.method = "average"), by = .(region, scenario)]

# keep top K by average weight, to avoid spaghetti
K <- 8
topK <- rank_dt[, .(wbar = mean(w)), by = .(region, dataset)][
  ,
  .SD[order(-wbar)][1:K],
  by = region
]

rank_plot <- merge(rank_dt, topK[, .(region, dataset)], by = c("region","dataset"))

ggplot(rank_plot, aes(x = scenario, y = rank, group = dataset)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 2) +
  scale_y_reverse(breaks = 1:K) +
  facet_wrap(~ region) +
  labs(x = NULL, y = "Rank (1 is best)") +
  theme_minimal(base_size = 12)


baseline <- "base"

base_dt <- ew[scenario == baseline, .(region, dataset, w_base = w_region_expected)]
cmp_dt  <- ew[scenario != baseline, .(region, scenario, dataset, w = w_region_expected)]

cmp <- merge(cmp_dt, base_dt, by = c("region","dataset"), all.x = TRUE)

eps <- 1e-12
cmp[, log_ratio := log((w + eps) / (w_base + eps))]

ggplot(cmp, aes(x = dataset, y = log_ratio, fill = scenario)) +
  geom_col(position = "dodge") +
  facet_wrap(~ region, scales = "free_x") +
  labs(x = NULL, y = "log(weight scenario / weight base)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


conc <- ew[
  ,
  .(
    hhi = sum(w_region_expected^2, na.rm = TRUE),
    entropy = -sum(ifelse(w_region_expected > 0, w_region_expected * log(w_region_expected), 0), na.rm = TRUE),
    n = .N
  ),
  by = .(region, scenario)
]

ggplot(conc, aes(x = scenario, y = hhi, group = region)) +
  geom_line(alpha = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ region, scales = "free_y") +
  labs(x = NULL, y = "HHI (higher means more concentrated)") +
  theme_minimal(base_size = 12)


### Global

setDT(expected_region_weights)

# equal-weight average over regions (no area_w)
global_w <- expected_region_weights[
  ,
  .(w_global = mean(w_region_expected, na.rm = TRUE)),
  by = .(scenario, dataset)
]

# renormalize per scenario (just in case)
global_w[, w_global := w_global / sum(w_global, na.rm = TRUE), by = scenario]
setorder(global_w, scenario, -w_global)

baseline <- "base"
base_dt <- global_w[scenario == baseline, .(dataset, w_base = w_global)]
cmp <- merge(global_w, base_dt, by = "dataset", all.x = TRUE)
cmp[, delta := w_global - w_base]

ggplot(cmp[scenario != baseline], aes(x = dataset, y = delta, fill = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Δ global weight vs baseline") +
  theme_minimal(base_size = 12)


baseline <- "base"   # change if needed

ranks <- expected_region_weights[
  ,
  .(w = w_region_expected),
  by = .(region, scenario, dataset)
]

# rank 1 = best
ranks[, rank := frank(-w, ties.method = "min"), by = .(region, scenario)]

base_rank <- ranks[scenario == baseline, .(region, dataset, rank_base = rank)]

cmp <- merge(
  ranks[scenario != baseline, .(region, scenario, dataset, rank)],
  base_rank,
  by = c("region", "dataset"),
  all.x = TRUE
)

# region-level flag: any dataset rank differs from baseline
region_shift <- cmp[
  ,
  .(rank_changed = any(rank != rank_base, na.rm = TRUE)),
  by = .(scenario, region)
]

# count shifted regions per scenario
shift_counts <- region_shift[, .(
  n_regions = .N,
  n_shift = sum(rank_changed, na.rm = TRUE),
  frac_shift = sum(rank_changed, na.rm = TRUE) / .N
), by = scenario][order(-n_shift)]

shift_magnitude <- cmp[
  ,
  .(
    mean_abs_drank = mean(abs(rank - rank_base), na.rm = TRUE),
    max_abs_drank  = max(abs(rank - rank_base), na.rm = TRUE)
  ),
  by = .(scenario, region)
]

mag_summary <- shift_magnitude[, .(
  mean_abs_drank = mean(mean_abs_drank, na.rm = TRUE),
  max_abs_drank  = max(max_abs_drank, na.rm = TRUE)
), by = scenario][order(-mean_abs_drank)]


ggplot(shift_counts, aes(x = scenario, y = n_shift, group = 1)) +
  geom_col() +
  labs(x = NULL, y = "Number of IPCC regions with any rank change vs baseline") +
  theme_minimal(base_size = 12)

top_base <- ranks[scenario == baseline][rank == 1, .(region, top_base = dataset)]
top_cmp  <- ranks[scenario != baseline][rank == 1, .(scenario, region, top = dataset)]
top_cmp  <- merge(top_cmp, top_base, by = "region", all.x = TRUE)

top_switch <- top_cmp[, .(
  n_regions = .N,
  n_top_switch = sum(top != top_base, na.rm = TRUE),
  frac_top_switch = sum(top != top_base, na.rm = TRUE) / .N
), by = scenario][order(-n_top_switch)]

top_switch
