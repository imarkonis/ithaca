
# ============================================================
# 8) Aggregate grid to biome within IPCC region
# ============================================================
# w_{region, biome}(dataset) with sum to 1 per region biome
dataset_region_biome <- dt[
  ,
  .(
    n_cells = .N,
    area_sum = sum(area_w, na.rm = TRUE),
    w_rb_raw = wmean_safe(w_cell, area_w)
  ),
  by = .(ipcc_short_region, biome, dataset)
]

dataset_region_biome[, w_region_biome := normalize_final(w_rb_raw), by = .(ipcc_short_region, biome)]
dataset_region_biome[, w_rb_raw := NULL]

# biome area fractions within each IPCC region
biome_area <- dt[
  ,
  .(biome_area = sum(area_w, na.rm = TRUE)),
  by = .(ipcc_short_region, biome)
]
biome_area[, A_region_biome := normalize_final(biome_area), by = ipcc_short_region]
biome_area[, biome_area := NULL]

# ============================================================
# 9) Monte Carlo sampling of dataset worlds
# ============================================================
# This produces sampled datasets per region biome per simulation and region level mixture weights.
#
# To compute P and E regional values per simulation you will need per grid cell P and E fields.
# This sampling scaffold is still useful now: it generates the dataset choice uncertainty structure.

sample_dataset_worlds <- function(w_rb, A_rb, n_sims = 2000, seed = 1L) {
  set.seed(seed)
  
  # build a quick lookup list: for each region biome, datasets and probs
  key <- w_rb[, .(dataset, prob = w_region_biome), by = .(ipcc_short_region, biome)]
  key <- key[is.finite(prob) & !is.na(prob) & prob > 0]
  
  rb <- unique(key[, .(ipcc_short_region, biome)])
  if (nrow(rb) == 0) stop("No region biome probabilities available for sampling")
  
  # sample one dataset per region biome per sim
  out <- rbindlist(lapply(seq_len(n_sims), function(s) {
    sel <- key[
      ,
      .(dataset = sample(dataset, size = 1, prob = prob)),
      by = .(ipcc_short_region, biome)
    ]
    sel[, sim := s]
    sel
  }))
  
  # attach biome area fractions
  out <- merge(out, A_rb[, .(ipcc_short_region, biome, A_region_biome)],
               by = c("ipcc_short_region", "biome"), all.x = TRUE)
  
  out[]
}

mc_choices <- sample_dataset_worlds(dataset_region_biome, biome_area, n_sims = 2000, seed = 42)

# ============================================================
# 10) Optional region level summary weight (expected mixture)
# ============================================================
# This is NOT the Monte Carlo, it is the deterministic expected weight:
# sum over biomes of A_region_biome * w_region_biome(dataset)
expected_region_weights <- merge(
  dataset_region_biome,
  biome_area,
  by = c("ipcc_short_region", "biome"),
  all.x = TRUE
)

expected_region_weights[, w_region_expected := w_region_biome * A_region_biome]
expected_region_weights <- expected_region_weights[
  ,
  .(w_region_expected = sum(w_region_expected, na.rm = TRUE)),
  by = .(ipcc_short_region, dataset)
]
expected_region_weights[, w_region_expected := normalize_final(w_region_expected), by = ipcc_short_region]

# global expected weight (area across regions)
region_area <- dt[, .(region_area = sum(area_w, na.rm = TRUE)), by = ipcc_short_region]
region_area[, region_w := normalize_final(region_area)]
expected_global <- merge(expected_region_weights, region_area[, .(ipcc_short_region, region_w)],
                         by = "ipcc_short_region", all.x = TRUE)
expected_global <- expected_global[
  ,
  .(w_global_expected = sum(w_region_expected * region_w, na.rm = TRUE)),
  by = dataset
]
expected_global[, w_global_expected := normalize_final(w_global_expected)]
setorder(expected_global, -w_global_expected)

# ============================================================
# Outputs
# ============================================================
cat("\nExpected global mixture weights\n")
print(expected_global)

cat("\nExample Monte Carlo draws (first 10 rows)\n")
print(mc_choices[1:10])

cat("\nExpected weights within selected regions (example)\n")
print(expected_region_weights[ipcc_short_region %in% c("MED", "WCE", "NEU")][order(ipcc_short_region, -w_region_expected)])


# input: dataset_region_biome with cols:
# ipcc_short_region, biome, dataset, w_region_biome_pe

make_prob_wide <- function(dataset_region_biome,
                           prob_col = "w_region_biome_pe_minus",
                           region = "MED") {
  x <- dataset_region_biome[ipcc_short_region == region,
                            .(biome, dataset, prob = get(prob_col))]
  dcast(x, biome ~ dataset, value.var = "prob", fill = 0)
}
make_prob_wide(dataset_region_biome, "w_region_biome", "MED")

make_prob_tidy <- function(dataset_region_biome,
                           prob_col = "w_region_biome_pe_minus") {
  dataset_region_biome[, .(
    ipcc_short_region,
    biome,
    dataset,
    prob = get(prob_col)
  )][is.finite(prob) & !is.na(prob)]
}

make_prob_tidy(dataset_region_biome, "w_region_biome")


make_area_wide <- function(biome_area, region = "MED") {
  x <- biome_area[ipcc_short_region == region, .(biome, A_region_biome)]
  dcast(x, . ~ biome, value.var = "A_region_biome", fill = 0)
}

make_area_wide(biome_area, "MED")
biome_area[, .(ipcc_short_region, biome, A_region_biome)]


make_mc_wide <- function(mc_choices, region = "MED", n_show = 10) {
  x <- mc_choices[ipcc_short_region == region & sim <= n_show,
                  .(biome, sim, dataset)]
  x[, sim := paste0("Ens_", sim)]
  dcast(x, biome ~ sim, value.var = "dataset")
}

make_mc_wide(mc_choices, "MED", n_show = 10)
mc_choices[, .(ipcc_short_region, biome, sim, dataset)]
mc_tidy

expected_region_tidy <- function(dataset_region_biome, biome_area,
                                 prob_col = "w_region_biome_pe_minus") {
  x <- dataset_region_biome[, .(ipcc_short_region, biome, dataset, w = get(prob_col))]
  x <- merge(x, biome_area[, .(ipcc_short_region, biome, A_region_biome)],
             by = c("ipcc_short_region", "biome"), all.x = TRUE)
  
  out <- x[, .(w_region_expected = sum(w * A_region_biome, na.rm = TRUE)),
           by = .(ipcc_short_region, dataset)]
  
  out[, w_region_expected := w_region_expected / sum(w_region_expected, na.rm = TRUE),
      by = ipcc_short_region]
  
  out[]
}

expected_region_tidy(dataset_region_biome, biome_area, "w_region_biome")



