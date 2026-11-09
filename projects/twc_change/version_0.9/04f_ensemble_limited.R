source("source/twc_change.R")

prec_evap_change <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap_change.Rds"))
pet_change       <- readRDS(file.path(PATH_OUTPUT_DATA, "pet_change.Rds"))
weights          <- readRDS(file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds"))

masks <- pRecipe::pRecipe_masks()

# -------------------------------------------------------------------
# Attach IPCC region
# -------------------------------------------------------------------
ipcc_mask <- masks[land_mask == "land", .(lon, lat, region = ipcc_short_region)]

prec_evap_change <- merge(
  prec_evap_change,
  ipcc_mask,
  by = c("lon", "lat")
)

pet_change <- merge(
  pet_change,
  ipcc_mask,
  by = c("lon", "lat")
)

# if biome is not already in prec_evap_change, attach it too
# adapt the biome column name from masks if needed
if (!"biome" %in% names(prec_evap_change)) {
  biome_mask <- masks[land_mask == "land", .(lon, lat, biome = biome_short_class)]
  prec_evap_change <- merge(prec_evap_change, biome_mask, by = c("lon", "lat"))
}

if (!"biome" %in% names(pet_change)) {
  biome_mask <- masks[land_mask == "land", .(lon, lat, biome = biome_short_class)]
  pet_change <- merge(pet_change, biome_mask, by = c("lon", "lat"))
}

# -------------------------------------------------------------------
# Aggregate to region x biome x dataset
# -------------------------------------------------------------------
prec_evap_region <- prec_evap_change[, .(
  prec_bef_2001 = mean(prec_bef_2001, na.rm = TRUE),
  prec_aft_2001 = mean(prec_aft_2001, na.rm = TRUE),
  evap_bef_2001 = mean(evap_bef_2001, na.rm = TRUE),
  evap_aft_2001 = mean(evap_aft_2001, na.rm = TRUE)
), by = .(region, biome, dataset)]

pet_region <- pet_change[, .(
  pet_bef_2001 = mean(pet_mean_bef_2001, na.rm = TRUE),
  pet_aft_2001 = mean(pet_mean_aft_2001, na.rm = TRUE)
), by = .(region, biome)]

# if pet_change also has dataset, use this instead:
# pet_region <- pet_change[, .(
#   pet_bef_2001 = mean(pet_mean_bef_2001, na.rm = TRUE),
#   pet_aft_2001 = mean(pet_mean_aft_2001, na.rm = TRUE)
# ), by = .(region, biome, dataset)]

# -------------------------------------------------------------------
# Join PET to P/E table
# -------------------------------------------------------------------
ipcc_region_biome_dataset <- merge(
  prec_evap_region,
  pet_region,
  by = intersect(names(prec_evap_region), names(pet_region))
)

# -------------------------------------------------------------------
# Keep only base scenario weights
# -------------------------------------------------------------------
w_base <- weights[scenario == "base"]

# define total regional weight carried by each biome x dataset
w_base[, w_total := w_region_biome * biome_fraction]

# keep only needed columns
w_base <- w_base[, .(region, biome, dataset, w_total)]

# -------------------------------------------------------------------
# Weighted regional means
# -------------------------------------------------------------------
ipcc_weighted <- merge(
  ipcc_region_biome_dataset,
  w_base,
  by = c("region", "biome", "dataset")
)

region_means <- ipcc_weighted[, .(
  prec_bef_2001 = weighted.mean(prec_bef_2001, w_total, na.rm = TRUE),
  prec_aft_2001 = weighted.mean(prec_aft_2001, w_total, na.rm = TRUE),
  evap_bef_2001 = weighted.mean(evap_bef_2001, w_total, na.rm = TRUE),
  evap_aft_2001 = weighted.mean(evap_aft_2001, w_total, na.rm = TRUE),
  pet_bef_2001  = weighted.mean(pet_bef_2001,  w_total, na.rm = TRUE),
  pet_aft_2001  = weighted.mean(pet_aft_2001,  w_total, na.rm = TRUE),
  sum_w         = sum(w_total, na.rm = TRUE)
), by = region]

# -------------------------------------------------------------------
# Direct region classification
# -------------------------------------------------------------------
region_means[, limited_bef_2001 := fifelse(
  pet_bef_2001 > prec_bef_2001, "water", "energy"
)]

region_means[, limited_aft_2001 := fifelse(
  pet_aft_2001 > prec_aft_2001, "water", "energy"
)]

region_means[, limited_change := fifelse(
  limited_bef_2001 == "water"  & limited_aft_2001 == "water",  "w-w",
  fifelse(limited_bef_2001 == "water"  & limited_aft_2001 == "energy", "w-e",
          fifelse(limited_bef_2001 == "energy" & limited_aft_2001 == "water",  "e-w", "e-e"))
)]

region_means[, `:=`(
  prec_change = prec_aft_2001 - prec_bef_2001,
  evap_change = evap_aft_2001 - evap_bef_2001,
  pet_change  = pet_aft_2001  - pet_bef_2001,
  aridity_bef = pet_bef_2001 / prec_bef_2001,
  aridity_aft = pet_aft_2001 / prec_aft_2001
)]

region_means[, limited_bef_2001 := factor(limited_bef_2001, levels = c("energy", "water"))]
region_means[, limited_aft_2001 := factor(limited_aft_2001, levels = c("energy", "water"))]
region_means[, limited_change   := factor(limited_change,   levels = c("w-w", "w-e", "e-w", "e-e"))]

region_means[]

saveRDS(region_means, file.path(PATH_OUTPUT_DATA, "ipcc_regions_change.Rds"))
saveRDS(region_means[, .(region, limited = limited_aft_2001)], 
        file.path(PATH_OUTPUT_DATA, "ipcc_water_energy_limited.Rds"))
