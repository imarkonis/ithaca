source("source/twc_change.R")

avail_flux_change <- readRDS(file.path(PATH_OUTPUT_DATA, "avail_flux_change_grid.rds"))
prec_evap_change  <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap_change.Rds"))
pet_change        <- readRDS(file.path(PATH_OUTPUT_DATA, "pet_change.Rds"))

setkey(avail_flux_change, lon, lat, dataset)
setkey(prec_evap_change,  lon, lat, dataset)
setkey(pet_change,        lon, lat)

aet_change <- prec_evap_change[, .(
  lon, lat, dataset,
  evap_change, evap_bef_2001, evap_aft_2001
)]
prec_change <- prec_evap_change[, .(
  lon, lat, dataset,
  prec_change, prec_bef_2001, prec_aft_2001
)]

# -------------------------------------------------------------------
# PET vs AET consistency check
# -------------------------------------------------------------------
pet_mean_aet_check <- merge(pet_change, aet_change, by = c("lon", "lat"))[
  , .(
    lon, lat, dataset,
    pet_aet_diff_bef_2001 = pet_mean_bef_2001 - evap_bef_2001,
    pet_aet_diff_aft_2001 = pet_mean_aft_2001 - evap_aft_2001
  )
]

pet_mean_aet_check[dataset == "GLEAM" & pet_aet_diff_aft_2001 < 0]
table(pet_mean_aet_check[pet_aet_diff_aft_2001 < 0, dataset])
table(pet_mean_aet_check[pet_aet_diff_bef_2001 < 0, dataset])

pet_min_aet_check <- merge(pet_change, aet_change, by = c("lon", "lat"))[
  , .(
    lon, lat, dataset,
    pet_aet_diff_bef_2001 = pet_min_bef_2001 - evap_bef_2001,
    pet_aet_diff_aft_2001 = pet_min_aft_2001 - evap_aft_2001
  )
]

pet_min_aet_check[dataset == "GLEAM" & pet_aet_diff_aft_2001 < 0]
table(pet_min_aet_check[pet_aet_diff_aft_2001 < 0, dataset])
table(pet_min_aet_check[pet_aet_diff_bef_2001 < 0, dataset])

# -------------------------------------------------------------------
# Build analysis table 
# -------------------------------------------------------------------
twc <- merge(avail_flux_change, pet_change, by = c("lon", "lat"))
twc <- merge(twc, prec_evap_change, by = c("lon", "lat", "dataset"))
twc[ , pet_change := pet_mean_aft_2001 - pet_mean_bef_2001]
table(twc[,  pet_change > evap_change])

# -------------------------------------------------------------------
# Limitation regime (water-limited if PET > P)
# -------------------------------------------------------------------
twc[, limited_bef_2001 := ifelse(pet_mean_bef_2001 > prec_bef_2001, "water", "energy")]
twc[, limited_aft_2001 := ifelse(pet_mean_aft_2001 > prec_aft_2001, "water", "energy")]

twc[, limited_bef_2001 := factor(limited_bef_2001, levels = c("energy", "water"))]
twc[, limited_aft_2001 := factor(limited_aft_2001, levels = c("energy", "water"))]

table(twc$limited_bef_2001)
table(twc$limited_aft_2001)

table(twc[limited_bef_2001 == "water",  limited_aft_2001])
table(twc[limited_bef_2001 == "energy", limited_aft_2001])

# -------------------------------------------------------------------
# Transition category (w-w, w-e, e-w, e-e)
# -------------------------------------------------------------------
twc[, limited_change := fifelse(
  limited_bef_2001 == "water"  & limited_aft_2001 == "water",  "w-w",
  fifelse(limited_bef_2001 == "water"  & limited_aft_2001 == "energy", "w-e",
          fifelse(limited_bef_2001 == "energy" & limited_aft_2001 == "water",  "e-w", "e-e"))
)]
twc[, limited_change := factor(limited_change, levels = c("w-w", "w-e", "e-w", "e-e"))]

to_save <- twc[, .(lon, lat, dataset, limited_bef_2001, limited_bef_2001, limited_change)]
saveRDS(to_save, paste0(PATH_OUTPUT_DATA, 'limited_change.rds'))

table(twc[avail_change < 0, limited_change])
table(twc[avail_change > 0, limited_change])

# -------------------------------------------------------------------
# Plots / extra cross-tabs
# -------------------------------------------------------------------
ggplot(twc) +
  geom_point(aes(x = lon, y = lat, col = limited_change)) +
  coord_equal() +
  theme_minimal()

table(twc[prec_change < 0, limited_change])
table(twc[evap_change < 0, limited_change])

table(twc[prec_change > 0, limited_change])
table(twc[evap_change > 0, limited_change])

# If you meant to color by something else in the last plot, here are two useful ones:
ggplot(twc) +
  geom_point(aes(x = lon, y = lat, col = limited_aft_2001)) +
  coord_equal() +
  theme_minimal()

ggplot(twc) +
  geom_point(aes(x = lon, y = lat, col = (prec_change < 0))) +
  coord_equal() +
  theme_minimal()
