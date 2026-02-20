source('source/twc_change.R')

#Precipitation & Evaporation (core datasets)
prec_evap_raw <- readRDS(paste0(PATH_OUTPUT_RAW, 'prec_evap_raw.Rds'))

prec_evap <- prec_evap_raw[dataset %in% PREC_NAMES_SHORT | dataset %in% EVAP_NAMES_SHORT]
prec_evap[, period := ordered('bef_2001')]
prec_evap[date >= END_PERIOD_1, period := ordered('aft_2001')]

prec_evap[, date := as.numeric(format(date, "%Y"))]

prec_evap[dataset == 'MSWEP', dataset := 'GLEAM'] #GLEAM uses MSWEP
setnames(prec_evap, "date", "year")

prec_evap_wide <- dcast(prec_evap, lon + lat + dataset + year + period ~ variable, value.var = 'value')
setcolorder(prec_evap_wide, c("lon", "lat", "year", "period", "dataset", "prec", "evap"))
prec_evap_wide <- prec_evap_wide[complete.cases(prec_evap_wide),]

saveRDS(prec_evap_wide, file = paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))

prec_evap_means <- prec_evap_wide[, .(prec = mean(prec), evap = mean(evap)), .(lon, lat, dataset, period)]

prec_evap_means_wide <- dcast(prec_evap_means, lon + lat + dataset ~ period, value.var = c('prec', 'evap'))
prec_evap_means_wide[, prec_change := prec_aft_2001 - prec_bef_2001]
prec_evap_means_wide[, evap_change := evap_aft_2001 - evap_bef_2001]

prec_evap_means_wide[prec_change > 0, prec := factor("pos")]
prec_evap_means_wide[prec_change < 0, prec := factor("neg")]

prec_evap_means_wide[evap_change > 0, evap := factor("pos")]
prec_evap_means_wide[evap_change < 0, evap := factor("neg")]

prec_evap_means_wide[prec == "pos" & evap == "pos", prec_evap := factor("prec_pos-evap_pos")]
prec_evap_means_wide[prec == "pos" & evap == "neg", prec_evap := factor("prec_pos-evap_neg")]
prec_evap_means_wide[prec == "neg" & evap == "pos", prec_evap := factor("prec_neg-evap_pos")]
prec_evap_means_wide[prec == "neg" & evap == "neg", prec_evap := factor("prec_neg-evap_neg")]
prec_evap_means_wide <- prec_evap_means_wide[complete.cases(prec_evap_means_wide)]

saveRDS(prec_evap_means_wide, file = paste0(PATH_OUTPUT_DATA, 'prec_evap_change.Rds'))

#PET
pet <- read_fst(paste0(PATH_OUTPUT_RAW_OTHER, "merra2_mswx_pet_mm_1980_2024_yearly.fst" ), 
                as.data.table = TRUE)
pet <- pet[date <= END_PERIOD_2]
pet[, year := year(date)]

pet[, period := ordered('bef_2001')]
pet[year >= year(END_PERIOD_1), period := ordered('aft_2001')]

pet <- setnames(pet, c('x', 'y', 'source'), c('lon', 'lat', 'dataset'))
pet[, date := NULL]

pet <- melt(pet, id.vars = c('lon', 'lat', 'year', 'dataset', 'period'), 
            variable.name = "method", value.name = 'value')

pet_wide[, pet_change := aft_2001 - bef_2001]
setnames(pet_wide, c('bef_2001', 'aft_2001'),  c('pet_bef_2001', 'pet_aft_2001'))

pet_ensemble <- pet[, .(pet = mean(value)), .(lon, lat, period, dataset, method)]

saveRDS(pet, file = paste0(PATH_OUTPUT_DATA, 'pet.Rds'))
saveRDS(pet_ensemble, file = paste0(PATH_OUTPUT_DATA, 'pet_ensemble_periods.Rds'))

pet_stats <- pet[, .(
  pet_mean = round(mean(value, na.rm = TRUE), 1),
  pet_max = max(value,   na.rm = TRUE),
  pet_min = min(value,   na.rm = TRUE),
  n = sum(!is.na(value))
), by = .(lon, lat, period)]

pet_stats <- pet_stats[n > 200]
pet_mean_wide  <- dcast(pet_stats, lon + lat ~ period, value.var = "pet_mean")
pet_min_wide  <- dcast(pet_stats, lon + lat ~ period, value.var = "pet_min")
pet_max_wide  <- dcast(pet_stats, lon + lat ~ period, value.var = "pet_max")

setnames(pet_mean_wide,  c("bef_2001", "aft_2001"),
         c("pet_mean_bef_2001", "pet_mean_aft_2001"))
setnames(pet_min_wide,  c("bef_2001", "aft_2001"),
         c("pet_min_bef_2001", "pet_min_aft_2001"))
setnames(pet_max_wide,  c("bef_2001", "aft_2001"),
         c("pet_max_bef_2001", "pet_max_aft_2001"))

pet_final <- Reduce(
  function(x, y) merge(x, y, by = c("lon", "lat"), all = TRUE),
  list(pet_mean_wide, pet_min_wide, pet_max_wide)
)

setcolorder(pet_final, c('lon', 'lat', 'pet_min_bef_2001', 'pet_mean_bef_2001', 'pet_max_bef_2001',
                         'pet_min_aft_2001', 'pet_mean_aft_2001', 'pet_max_aft_2001'))
saveRDS(pet_final, file = paste0(PATH_OUTPUT_DATA, "pet_change.Rds"))  


#Evaluate
pet_final[, rel_range := round((pet_max_bef_2001 - pet_min_bef_2001) / pet_mean_bef_2001, 2)]
ggplot(pet_final) +
  geom_point(aes(x = lon, y = lat, col = rel_range))
pet_final[, rel_range := NULL]