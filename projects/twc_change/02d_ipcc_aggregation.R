source('source/twc_change.R')

prec_evap_change <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap_change.Rds'))[, .(
  lon, lat, dataset, prec_change, evap_change, prec, evap, prec_evap)]
avail_flux_change <- readRDS(paste0(PATH_OUTPUT_DATA, 'avail_flux_change.rds'))
limited_change <- readRDS(paste0(PATH_OUTPUT_DATA, 'limited_change.rds'))

masks <- pRecipe::pRecipe_masks()

ipcc_change <- merge(prec_evap_change, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)], 
                    by = c('lon', 'lat')) 
setnames(ipcc_change, "ipcc_short_region", "region")

ipcc_change <- merge(ipcc_change[, .(lon, lat, region, dataset, prec_change, evap_change)],
                           avail_flux_change[, .(lon, lat, dataset, flux_change, avail_change)],
                           by = c('lon', 'lat', 'dataset'))
ipcc_change <- merge(ipcc_change, limited_change)
ipcc_change[, total_grids := .N, .(dataset, region)]

towards_energy_limited <- c("w-e", "w-u", "u-e")
towards_water_limited <- c("u-w", "e-u", "e-w")

grids_towards_energy_limited <- ipcc_change[limited_change %in% towards_energy_limited]
grids_towards_energy_limited <- grids_towards_energy_limited[, .N, .(dataset, region, total_grids)]
water_energy_transition <- grids_towards_energy_limited[, .(region, dataset, to_energy = round(N/total_grids, 2))]

grids_towards_water_limited <- ipcc_change[limited_change %in% towards_water_limited]
grids_towards_water_limited <- grids_towards_water_limited[, .N, .(dataset, region, total_grids)]

water_energy_transition <- merge(water_energy_transition, 
      grids_towards_water_limited[, .(region, dataset, to_water = round(N / total_grids, 2))],
      by = c('region', 'dataset'), all = T)

water_energy_transition[is.na(to_energy), to_energy := 0]
water_energy_transition[is.na(to_water), to_water := 0]

ipcc_change_class <- merge(prec_evap_change, masks[land_mask == 'land', .(lon, lat, ipcc_short_region)], 
                           by = c('lon', 'lat')) 
setnames(ipcc_change_class, "ipcc_short_region", "region")

ipcc_change_class <- merge(ipcc_change_class[, .(lon, lat, region, dataset, prec, evap, prec_evap)],
                           avail_flux_change[, .(lon, lat, dataset, flux, avail, flux_avail)],
                           by = c('lon', 'lat', 'dataset'))
ipcc_change_class <- merge(ipcc_change_class, limited_change)


#Analysis
##Mode classes
class_cols <- c("prec", "evap", "prec_evap", "flux","avail", "flux_avail", "limited_change")

get_mode_by_dataset_region <- function(dt, col) {
  tmp <- dt[, .N, by = c("dataset","region", col)]
  setorderv(tmp, c("dataset","region","N"), c(1,1,-1))   # N descending within dataset/region
  tmp[, .SD[1], by = .(dataset, region)][, c("dataset","region", col), with = FALSE]
}

ipcc_change_class_modes_list <- lapply(class_cols, \(cc) get_mode_by_dataset_region(ipcc_change_class, cc))
ipcc_change_class_modes <- Reduce(function(x,y) merge(x,y, by = c("dataset","region"), all = TRUE), ipcc_change_class_modes_list)

#Mean values
ipcc_change_mean <- ipcc_change[, .(prec_change = mean(prec_change),
                                    evap_change = mean(evap_change), 
                                    avail_change = mean(avail_change), 
                                    flux_change = mean(flux_change),
                                    total_grids = mean(total_grids)),
                                .(dataset, region)]

ipcc_change_mean[prec_change > 0, prec := factor("pos")]
ipcc_change_mean[prec_change < 0, prec := factor("neg")]

ipcc_change_mean[evap_change > 0, evap := factor("pos")]
ipcc_change_mean[evap_change < 0, evap := factor("neg")]

ipcc_change_mean[prec == "pos" & evap == "pos", prec_evap := factor("prec_pos-evap_pos")]
ipcc_change_mean[prec == "pos" & evap == "neg", prec_evap := factor("prec_pos-evap_neg")]
ipcc_change_mean[prec == "neg" & evap == "pos", prec_evap := factor("prec_neg-evap_pos")]
ipcc_change_mean[prec == "neg" & evap == "neg", prec_evap := factor("prec_neg-evap_neg")]

ipcc_change_mean[flux_change > 0, flux := factor("accelerated")]
ipcc_change_mean[flux_change < 0, flux := factor("decelerated")]

ipcc_change_mean[avail_change > 0, avail := factor("wetter")]
ipcc_change_mean[avail_change < 0, avail := factor("drier")]

ipcc_change_mean[flux == "accelerated" & avail == "wetter", flux_avail := factor("wetter-accelerated")]
ipcc_change_mean[flux == "accelerated" & avail == "drier",  flux_avail := factor("drier-accelerated")]
ipcc_change_mean[flux == "decelerated" & avail == "wetter", flux_avail := factor("wetter-decelerated")]
ipcc_change_mean[flux == "decelerated" & avail == "drier",  flux_avail := factor("drier-decelerated")]

ipcc_change_mean <- merge(ipcc_change_mean, ipcc_change_class_modes[, .(dataset, region, limited_change)]) 
ipcc_change_mean <- merge(ipcc_change_mean, water_energy_transition, all = T)
ipcc_change_mean[is.na(to_energy), to_energy := 0]
ipcc_change_mean[is.na(to_water), to_water := 0]

ipcc_change_mean <- ipcc_change_mean[total_grids > 300] 
ipcc_change_class_modes <- ipcc_change_class_modes[region %in% unique(ipcc_change_mean$region)]

saveRDS(ipcc_change_mean, file = paste0(PATH_OUTPUT, 'ipcc_change_mean.rds'))
saveRDS(ipcc_change_class_modes, file = paste0(PATH_OUTPUT, 'ipcc_change_class_modes.rds'))

#Mean-based class differs significantly to count/mode-based class
sigcols <- c("dataset","region","prec","evap","prec_evap","flux","avail","flux_avail")

A <- unique(ipcc_change_mean[, ..sigcols])
B <- unique(ipcc_change_class_modes[, ..sigcols])
n_matches <- nrow(merge(A, B, by = sigcols))

matches <- merge(A, B, by = sigcols)
table(matches$dataset)
length(unique(ipcc_change_mean$region))

#Use mean-based class for any statement about the regional budget / water availability
#Use count/mode-based class for any statement about spatial prevalence / emergence
#Don’t choose one — report both, and treat disagreements as information
#Mismatches often mean: the region contains sub-regimes (wetting somewhere, drying elsewhere),
#changes are patchy, or extremes dominate the mean (few large cells swing the budget).
