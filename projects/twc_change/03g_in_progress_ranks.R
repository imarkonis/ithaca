source('source/geo_functions.R')

prec_mean_rank <- dataset_ranks[prec_mean_rank == 1, .(lon, lat, dataset, prec_mean_rank)]
plot_ipcc_pies(prec_mean_rank)

prec_mean_rank <- dataset_ranks[prec_sd_rank == 1, .(lon, lat, dataset, prec_sd_rank)]
plot_ipcc_pies(prec_mean_rank)

prec_mean_rank <- dataset_ranks[evap_mean_rank == 1, .(lon, lat, dataset, evap_mean_rank)]
plot_ipcc_pies(prec_mean_rank)

prec_mean_rank <- dataset_ranks[evap_sd_rank == 1, .(lon, lat, dataset, evap_sd_rank)]
plot_ipcc_pies(prec_mean_rank)

prec_mean_rank <- dataset_ranks[pe_ratio_check == T, .(lon, lat, dataset, pe_ratio_check)]
plot_ipcc_pies(prec_mean_rank)

prec_mean_rank <- dataset_ranks[prec_check_significance  == T, .(lon, lat, dataset, prec_check_significance)]
plot_ipcc_pies(prec_mean_rank)

prec_mean_rank <- dataset_ranks[prec_rank_slope  == 1, .(lon, lat, dataset, prec_check_significance)]
plot_ipcc_pies(prec_mean_rank)

prec_mean_rank <- dataset_ranks[evap_rank_slope  == 1, .(lon, lat, dataset, prec_check_significance)]
plot_ipcc_pies(prec_mean_rank)

aa <- dataset_ranks[dataset == 'ERA5L'][, 1:7]
aa_melt <- melt(aa, id.vars = c("lon", "lat", "dataset"))
aa_melt[, dataset := NULL]
plot_ipcc_pies(aa_melt[value == 1]) 

aa <- dataset_ranks[dataset == 'ERA5L'][, c(1:7, 11, 14)]
aa_melt <- melt(aa, id.vars = c("lon", "lat", "dataset"))
aa_melt[, dataset := NULL]
plot_ipcc_pies(aa_melt[value == 1]) 

aa <- dataset_ranks[dataset == 'TERRA'][, c(1:7, 11, 14)]
aa_melt <- melt(aa, id.vars = c("lon", "lat", "dataset"))
aa_melt[, dataset := NULL]
plot_ipcc_pies(aa_melt[value == 1]) 

dataset_ranks_melt <- melt(dataset_ranks[, c(1:7, 11, 14)], id.vars = c("lon", "lat", "dataset"))
oo <- dataset_ranks_melt[value < 3, .N, .(dataset, variable)]
oo[order(dataset)]

dataset_ranks_melt <- melt(dataset_ranks[prec_mean_rank <= 3 &
                                           prec_sd_rank <= 3 &
                                           evap_mean_rank <= 3 &
                                           evap_sd_rank <= 3 &
                                           prec_rank_slope <= 3 &
                                           evap_rank_slope <= 3, 
                                         c(1:7, 11, 14)], id.vars = c("lon", "lat", "dataset"))
oo <- dataset_ranks_melt[value < 3, .N, .(dataset, variable)]
oo[order(dataset)]

ggplot(dataset_ranks_melt[dataset == 'TERRA']) +
  geom_point(aes(x = lon, y = lat))

ggplot(dataset_ranks_melt[dataset == 'GLEAM']) +
  geom_point(aes(x = lon, y = lat))

ggplot(dataset_ranks_melt[dataset == 'ERA5L']) +
  geom_point(aes(x = lon, y = lat))

ggplot(dataset_ranks_melt[dataset == 'FLDAS']) +
  geom_point(aes(x = lon, y = lat))

ggplot(dataset_ranks_melt[dataset == 'MERRA']) +
  geom_point(aes(x = lon, y = lat))

