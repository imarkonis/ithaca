source('source/twc_change.R')

prec_evap_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_evap_stats.Rds'))
pet_mean <- readRDS(file.path(PATH_OUTPUT_DATA, 'pet_mean.Rds'))
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))

setnames(pet_mean, 'dataset', 'dataset_temp')
dummy <- pet_mean[prec_evap_stats,
  on = .(lon, lat),
  allow.cartesian = TRUE,
  nomatch = 0]

pet_check <- dummy[, .(n_below_pet = sum(evap_mean < value, na.rm = TRUE)),
                   by = .(lon, lat, dataset)]

dataset_ranks <- dataset_ranks[pet_check, 
          on = .(lon, lat, dataset)]

saveRDS(dataset_ranks, file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))    

ggplot(pet_check) +
  geom_point(aes(x = lon, y = lat, col = n_below_pet)) +
  facet_wrap(~dataset)

ggplot(pet_check) +
  geom_histogram(aes(x = n_below_pet)) +
  facet_wrap(~dataset)


