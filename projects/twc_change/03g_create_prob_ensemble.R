dataset_weights <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_weights.Rds'))

n_members <- 100

sample_ensemble <- function(values, weights, n = 100) {
  sample(values, size = n, replace = TRUE, prob = weights)
}

prob_ensemble <- dataset_weights[, {
  sampled <- sample_ensemble(dataset, weight, n_members)
  .(member = 1:n_members, dataset = sampled)
}, by = .(lon, lat)]

saveRDS(prob_ensemble, file.path(PATH_OUTPUT_DATA, 'prob_ensemble.Rds'))
