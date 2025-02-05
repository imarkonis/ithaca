# Dataset trend averages per p-value group ----
source('source/evap_trend.R')
source('source/geo_functions.R')

evap_trend_averages <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_average_trend_per_pval_groupe.rds")) 

trends <- unlist(evap_trend_averages[pval_brk == levels(evap_trend_averages$pval_brk)[3], 
                                     .(value = mean_slope)])
trends <- evap_trend_averages[, .(dataset, pval_brk, value = mean_slope)]

# Calculate averages for all possible combinations for each category
averages_by_category <- trends[, .(Combinations = lapply(1:5, function(k) {
  combn(value, k, simplify = FALSE)
}),
Averages = lapply(1:5, function(k) {
  combn(value, k, mean)
})), by = dataset]

# Unnest the lists of combinations and their corresponding averages
expanded_results <- averages_by_category[, .(Combination = unlist(Combinations, recursive = FALSE),
                                             Average = unlist(Averages)), by = dataset]


# estimate positive trend probability
prob_positive <- length(all_trend_combn[all_trend_combn > 0]) / length(all_trend_combn)

prob_positive <- data.table(n_dataset = 1:sample_size, 
                            prob = sapply(trend_combn, function(x){
                              length(x[x > 0]) / length(x)
                            })
)      

ggplot(prob_positive) +
  geom_line(aes(x = n_dataset, y = prob)) +
  geom_point(aes(x = n_dataset, y = prob)) +
  xlab("Number of datasets") +
  ylab("Probability positive (%)") +
  theme_light()
