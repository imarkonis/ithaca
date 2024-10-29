# Dataset trend averages per p-value group ----
source('source/evap_trend.R')
source('source/geo_functions.R')

evap_trend_averages <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_average_trend_per_pval_groupe.rds")) 

trends <- unlist(evap_trend_averages[pval_brk == levels(evap_trend_averages$pval_brk)[1], 
                                     .(value = mean_slope)])

# estimate trend combinations
trend_combn <- lapply(1:sample_size, function(k) {
  combn(trends, k, function(x) mean(x))
})

all_trend_combn <- unlist(trend_combn)
plot(density(trend_combn[[2]]))
plot(density(unlist(trend_combn)))

prob_positive <- length(all_trend_combn[all_trend_combn > 0]) / length(all_trend_combn)

prob_positive <- data.table(n_dataset = 1:sample_size, 
                            prob = sapply(trend_combn, function(x){
                              length(x[x > 0]) / length(x)
                            })
)      

ggplot(prob_positive) +
  geom_line(aes(x = n_dataset, y = prob)) +
  geom_point(aes(x = n_dataset, y = prob)) +
  theme_light()
