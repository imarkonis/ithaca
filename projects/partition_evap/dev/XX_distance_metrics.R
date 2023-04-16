## Mahalanobis and Hellinger distances
library(statip)
data_for_distance$mahalanobis <- mahalanobis(data_for_distance[, -(1:2)], 
                                             colMeans(data_for_distance[, -(1:2)]), 
                                             cov(data_for_distance[, -(1:2)]))

dummy <- merge(prec_stats, data_for_distance, by = c("lon", "lat"))
dummy <- merge(dummy, prec_mask[, .(lon, lat, rel_dataset_agreement)], by = c("lon", "lat"))

distances <- combn(names(data_for_distance_temporal), 2, function(x) 
  hellinger(data_for_cor_spatial[[x[1]]], data_for_cor_spatial[[x[2]]]))

ggplot(dummy) +
  geom_point(aes(x = mahalanobis, y = std_quant_range, col = rel_dataset_agreement)) + 
  theme_bw()

ggplot(dummy) +
  geom_point(aes(x = std_quant_range, y = ens_mean_cv, col = rel_dataset_agreement)) + 
  theme_bw()