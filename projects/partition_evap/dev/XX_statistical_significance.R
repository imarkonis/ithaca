# Example for the estimation of statistical significance in change
source('source/partition_prec.R')
source('source/graphics.R')

prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

dummy_raster <- prec_2000_2019[[1]]

dummy <- data.table(time = seq(ymd("2000-01-01"), ymd("2019-12-01"), by = 'year'))
dummy[, sum_annual := cellStats(dummy_raster, "mean")]
dummy_annual <- dummy[, unique(sum_annual)]
obs_mean_diff <-  mean(dummy_annual[11:20]) -  mean(dummy_annual[1:10])

sample_size <- 10000
mc_sample <- replicate(sample_size, sample(dummy_annual, 20, replace = TRUE))
mc_sample_means <- data.table(annual_diff = apply(mc_sample[1:10, ], 2, mean) - 
                                apply(mc_sample[11:20, ], 2, mean))

prob_exceedence <- nrow(mc_sample_means[annual_diff > obs_mean_diff]) / sample_size

ggplot(mc_sample_means) +
  geom_density(aes(annual_diff)) +
  geom_vline(aes(xintercept = obs_mean_diff), col = 'red') +
  theme_bw()


