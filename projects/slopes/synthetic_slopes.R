install.packages('CoSMoS')
library(CoSMoS)
library(data.table)

## compute VAR model parameters  
sample_size <- 14
n_years <- 20
coord <- cbind(runif(sample_size)*30, runif(sample_size)*30)

## compute VAR model parameters  
fit <- fitVAR(spacepoints = coord, 
              p = 4, 
              p0 = 0,
              margdist = "norm",
              margarg = list(20, 5), 
              stcsid = "clayton",
              stcsarg = list(scfid = "weibull", tcfid = "weibull", copulaarg = 2,
                             scfarg = list(scale = 25, shape = 0.7), 
                             tcfarg = list(scale = 3.1, shape = 0.8) ) )

## generate correlated timeseries  
set.seed(1939)
sim <- generateMTS(n = n_years, STmodel = fit)

## create trends
pos_trend <- seq(from = 0, to = 15, length.out = n_years) 
neg_trend <- seq(from = 0, to = -15, length.out = n_years)

sim[, 1:3] <- sim[, 1:3] + pos_trend
sim[, 5] <- sim[, 5] + neg_trend

## visualize simulated timeseries
dta <- melt(data = data.table(time = 1:nrow(sim), sim[,1:sample_size]), id.vars = "time")

ggplot(data = dta, mapping = aes(x = time, y = value)) + geom_line() +
  facet_grid(facets = variable ~ ., scales = "free_y") + theme_light()

## estimate trends
formulas <- list(time ~ value)
fit_lm <- dta[, lapply(formulas, function(x) list(lm(x, data = .SD))), by = variable]
trends <- unlist(lapply(fit_lm$V1, coef))[seq(from = 2, to = 2 * sample_size, by = 2)]

# estimate trend combinations
trend_combn <- lapply(1:sample_size, function(k) {
  combn(trends, k, function(x) mean(x))
})

trend_combn
all_trend_combn <- unlist(trend_combn)
mean(all_trend_combn)
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


