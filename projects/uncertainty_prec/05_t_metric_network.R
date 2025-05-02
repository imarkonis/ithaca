# Supplementary figure: Correlation network
source("source/uncertainty_prec.R")

install.packages(setdiff(c("corrr", "reshape2"),
                         rownames(installed.packages())))

library(corrr)
library(reshape2)
options(ggrepel.max.overlaps = Inf)

registerDoParallel(cores = N_CORES)

## Load data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_roi.rds"))

DATASETS <- unique(prec_data$dataset)

## Analyses
prec_time_series <- foreach(data_count = 1:length(DATASETS), .combine = rbind) %do% {
  dummie <- prec_data[dataset == DATASETS[data_count]
                      ][, .(lon, lat, date, value = prec)]
  dummie <- fldmean(dummie)
  dummie$dataset <- DATASETS[data_count]
  return(dummie)
}

saveRDS(prec_time_series, paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                 "prec_time_series.rds"))

rm(prec_data)

ALL_COMBS <- combn(DATASETS, 2, simplify = FALSE)

cor_matrix <- foreach(idx = 1:length(ALL_COMBS), .combine = rbind) %dopar% {
  dummie <- merge(prec_time_series[dataset == ALL_COMBS[[idx]][1]],
                  prec_time_series[dataset == ALL_COMBS[[idx]][2]], by = "date")
  dummie <- dummie[, .(mse_prec = mean((value.y - value.x)^2, na.rm = TRUE),
                       r_prec = cor(value.x, value.y, use = "pairwise.complete.obs"),
                       bias_prec = mean(value.x, na.rm = TRUE) - mean(value.y, na.rm = TRUE),
                       var_prec = sd(value.x, na.rm = TRUE)^2,
                       median_var = sd(value.y, na.rm = TRUE)^2)]
  
  dummie <- dummie[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var))),
                       dataset_1 = ALL_COMBS[[idx]][1],
                       dataset_2 = ALL_COMBS[[idx]][2])]
  
  return(dummie)
}

cor_matrix_b <- copy(cor_matrix)
setnames(cor_matrix_b, c("dataset_1", "dataset_2"), c("dataset_2", "dataset_1"))

cor_matrix <- rbind(cor_matrix, cor_matrix_b)

cor_matrix[t_prec < 0, t_prec := 0]

cor_matrix <- acast(cor_matrix, dataset_1 ~ dataset_2, value.var = 't_prec')

## Plot
png(filename = paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                      "cor_network.png"), units = "cm", res = 600,
    width = 18*GOLDEN_RATIO, height = 18)
network_plot(cor_matrix, min_cor = 0, legend = 'range',
             colors = hcl.colors(10, "viridis"))
dev.off()

