## Table S2 and S3 ----
source("source/partition_evap.R")

evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(evap_mask$evap_quant_dataset_agreement) <- c("High", "Above average", "Average",
                                                    "Below average", "Low")


distribution <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise.rds")))
distribution <-  distribution[!is.na(index),]
distribution[, summary(index)]

### Distribution agreement thresholds at quantile 0.1, 0.3, 0.7. 0.9
quant_dist_0_1 <- quantile(distribution$index, c(0.1))
quant_dist_0_3 <- quantile(distribution$index, c(0.3))
quant_dist_0_7 <- quantile(distribution$index, c(0.7))
quant_dist_0_9 <- quantile(distribution$index, c(0.9))

### Relative dataset agreement thresholds at quantile 0.1, 0.3, 0.7. 0.9
quant_quart_0_1 <- quantile(evap_mask$std_quant_range, c(0.1))
quant_quart_0_3 <- quantile(evap_mask$std_quant_range, c(0.3))
quant_quart_0_7 <- quantile(evap_mask$std_quant_range, c(0.7))
quant_quart_0_9 <- quantile(evap_mask$std_quant_range, c(0.9))

