source('source/partition_prec.R')
source('source/graphics.R')

library(corrr)
library(openair)

## Load data
prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))

## Variables
data_for_cor_temporal <- dcast(prec_annual, year ~ dataset, value.var = 'prec_mean')
data_for_cor_spatial <- dcast(prec_datasets, lon + lat ~ dataset, value.var = 'prec_mean')

taylor_obs <- prec_annual[dataset == 'gpcc', .(year, obs = prec_mean)]
data_for_taylor <- merge(taylor_obs, prec_annual[dataset != 'gpcc'], by = "year")
setnames(data_for_taylor, 'prec_mean', 'mod') 

## Analyses
### Correlation
cor_matrix_temporal <- correlate(data_for_cor_temporal[, -1])
network_plot(cor_matrix_temporal, min_cor = 0, legend = 'range')

cor_matrix_spatial <- correlate(data_for_cor_spatial[, -(1:2)])
network_plot(cor_matrix_spatial, min_cor = 0, legend = 'range')

#### Low dataset agreement and spatial correlation
data_for_cor_spatial_agree <- merge(prec_mask[, .(lon, lat, prec_quant_dataset_agreement)], data_for_cor_spatial, by = c('lon', 'lat'))
data_for_cor_spatial_low_agree <- data_for_cor_spatial_agree[prec_quant_dataset_agreement == 'low']
cor_matrix_spatial_low_agree <- correlate(data_for_cor_spatial_low_agree[, -(1:2)])
network_plot(cor_matrix_spatial_low_agree, min_cor = 0, legend = 'range')

### Taylor plot
TaylorDiagram(data_for_taylor, group = "dataset") 

taylor_obs <- prec_annual[, .(obs = mean(prec_mean, na.rm = TRUE)), year]
data_for_taylor <- merge(taylor_obs, prec_annual, by = "year")
setnames(data_for_taylor, 'prec_mean', 'mod') 
TaylorDiagram(data_for_taylor, group = "dataset") 
