# Tables for figure 5 for Zenodo ----
source('source/evap_trend.R')

CSI_BIAS_data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap_all_p.rds"))

write.table(CSI_BIAS_data , paste0(PATH_SAVE_EVAP_TREND_TABLES, "CSI_BIAS_across_pval.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
