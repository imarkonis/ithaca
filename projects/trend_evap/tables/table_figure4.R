# Tables for figure 4 for Zenodo ----
source('source/evap_trend.R')

evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))

write.table(evap_signal, paste0(PATH_SAVE_EVAP_TREND_TABLES, "dataset_rank_signal_positivie_negative_booster_no_trenders.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))

write.table(evap_opposers, paste0(PATH_SAVE_EVAP_TREND_TABLES, "dataset_rank_opposing_trend.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_DCI.rds"))

write.table(evap_DCI_opposers, paste0(PATH_SAVE_EVAP_TREND_TABLES, "dataset_rank_opposing_DCI.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_significance.rds"))

write.table(evap_significance_opposers, paste0(PATH_SAVE_EVAP_TREND_TABLES, "dataset_rank_opposing_significance.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
