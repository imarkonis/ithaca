# Figure 4 - results depend product selection ----
## Opposers, positive and negative signal boosters, no trenders
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_significance.rds"))

### Ranked data products

no_trenders <- evap_signal[variable %in%
                             c("sum_N_none_0_2", "sum_N_none_0_1", "sum_N_none_0_05", "sum_N_none_0_01")]

no_trenders[variable == "sum_N_none_0_01", variable := "p <= 0.01", ]
no_trenders[variable == "sum_N_none_0_05", variable := "p <= 0.05", ]
no_trenders[variable == "sum_N_none_0_1", variable := "p <= 0.1", ]
no_trenders[variable == "sum_N_none_0_2", variable := "p <= 0.2", ]

pos_signal <- evap_signal[variable %in%
                            c("sum_N_pos_all", "sum_N_pos_0_2", "sum_N_pos_0_1", "sum_N_pos_0_05", "sum_N_pos_0_01")]

pos_signal[variable == "sum_N_pos_0_01", variable := "p <= 0.01", ]
pos_signal[variable == "sum_N_pos_0_05", variable := "p <= 0.05", ]
pos_signal[variable == "sum_N_pos_0_1", variable := "p <= 0.1", ]
pos_signal[variable == "sum_N_pos_0_2", variable := "p <= 0.2", ]
pos_signal[variable == "sum_N_pos_all", variable := "p <= 1", ]


neg_signal <- evap_signal[variable %in%
                            c("sum_N_neg_all", "sum_N_neg_0_2", "sum_N_neg_0_1", "sum_N_neg_0_05", "sum_N_neg_0_01")]

neg_signal[variable == "sum_N_neg_0_01", variable := "p <= 0.01", ]
neg_signal[variable == "sum_N_neg_0_05", variable := "p <= 0.05", ]
neg_signal[variable == "sum_N_neg_0_1", variable := "p <= 0.1", ]
neg_signal[variable == "sum_N_neg_0_2", variable := "p <= 0.2", ]
neg_signal[variable == "sum_N_neg_all", variable := "p <= 1", ]

fig_signal_none <- ggplot(no_trenders[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top no trenders", fill = "Dataset ", y = "")+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))

fig_signal_pos <- ggplot(pos_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top positive\nsignal boosters", fill = "Dataset ", y = "")+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))

fig_signal_neg <- ggplot(neg_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top negative\nsignal boosters", fill = "Dataset ", y = "")+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))


evap_opposers[variable == "all", variable := "p <= 1"]
fig_opposers <- ggplot(evap_opposers)+
  geom_tile(aes(x = 1, y = variable, fill = dataset_leftout), color = "white", lwd = 0.8, linetype = 1)+
  geom_tile(data = evap_opposers[rank_opp < 6], aes(x = rank_opp, y = variable, fill = dataset_leftout), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top outlier", fill = "Dataset ", y = "")+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))


fig_DCI_opposer <- ggplot(evap_DCI_opposers[opposing_0_01 == 1 & rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top DCI deviators", fill = "Dataset ", y = "")+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))

fig_significance_opposers <- ggplot(evap_significance_opposers[rank_datasets < 6] )+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top signal opposer", fill = "Dataset ", y = "")+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))


ggarrange(fig_opposers, fig_DCI_opposer, fig_significance_opposers, fig_signal_pos, fig_signal_neg, fig_signal_none, align = "hv",
          common.legend = T, nrow = 2, ncol = 3, labels = c("a", "b", "c", "d", "e", "f"),
          font.label = list(size = 20))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig4_product_signals_bootstrap.png"), 
       width = 12, height = 8)
