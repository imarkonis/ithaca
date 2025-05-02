# Figure 3 - results depend product selection ----
## Opposers, positive and negative signal boosters, no trenders
source('source/changing_prec.R')
source('source/geo_functions.R')

# Data
prec_signal <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_i_global_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
prec_opposers <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_i_global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))

#### Ranked data products

no_trenders <- prec_signal[variable %in%
                             c("sum_N_none_0_2", "sum_N_none_0_1", "sum_N_none_0_05", "sum_N_none_0_01")]

no_trenders[variable == "sum_N_none_0_01", variable := "p < 0.01", ]
no_trenders[variable == "sum_N_none_0_05", variable := "p < 0.05", ]
no_trenders[variable == "sum_N_none_0_1", variable := "p < 0.1", ]
no_trenders[variable == "sum_N_none_0_2", variable := "p < 0.2", ]

pos_signal <- prec_signal[variable %in%
                            c("sum_N_pos_all", "sum_N_pos_0_2", "sum_N_pos_0_1", "sum_N_pos_0_05", "sum_N_pos_0_01")]

pos_signal[variable == "sum_N_pos_0_01", variable := "p < 0.01", ]
pos_signal[variable == "sum_N_pos_0_05", variable := "p < 0.05", ]
pos_signal[variable == "sum_N_pos_0_1", variable := "p < 0.1", ]
pos_signal[variable == "sum_N_pos_0_2", variable := "p < 0.2", ]
pos_signal[variable == "sum_N_pos_all", variable := "p <=1", ]


neg_signal <- prec_signal[variable %in%
                            c("sum_N_neg_all", "sum_N_neg_0_2", "sum_N_neg_0_1", "sum_N_neg_0_05", "sum_N_neg_0_01")]

neg_signal[variable == "sum_N_neg_0_01", variable := "p < 0.01", ]
neg_signal[variable == "sum_N_neg_0_05", variable := "p < 0.05", ]
neg_signal[variable == "sum_N_neg_0_1", variable := "p < 0.1", ]
neg_signal[variable == "sum_N_neg_0_2", variable := "p < 0.2", ]
neg_signal[variable == "sum_N_neg_all", variable := "p <=1", ]

fig_signal_none <- ggplot(no_trenders[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top no trenders", fill = "Dataset ", y = "")+
  theme_minimal()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

fig_signal_pos <- ggplot(pos_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top Positive\nSignal Boosters", fill = "Dataset ", y = "")+
  theme_minimal()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

fig_signal_neg <- ggplot(neg_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top Negative\nSignal Boosters", fill = "Dataset ", y = "")+
  theme_minimal()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


prec_opposers[variable == "all", variable := "p <= 1"]
fig_opposers <- ggplot(prec_opposers)+
  geom_tile(aes(x = 1, y = variable, fill = dataset_leftout))+
  geom_tile(data = prec_opposers[rank_opp < 6], aes(x = rank_opp, y = variable, fill = dataset_leftout))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top opposers", fill = "Dataset ", y = "")+
  theme_minimal()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


ggarrange(fig_opposers, fig_signal_pos, fig_signal_neg, fig_signal_none, align = "hv",
          common.legend = T, nrow = 1, ncol = 4, labels = c("a", "b", "c", "d"))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_MAIN, "fig3_product_signals_bootstrap.png"), 
       width = 12, height = 4)
