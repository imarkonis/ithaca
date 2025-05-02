# figure 5 ----
source('source/evap_trend.R')

## CSI BIAS ----
CSI_BIAS_data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap.rds"))


CSI_BIAS_data[, dataset_A := toupper(dataset_A)]
CSI_BIAS_data[dataset_A == "ETMONITOR", dataset_A := "ETMonitor"]
CSI_BIAS_data[dataset_A == "SYNTHESIZEDET", dataset_A := "SynthesizedET"]
CSI_BIAS_data[dataset_A == "ERA5-LAND", dataset_A := "ERA5-land"]
CSI_BIAS_data[dataset_A == "MERRA2", dataset_A := "MERRA-2"]
CSI_BIAS_data[dataset_A == "JRA55", dataset_A := "JRA-55"]
CSI_BIAS_data[dataset_A == "TERRACLIMATE", dataset_A := "TerraClimate"]

CSI_BIAS_data[, dataset_B := toupper(dataset_B)]
CSI_BIAS_data[dataset_B == "ETMONITOR", dataset_B := "ETMonitor"]
CSI_BIAS_data[dataset_B == "SYNTHESIZEDET", dataset_B := "SynthesizedET"]
CSI_BIAS_data[dataset_B == "ERA5-LAND", dataset_B := "ERA5-land"]
CSI_BIAS_data[dataset_B == "MERRA2", dataset_B := "MERRA-2"]
CSI_BIAS_data[dataset_B == "JRA55", dataset_B := "JRA-55"]
CSI_BIAS_data[dataset_B == "TERRACLIMATE", dataset_B := "TerraClimate"]

CSI_BIAS_data[, BIAS_brks := cut(BIAS, breaks = c(round(1/6.1, 2),1/4,1/2,round(1/1.2, 2),1.2,2,4,6.02))]

CSI_BIAS_data[, CSI_brks := cut(CSI, breaks = c(1/1000,1/10,2/10,3/10,1))]
CSI_BIAS_data[CSI < 0.1, CSI_fac := "< 10 %"]
CSI_BIAS_data[CSI >= 0.1 & CSI < 0.2, CSI_fac := "< 20 %"]
CSI_BIAS_data[CSI >= 0.2 & CSI < 0.3, CSI_fac := "< 30 %"]
CSI_BIAS_data[CSI >= 0.3, CSI_fac := ">= 30 %"]

fig_CSI <-ggplot(CSI_BIAS_data[CSI < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_fac), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = c("gold", "darkorange",  "lightcoral", "darkred"))+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(plot.margin = unit(c(0.3,0.3,0,0.8), 'cm'))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Critical\nsuccess index")

fig_BIAS <- ggplot(CSI_BIAS_data[CSI < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS_brks), color = "white", lwd = 0.8, linetype = 1)+
  scale_fill_manual(values = c("darkblue", "royalblue3", "lightblue", "gray90","orange","darkorange","darkred"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16))+
  theme(plot.margin = unit(c(0.3,0.3,0,0.8), 'cm'))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Bias")

ggarrange(fig_CSI, fig_BIAS, align = "hv", labels = c("a", "b"), nrow = 2, font.label = list(size = 20))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig5_CSI_BIAS.png"), 
       width = 12, height = 18)
