# figure 4
source('source/evap_trend.R')

## CSI BIAS ----
CSI_BIAS_data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap_all_p.rds"))
CSI_BIAS_data[, BIAS_brks_0_01 := cut(BIAS0_01, breaks = c(round(1/14, 2),1/4,1/2,round(1/1.2, 2),1.2,2,4,14))]

CSI_BIAS_data[, BIAS_brks_0_05 := cut(BIAS0_05, breaks = c(round(1/14, 2),1/4,1/2,round(1/1.2, 2),1.2,2,4,14))]

CSI_BIAS_data[, BIAS_brks_0_1 := cut(BIAS0_1, breaks = c(round(1/14, 2),1/4,1/2,round(1/1.2, 2),1.2,2,4,14))]

CSI_BIAS_data[, BIAS_brks_0_2 := cut(BIAS0_2, breaks = c(round(1/14, 2),1/4,1/2,round(1/1.2, 2),1.2,2,4,14))]

CSI_BIAS_data[, BIAS_brks_all := cut(BIAS, breaks = c(round(1/14, 2),1/4,1/2,round(1/1.2, 2),1.2,2,4,14))]

CSI_BIAS_data[, CSI_brks := cut(CSI, breaks = c(1/1000, 1/10, 2/10, 3/10, 5/10, 1))]
CSI_BIAS_data[CSI < 0.1, CSI_fac := "< 10 %"]
CSI_BIAS_data[CSI >= 0.1 & CSI < 0.2, CSI_fac := "< 20 %"]
CSI_BIAS_data[CSI >= 0.2 & CSI < 0.3, CSI_fac := "< 30 %"]
CSI_BIAS_data[CSI >= 0.3 & CSI < 0.5, CSI_fac := "< 50 %"]
CSI_BIAS_data[CSI >= 0.5, CSI_fac := ">= 50 %"]

CSI_BIAS_data[, CSI_brks_0_01 := cut(CSI0_01, breaks = c(1/1000, 1/10, 2/10, 3/10, 5/10, 1))]
CSI_BIAS_data[CSI0_01 < 0.1, CSI_fac_0_01 := "< 10 %"]
CSI_BIAS_data[CSI0_01 >= 0.1 & CSI0_01 < 0.2, CSI_fac_0_01 := "< 20 %"]
CSI_BIAS_data[CSI0_01 >= 0.2 & CSI0_01 < 0.3, CSI_fac_0_01 := "< 30 %"]
CSI_BIAS_data[CSI0_01 >= 0.3 & CSI0_01 < 0.5, CSI_fac_0_01 := "< 50 %"]
CSI_BIAS_data[CSI0_01 >= 0.5, CSI_fac_0_01 := ">= 50 %"]

CSI_BIAS_data[, CSI_brks_0_05 := cut(CSI0_05, breaks = c(1/1000, 1/10, 2/10, 3/10, 5/10, 1))]
CSI_BIAS_data[CSI0_05 < 0.1, CSI_fac_0_05 := "< 10 %"]
CSI_BIAS_data[CSI0_05 >= 0.1 & CSI0_05 < 0.2, CSI_fac_0_05 := "< 20 %"]
CSI_BIAS_data[CSI0_05 >= 0.2 & CSI0_05 < 0.3, CSI_fac_0_05 := "< 30 %"]
CSI_BIAS_data[CSI0_05 >= 0.3 & CSI0_05 < 0.5, CSI_fac_0_05 := "< 50 %"]
CSI_BIAS_data[CSI0_05 >= 0.5, CSI_fac_0_05 := ">= 50 %"]

CSI_BIAS_data[, CSI_brks_0_1 := cut(CSI0_1, breaks = c(1/1000, 1/10, 2/10, 3/10, 5/10, 1))]
CSI_BIAS_data[CSI0_1 < 0.1, CSI_fac_0_1 := "< 10 %"]
CSI_BIAS_data[CSI0_1 >= 0.1 & CSI0_1 < 0.2, CSI_fac_0_1 := "< 20 %"]
CSI_BIAS_data[CSI0_1 >= 0.2 & CSI0_1 < 0.3, CSI_fac_0_1 := "< 30 %"]
CSI_BIAS_data[CSI0_1 >= 0.3 & CSI0_1 < 0.5, CSI_fac_0_1 := "< 50 %"]
CSI_BIAS_data[CSI0_1 >= 0.5, CSI_fac_0_1 := ">= 50 %"]

CSI_BIAS_data[, CSI_brks_0_2 := cut(CSI0_2, breaks = c(1/1000, 1/10, 2/10, 3/10, 5/10, 1))]
CSI_BIAS_data[CSI0_2 < 0.1, CSI_fac_0_2 := "< 10 %"]
CSI_BIAS_data[CSI0_2 >= 0.1 & CSI0_2 < 0.2, CSI_fac_0_2 := "< 20 %"]
CSI_BIAS_data[CSI0_2 >= 0.2 & CSI0_2 < 0.3, CSI_fac_0_2 := "< 30 %"]
CSI_BIAS_data[CSI0_2 >= 0.3 & CSI0_2 < 0.5, CSI_fac_0_2 := "< 50 %"]
CSI_BIAS_data[CSI0_2 >= 0.5, CSI_fac_0_2 := ">= 50 %"]


fig_CSI <-ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_fac))+
  scale_fill_manual(values = c("lightcoral", "darkred", "#330000"))+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  guides(fill = "none")+
  labs(x = "Dataset B", y = "Dataset A", fill = "Critical\nSuccess Index")+
  ggtitle("P-value <= 1")

fig_CSI_0_2 <-ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_fac_0_2))+
  scale_fill_manual(values = c("gold", "darkorange",  "lightcoral", "darkred","#330000"))+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Critical\nSuccess Index")+
  ggtitle("P-value <= 0.2")


fig_CSI_0_1 <-ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_fac_0_1))+
  scale_fill_manual(values = c("gold", "darkorange",  "lightcoral", "darkred","#330000"))+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Critical\nSuccess Index   ")+
  ggtitle("P-value <= 0.1")

fig_CSI_0_05 <-ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_fac_0_05))+
  scale_fill_manual(values = c("gold", "darkorange",  "lightcoral", "darkred","#330000"))+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  guides(fill = "none")+
  labs(x = "Dataset B", y = "Dataset A", fill = "Critical\nSuccess Index")+
  ggtitle("P-value <= 0.05")


fig_CSI_0_01 <-ggplot(CSI_BIAS_data[CSI0_01 < 1])+
    geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_fac_0_01))+
                scale_fill_manual(values = c("gold", "darkorange",  "lightcoral", "darkred","#330000"))+
                theme_bw()+
                theme(axis.ticks.length = unit(0, "cm"),
                      panel.grid.major = element_line(colour = "gray60"),
                      axis.title = element_text(size = 16), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 16))+
                theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
                labs(x = "Dataset B", y = "Dataset A", fill = "Critical\nSuccess Index")+
  guides(fill = "none")+
  ggtitle("P-value <= 0.01")
              
              

fig_BIAS_0_2 <- ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS_brks_0_2))+
  scale_fill_manual(values = c("royalblue3", "lightblue", "gray90","orange","darkorange","darkred"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Bias")+
  guides(fill = "none")+
  ggtitle("P-value <= 0.2")

fig_BIAS_0_1 <- ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS_brks_0_1))+
  scale_fill_manual(values = c("royalblue3", "lightblue", "gray90","orange","darkorange","darkred"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Bias")+
  guides(fill = "none")+
  ggtitle("P-value <= 0.1")

fig_BIAS_0_05 <- ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS_brks_0_05))+
  scale_fill_manual(values = c("darkblue", "royalblue3", "lightblue", "gray90","orange","darkorange","darkred"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Bias")+
  guides(fill = guide_legend(nrow=1, byrow=TRUE))+ 
  ggtitle("P-value <= 0.05")

fig_BIAS_0_01 <- ggplot(CSI_BIAS_data[CSI0_01 < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS_brks_0_01))+
  scale_fill_manual(values = c("darkblue", "royalblue3", "lightblue", "gray90","orange","darkorange","darkred"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  labs(x = "", y = "Dataset A", fill = "Bias            ")+
  ggtitle("P-value <= 0.01")

fig_CSI_col <- ggarrange( fig_CSI_0_01,  fig_CSI_0_05, fig_CSI_0_1, fig_CSI_0_2, align = "hv", 
          labels = c("a", "b", "c", "d"), ncol = 2, nrow = 2, common.legend = T, legend = "right")

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_CSI_p_val.png"), 
       width = 12, height = 12)

fig_BIAS_col <-ggarrange(fig_BIAS_0_01, fig_BIAS_0_05,  fig_BIAS_0_1, fig_BIAS_0_2, align = "hv", 
          labels = c("a", "b", "c", "d"), ncol = 2, nrow = 2, common.legend = T, legend = "right")

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig5_SI_BIAS_p_val.png"), 
       width = 12, height = 12)
