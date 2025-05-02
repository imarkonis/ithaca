# Figures showing p-value dependency of ----
# 1. Area fraction of DCI
# 2. Area fraction of positive, negative and no trend for each p-value group
# 3. Area fraction of number significant signs for each p-value group
# 4. Global map showing p-value at which opposing trends occur

## Source ----
source('source/changing_prec.R')
source('source/geo_functions.R')

## Data ----
prec_index <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_g_global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

## Figures -----
### Figure 1 DCI area fraction ----
#### Figure 1 prep ----

data_sel <- subset(prec_index, select = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all", "lon", "lat"))
setnames(data_sel, old = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_melt <- melt(data_sel, 
                      measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"), 
                      id.vars = c("lon", "lat"))

data_sel_melt[, DCI_brk := cut(value, c(-1.01, -0.75,-0.5,-0.07, 0.07,0.5, 0.75,1))]
grid_cell_area <- unique(data_sel_melt[, .(lon, lat)]) %>% grid_area() # m2
data_sel_melt <- grid_cell_area[data_sel_melt, on = .(lon, lat)]

data_dci_area <- data_sel_melt[, .(DCI_area = sum(area)), .(DCI_brk, variable)]
data_dci_area[, total_area := sum(DCI_area), variable]
data_dci_area[, fraction := DCI_area/total_area]
data_dci_area[DCI_brk == "(-1.01,-0.75]", DCI_brk := "[-1,-0.75]"]
data_dci_area[, DCI_brk:= factor(DCI_brk, levels = c("[-1,-0.75]", "(-0.75,-0.5]",
                                                       "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                       "(0.5,0.75]", "(0.75,1]"))]


#### Figure 1 gg ----

fig_DCI <- ggplot(data_dci_area)+
  geom_bar(aes(x = variable, y = fraction, fill = DCI_brk), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("darkblue", "royalblue2", "lightblue", "gray90", "orange","lightcoral","darkred"))+
  labs(y = "Area Fraction", x = "P-value Threshold", fill = "DCI")+
  theme(legend.position="bottom")+guides(fill = guide_legend(nrow = 3 ,byrow = TRUE))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_n_bar_area_fraction_DCI_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)

### Figure 2 trend direction ----
#### Figure 2 prep ----
data_sel_trend <- subset(prec_index, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
data_sel_trend  <- grid_cell_area[data_sel_trend, on = .(lon, lat)]

setnames(data_sel_trend, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_trend_melt <- melt(data_sel_trend, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_trend_area <- data_sel_trend_melt[, .(trend_area = sum(area)), .(value, variable)] 
data_sel_trend_area[, total_area := sum(trend_area), variable]
data_sel_trend_area[, fraction := trend_area/total_area]

#### Figure 2 gg ----

fig_trend <- ggplot(data_sel_trend_area)+
  geom_bar(aes(x = variable, y = fraction, fill = value), stat = "identity")+
  scale_fill_manual(values = c ("darkblue", "gray90", "yellow","darkred"))+
  theme_bw()+
  labs(x = "P-value Threshold", y = "Area fraction [-]", fill = "Direction of\nSignificant\nTrends")+
  theme(legend.position="bottom")+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_n_bar_area_fraction_significant_trend_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)

### Figure 3 Number of significant trends over p ----
### Product selection: Trend or no trend

#### Figure 3 prep ----
prec_index[, N_sum_0_01:= N_pos_0_01+N_neg_0_01]
prec_index[, N_sum_0_05:= N_pos_0_05+N_neg_0_05]
prec_index[, N_sum_0_1:= N_pos_0_1+N_neg_0_1]
prec_index[, N_sum_0_2:= N_pos_0_2+N_neg_0_2]
prec_index[, N_sum_all:= N_pos_all+N_neg_all]

prec_sel <- subset(prec_index, select = c("N_sum_0_01", "N_sum_0_05", 
                                          "N_sum_0_1", "N_sum_0_2", "N_sum_all",
                                          "lat", "lon"))
prec_sel  <- grid_cell_area[prec_sel, on = .(lon, lat)]

setnames(prec_sel , old = c("N_sum_0_01", "N_sum_0_05", 
                            "N_sum_0_1", "N_sum_0_2", "N_sum_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_melt <- melt(prec_sel, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_melt[, N_sum_brk := cut(round(value), c(-0.1, 0.9, 3, 6, 9, 12, 14))]
data_melt[N_sum_brk == "(-0.1,0.9]", N_sum_brk := "[0,1)"]
data_melt[N_sum_brk == "(0.9,3]", N_sum_brk := "[1,3]"]

N_sig_area <- data_melt[,.(N_sig_area = sum(area)), .(N_sum_brk, variable)]
N_sig_area[, variable_area := sum(N_sig_area), variable]
N_sig_area[, fraction := N_sig_area/variable_area]
N_sig_area[, N_sum_brk:= factor(N_sum_brk, levels = c("[0,1)", "[1,3]",
                                                      "(3,6]", "(6,9]",
                                                      "(9,12]","(12,14]"))]

fig_Ntrend <- ggplot(N_sig_area)+
  geom_bar(aes(x = variable, y = fraction, fill = N_sum_brk), stat = "identity")+
  theme_bw()+
  labs( fill = "Number of\nSignificant\nTrends", x = 'P-value Threshold', y = "Area Fraction [-]")+
  scale_fill_manual(values = c("gray90", "lightblue", "steelblue1","royalblue1", "royalblue3", "darkblue"))+
  theme(legend.position="bottom")+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_n_bar_area_fraction_N_significant_trend_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)



### Composite figure ----

ggarrange(fig_Ntrend, fig_trend, fig_DCI, align = "hv",
          ncol = 3, nrow = 1,
          labels = c("a", "b", "c"))
ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_n_area_fraction_trend_indices_p_val_threshold_bootstrap.png"), 
       width = 14, height = 6)

