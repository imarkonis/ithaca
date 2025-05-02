# Figures showing p-value dependency of ----
# 1. Range of significant Slope
# 2.1 DCI
# 2.2 Area fraction of DCI
# 2.3 p-value of max abs DCI
# 3. area fraction of positive, negative and no trend for each p-value group
# 4. Global map showing p-value at which opposing trends occur
# 5. Bivariatie color map

## Source ----
source('source/changing_prec.R')
source('source/geo_functions.R')

## Data ----
prec_index <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_g_global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

prec_annual_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_b_prec_annual_trend_bootstrap.rds"))  


## Figures -----
### Figure 1 Magnitude ----
#### Figure 1 prep ----
prec_trend_range <- prec_annual_trend[p <= 0.01 , .(trend_min = min(slope), trend_max = max(slope),p_val = "p < 0.01")]
dummy <- prec_annual_trend[p <= 0.05 , .(trend_min = min(slope), trend_max = max(slope), p_val = "p < 0.05")]

prec_trend_range <- merge(prec_trend_range, dummy, all = T)

dummy <- prec_annual_trend[p <= 0.1 , .(trend_min = min(slope), trend_max = max(slope), p_val = "p < 0.1")]
prec_trend_range <- merge(prec_trend_range, dummy, all = T)

dummy <- prec_annual_trend[p <= 0.2 , .(trend_min = min(slope), trend_max = max(slope), p_val = "p < 0.2")]

prec_trend_range <- merge(prec_trend_range, dummy, all = T)
dummy <- prec_annual_trend[, .(trend_min = min(slope), trend_max = max(slope), p_val = "all")]

prec_trend_range <- merge(prec_trend_range, dummy, all = T)

#### Figure 1 gg ----
ggplot(prec_trend_range)+
  geom_segment(aes(x = p_val, y = trend_min, yend = trend_max, group = p_val))+
  theme_bw()+
  labs(y = "significant slope [mm/year/year]", x = "p-value threshold")

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_magnitude_slope_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)

### Figure 2 DCI ----
#### Figure 2.1 
##### Figure 2.1 Prep ----

data_sel <- subset(prec_index, select = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all", "lon", "lat"))
setnames(data_sel, old = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_melt <- melt(data_sel, 
                      measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"), 
                      id.vars = c("lon", "lat"))
##### Figure 2.1 gg ----

ggplot(data_sel_melt)+
  geom_boxplot(aes(x = variable, y = value))+
  theme_bw()+
  labs(y = "DCI", x = "p-value threshold")

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_boxplot_DCI_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)
### Figure 2.2 DCI area fraction ----
#### Figure 2.2 prep ----

data_sel_melt[, DCI_brk := cut(value, c(-1.01, -0.75,-0.5,-0.07, 0.07,0.5, 0.75,1))]
grid_cell_area <- unique(data_sel_melt[, .(lon, lat)]) %>% grid_area() # m2
data_sel_melt <- grid_cell_area[data_sel_melt, on = .(lon, lat)]

data_dci_area <- data_sel_melt[, .(DCI_area = sum(area)), .(DCI_brk, variable)]
data_dci_area[, total_area := sum(DCI_area), variable]
data_dci_area[, fraction := DCI_area/total_area]

#### Figure 2.2 gg ----

ggplot(data_dci_area)+
  geom_bar(aes(x = variable, y = fraction, fill = DCI_brk), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("darkblue", "royalblue2", "lightblue", "gray90", "orange","lightcoral","darkred"))+
  labs(y = "area fraction", x = "p-value threshold")

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_bar_area_fraction_DCI_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)

### Figure 2.3 p-value of max |DCI| ----
#### Figure 2.3 prep ----
prec_index[, DCI_max := c(DCI_0_01, DCI_0_05, DCI_0_1, DCI_0_2, DCI_all)[whichDCI_max], .(lat, lon)]

prec_index[, trendDCI_max := c(trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all)[whichDCI_max], .(lat, lon)]

#### Figure 2.3 gg
ggplot(prec_index)+
  geom_boxplot(aes(x = pDCI_max, fill = trendDCI_max, y = DCI_max))+
  theme_bw()+
  scale_fill_manual(values = c ("darkblue", "gray90", "yellow","darkred"))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_boxplot_max_DCI_trend_direction_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)

### Figure 2.4 DCI fraction per trend group ----

#### Figure 2.4 prep ----

data_sel_DCI <- subset(prec_index, select = c("pDCI_max","trendDCI_max","DCI_max", "lon", "lat"))

data_sel_DCI[, DCI_brk := cut(DCI_max, c(-1.01, -0.75,-0.5,-0.07, 0.07,0.5, 0.75,1))]
grid_cell_area <- unique(data_sel_DCI[, .(lon, lat)]) %>% grid_area() # m2
data_sel_DCI <- grid_cell_area[data_sel_DCI, on = .(lon, lat)]

data_dci_area_p <- data_sel_DCI[, .(DCI_area = sum(area)), .(DCI_brk, trendDCI_max, pDCI_max)]
data_dci_area_p[, total_area := sum(DCI_area), .(trendDCI_max, pDCI_max)]
data_dci_area_p[, fraction := DCI_area/total_area]
data_dci_area_p[, sum(fraction), .(trendDCI_max, pDCI_max)]

#### Figure 2.4 gg

ggplot(data_dci_area_p, (aes(x = pDCI_max, y = fraction)))+
  geom_bar(aes(fill = DCI_brk), 
           stat = "identity",
           position = "stack")+
  theme_bw()+
  scale_fill_manual(values = c("darkblue", "royalblue2", "lightblue", "gray90", "orange","lightcoral","darkred"))+
  scale_color_manual(values = c ("darkblue", "gray90", "yellow","darkred"))+
  facet_grid(~trendDCI_max)+
  labs(y = "area fraction", x = "p-value threshold")

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_bar_area_fraction_max_DCI_by_trend_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)

### Figure 3 trend direction ----
#### Figure 3 prep ----
data_sel_trend <- subset(prec_index, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
data_sel_trend  <- grid_cell_area[data_sel_trend, on = .(lon, lat)]

setnames(data_sel_trend, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_trend_melt <- melt(data_sel_trend, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_trend_area <- data_sel_trend_melt[, .(trend_area = sum(area)), .(value, variable)] 
data_sel_trend_area[, total_area := sum(trend_area), variable]
data_sel_trend_area[, fraction := trend_area/total_area]

#### Figure 3 gg ----

ggplot(data_sel_trend_area)+
  geom_bar(aes(x = variable, y = fraction, fill = value), stat = "identity")+
  scale_fill_manual(values = c ("darkblue", "gray90", "yellow","darkred"))+
  theme_bw()+
  labs(x = "p-value threshold", y = "area fraction [-]", fill = "Significant\nTrend Direction")

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_bar_area_fraction_significant_trend_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)

### Figure 4 Map of p-value when grid becomes opposing ----

ggplot(prec_index)+
  geom_tile(aes(x = lon, y = lat, fill = p_val_opposing))+
  theme_bw()+
  labs(fill = "p-value threshold")+
  scale_fill_manual(values = c("darkblue","darkorchid3","darkorchid1","lightcoral","yellow", "green"))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_map_p_val_opposing_bootstrap.png"), 
       width = 12, height = 8)

prec_index <- grid_cell_area[prec_index, on = .(lon, lat)]
p_val_opposing <- prec_index[,.(p_area = sum(area)), (p_val_opposing)]
p_val_opposing[, total_area := sum(p_area)]
p_val_opposing[, fraction := p_area/total_area]

p_val_opposing
p_val_opposing[order(p_val_opposing)]

### Figure 5 Number of significant trends over p ----
### Product selection: Trend or no trend

#### Figure 5 prep ----
prec_index[, N_sum_0_01:= N_pos_0_01+N_neg_0_01]
prec_index[, N_sum_0_05:= N_pos_0_05+N_neg_0_05]
prec_index[, N_sum_0_1:= N_pos_0_1+N_neg_0_1]
prec_index[, N_sum_0_2:= N_pos_0_2+N_neg_0_2]
prec_index[, N_sum_all:= N_pos_all+N_neg_all]

prec_sel <- subset(prec_index, select = c("N_sum_0_01", "N_sum_0_05", 
                                          "N_sum_0_1", "N_sum_0_2", "N_sum_all",
                                          "lat", "lon", "area"))

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

ggplot(N_sig_area)+
  geom_bar(aes(x = variable, y = fraction, fill = N_sum_brk), stat = "identity")+
  theme_bw()+
  labs( fill = "N Significant\nSlopes", x = 'p-value threshold', y = "area fraction")+
  scale_fill_manual(values = c("gray90", "lightblue", "steelblue1","royalblue1", "royalblue3", "darkblue"))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES, "03_bar_area_fraction_N_significant_trend_p_val_threshold_bootstrap.png"), 
       width = 8, height = 8)
#### Figure 5 gg ---

### Figure 6 Difference of significant negative and positive trends over sum of trends over p ----
### Product selection: Trend direction

prec_index[, N_sum_0_05:= N_pos_0_05+N_neg_0_05]
prec_index[, N_dif_0_05:= abs(N_pos_0_05-N_neg_0_05)]

prec_index[trend_0_05 == "opposing", max(N_dif_0_05)]
hist(prec_index[trend_0_05 == "opposing", N_dif_0_05])

ggplot(prec_index[N_sum_0_05 > 0])+
  geom_tile(aes(x = lon, y = lat, fill = N_sum_0_05))+
  theme_bw()+
  labs(fill = "Significant trends")

prec_index[, N_sum_0_05_brk := cut(N_sum_0_05, breaks = c(1.9,4,6,14))]

hist(prec_index[trend_0_05 == "opposing", N_sum_0_05])
ggplot(prec_index[trend_0_05 == "opposing"])+
  geom_histogram(aes(x = N_dif_0_05, fill = N_sum_0_05_brk), position = "dodge")+
  theme_bw()

prec_index[trend_0_05 == "opposing", N_trend_frac := N_neg_0_05/N_pos_0_05]

### Figure X raster of top 3 ----
#### Opposers, positive signal booster, negative signal boosters, no trenders per p-value
