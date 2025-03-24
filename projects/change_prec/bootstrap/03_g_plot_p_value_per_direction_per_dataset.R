# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# plot increase of uncertainty with dataset leftout
source('source/changing_prec.R')
source('source/geo_functions.R')

## Data ----
### Input data generated in changing_prec/bootstrap/01_c 
prec_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "01_c_global_grid_per_dataset_prec_slope_bootstrap.rds"))  
prec_trend <- prec_trend[dataset_count >= 10]
### Input Data generated in projects/partition_evap/04
# evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))

## Analysis ----
prec_trend_masks <- merge(prec_trend, prec_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(prec_trend[, .(lon, lat)]) %>% grid_area() # m2
prec_trend_masks <- grid_cell_area[prec_trend_masks, on = .(lon, lat)]
prec_trend_masks[slope > 0, direction := "positive"]
prec_trend_masks[slope < 0, direction := "negative"]
prec_trend_masks[slope == 0, direction := "exactly zero"]

prec_trend_masks[,p_brk := cut(p, c(-1e-5,0.05,0.1,0.2,1))]

## plot ----

ggplot(prec_trend_masks[p < 0.2])+
  geom_violin(aes(x = land_cover_short_class, y = p, col = direction))+
  geom_abline(slope = 0, intercept = 0.05)+
  geom_abline(slope = 0, intercept = 0.1)+
  geom_abline(slope = 0, intercept = 0.01)+
  theme_bw()+
  facet_wrap(~dataset)+
  scale_color_manual(values = c("positive" = "darkred", "negative" = "royalblue"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))


ggplot(prec_trend_masks[p < 0.2 & !land_cover_short_class %in% c("Other", "Snow/Ice")], aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_SUPP, "03_g_hist_land_use_p_val_smaller_02_distribution_direction_dataset.png"), 
       width = 12, height = 12)

ggplot(prec_trend_masks, aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_SUPP, "03_g_hist_land_use_p_val_distribution_direction_dataset.png"), 
       width = 16, height = 12)


ggplot(prec_trend_masks[!land_cover_short_class %in% c("Other", "Snow/Ice")], aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_SUPP, "03_g_hist_land_use_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)


ggplot(prec_trend_masks[!land_cover_short_class %in% c("Other", "Snow/Ice")], aes(x = p_brk, fill= direction))+
  geom_bar(stat = "count", position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_SUPP, "03_g_bar_land_use_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)

ggplot(prec_trend_masks[!land_cover_short_class %in% c("Other", "Snow/Ice") & p < 0.2], aes(x = p_brk, fill= direction))+
  geom_bar(stat = "count", position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_SUPP, "03_g_bar_land_use_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)

ggplot(prec_trend_masks[!is.na(KG_class_1)], aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(KG_class_1~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_SUPP, "03_g_hist_KG_1_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)
