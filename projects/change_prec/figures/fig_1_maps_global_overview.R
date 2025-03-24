# Figure 1 - the global overview ----
source('source/changing_prec.R')

library(rnaturalearth)

# Map preparation -----
## World and Land borders ----
PATH_SAVE_PARTITION_PREC <- paste0(PATH_SAVE, "partition_prec/")
load(paste0(PATH_SAVE_PARTITION_PREC, "paths.Rdata"))

earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels ----
labs_y <- data.frame(lon = -170, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°", ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

## Global prec trend bt data product  ----
prec_annual_trend <- readRDS(paste0(PATH_SAVE_CHANGING_PREC_TABLES, "data_fig_1_a_global_prec_trend.rds"))
prec_annual_trend[, trend_significance := factor(trend_significance, levels = c("positive p < 0.01", "positive p < 0.05",
                                                                                "positive p <= 0.1", "negative p <= 0.1",
                                                                                "p > 0.1"))]
fig_trend <- ggplot(prec_annual_trend)+
  geom_segment(aes(x = dataset, y = lower, yend = upper), lwd = 1)+
  geom_point(aes(x = dataset, y = slope, col = as.factor(trend_significance)), size = 4)+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_color_manual(values = c("p > 0.1" = "gray", 
                               "negative p <= 0.1" = "royalblue3", 
                               "positive p < 0.01" = "darkred",
                               "positive p < 0.05" = "firebrick3",
                               "positive p <= 0.1" = "lightcoral"))+
  labs(y = expression(paste("Precip trend [mm year"^-~2,"] ")), color = "Trend significance ", x = "Dataset")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 15), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "right")



## folds of Q25 Q75 ----

prec_trend_stats <- readRDS(paste0(PATH_SAVE_CHANGING_PREC_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))


### Map ----

cols_grid_fold <- c("royalblue3","gold")
                                
to_plot_sf <- prec_trend_stats[, .(lon, lat, fold_brk)
][, value := as.numeric(fold_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_Q75Q25_fold <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_grid_fold, labels = levels(prec_trend_stats$fold_brk)) +
  scale_color_manual(values = cols_grid_fold,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile   \nfold [-]") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


## Quartile sign disagreement

cols_grid_agreement <- c("royalblue3","gold")

to_plot_sf <- prec_trend_stats[, .(lon, lat, sign)
][, value := as.numeric(sign)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_sign_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_grid_agreement, labels = levels(prec_trend_stats$sign)) +
  scale_color_manual(values = cols_grid_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Q25 and Q75   \nsigns") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


## ggarrange ----

ggmaps <- ggarrange(fig_Q75Q25_fold, fig_sign_agreement, align = "hv", 
                    ncol = 2, nrow = 1, labels = c("b", "c"))
fig_1 <- ggarrange(fig_trend, ggmaps,  
          ncol = 1, nrow = 2, labels = c("a", ""), heights = c(1, 1.0))

ggsave(paste0(PATH_SAVE_CHANGING_PREC_FIGURES_MAIN, "fig1_maps_prec_trend_overview.png"), 
       width = 12, height = 8)


