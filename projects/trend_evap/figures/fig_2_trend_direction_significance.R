# Figure 2 - results depend product selection ----
## Opposing map
## N insig vs N sig map
source('source/evap_trend.R')

library(rnaturalearth)

# Data
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_a_c_d_grid_trend_stats.rds"))

# Map preparation -----
## World and Land borders ----
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
load(paste0(PATH_SAVE_PARTITION_EVAP, "paths.Rdata"))

earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels ----
labs_y <- data.frame(lon = -160, lat = c(50, 25, -5, -35, -65))
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


## Maps of p-value when grid becomes opposing ----
to_plot_sf <- evap_index[, .(lon, lat, p_val_opposing)
][, value := as.numeric(p_val_opposing)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

cols_opposing <- c("darkblue","darkorchid3","darkorchid1","lightcoral","gold", "green")

fig_map_opposing <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_opposing, labels = levels(evap_index$p_val_opposing)) +
  scale_color_manual(values = cols_opposing,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Significant \ntrends\nare opposing") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18))

## When more trends become significant ----

to_plot_sf <- evap_index[, .(lon, lat, more_sig_trends)
][, value := as.numeric(more_sig_trends)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

cols_sig_trends <- c("darkblue","darkorchid3","darkorchid1","lightcoral", "gold")

fig_map_more_sig_trends <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_sig_trends, labels = levels(evap_index$more_sig_trends)) +
  scale_color_manual(values = cols_sig_trends,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Most trends\nare \nsignificant") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18))



## Maps of DCI when all trends are considered ----

to_plot_sf <- evap_index[, .(lon, lat, DCI_all_brk)
][, value := as.numeric(DCI_all_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

cols_DCI <- c("darkblue", "lightblue", "gray90", "orange","darkred")

fig_map_DCI <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_DCI, labels = levels(evap_index$DCI_all_brk)) +
  scale_color_manual(values = cols_DCI,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "DCI\nall trends") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18))


## Barplot Figures -----
### Figure  DCI area fraction ----
data_dci_area <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_f_area_stats_DCI_all_trends.rds"))

fig_DCI <- ggplot(data_dci_area)+
  geom_bar(aes(x = variable, y = fraction, fill = DCI_brk), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("darkblue", "lightblue", "gray90", "orange","darkred"))+
  labs(y = "Area Fraction [-]", x = "P-value threshold", fill = "DCI")+
  theme(legend.position="right")+
  guides(fill = guide_legend(nrow = 5 ,byrow = TRUE))+
  theme(plot.margin = unit(c(1,0.3,0,1.1), 'cm'))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title.y = element_text(size = 18, vjust = 3), 
        axis.title.x = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18))


### Figure 2 trend direction ----
data_sel_trend_area <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_d_area_stats_trend_direction.rds"))

fig_trend <- ggplot(data_sel_trend_area)+
  geom_bar(aes(x = variable, y = fraction, fill = value), stat = "identity")+
  scale_fill_manual(values = c ("darkblue", "gray90", "yellow","darkred"))+
  theme_bw()+
  labs(x = "", y = "Area fraction [-]", fill = "Direction of\nsignificant\ntrends")+
  theme(legend.position="right")+
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))+
  theme(plot.margin = unit(c(0.5,0.3,0,1.1), 'cm'))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title.y = element_text(size = 18, vjust = 3), 
        axis.title.x = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18))

### Figure 3 Number of significant trends over p ----
### Product selection: Trend or no trend
N_sig_area <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_b_area_stats_significant_trend_count.rds"))
fig_Ntrend <- ggplot(N_sig_area)+
  geom_bar(aes(x = variable, y = fraction, fill = N_sum_brk), stat = "identity")+
  theme_bw()+
  labs( fill = "Number of\nsignificant\ntrends", x = '', y = "Area fraction [-]")+
  scale_fill_manual(values = c("gray90", "lightblue", "steelblue1", "royalblue3", "darkblue"))+
  theme(legend.position="right")+
  guides(fill = guide_legend(nrow = 5, byrow = TRUE))+
  theme(plot.margin = unit(c(0.5,0.3,0,1.1), 'cm'))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title.y = element_text(size = 18, vjust = 3), 
        axis.title.x = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18))+
    theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1, size = 18),
          axis.text.y = element_text(size = 18))

## Composite figure ----

maps <- ggarrange(fig_map_more_sig_trends,
                  fig_map_opposing, fig_map_DCI, align = "hv", labels = c("a", "c", "e"), nrow = 3, ncol = 1,
                  font.label = list(size = 20))

area_plots <- ggarrange(fig_Ntrend, fig_trend, fig_DCI, align = "hv",
          nrow = 3, ncol = 1,
          labels = c("b", "d", "f"), font.label = list(size = 20))

fig_2 <- ggarrange(maps, area_plots, ncol = 2, widths = c(1, 0.75))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig2_maps_barplots.png"), 
       width = 12, height = 11)
