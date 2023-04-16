# Estimation and plotting of slopes 

source('source/europe.R')
source('source/graphics.R')

## Load data
prec_summer <- readRDS(paste0(PATH_SAVE_EUROPE, "prec_summer.rds"))
prec_warm <- readRDS(paste0(PATH_SAVE_EUROPE, "prec_warm_season.rds"))

## Analysis
prec_summer_slopes <- prec_summer[, {
  linear_model <- lm(prec_sum ~ year); coef(linear_model)[2]
}, by = .(lon, lat, dataset)]
setnames(prec_summer_slopes, "V1", "slope")

prec_warm_slopes <- prec_warm[, {
  linear_model <- lm(prec_sum ~ year); coef(linear_model)[2]
}, by = .(lon, lat, dataset)]
setnames(prec_warm_slopes, "V1", "slope")

## Figures
to_plot_sf <- prec_summer_slopes[dataset == 'mswep', .(lon, lat, slope)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_mswep_summer_slopes <- ggplot(to_plot_sf) +
  geom_sf(aes(color = slope, fill = slope)) +
  scale_color_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue') +
  scale_fill_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue') +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_x_continuous(breaks = seq(-20, 30, 10)) +
  scale_y_continuous(breaks = seq(30, 70, 10)) +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_EUROPE_FIGURES,
              "mswep_summer_slopes.png"), width = 12, height = 8)

to_plot_sf <- prec_warm_slopes[dataset == 'mswep', .(lon, lat, slope)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_mswep_warm_slopes <- ggplot(to_plot_sf) +
  geom_sf(aes(color = slope, fill = slope)) +
  scale_color_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue') +
  scale_fill_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue') +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_x_continuous(breaks = seq(-20, 30, 10)) +
  scale_y_continuous(breaks = seq(30, 70, 10)) +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_EUROPE_FIGURES,
              "mswep_warm_season_slopes.png"), width = 12, height = 8)

to_plot_sf <- prec_summer_slopes[dataset == 'gpcp', .(lon, lat, slope)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_gpcp_summer_slopes <- ggplot(to_plot_sf) +
  geom_sf(aes(color = slope, fill = slope)) +
  scale_color_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue') +
  scale_fill_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue') +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_x_continuous(breaks = seq(-20, 30, 10)) +
  scale_y_continuous(breaks = seq(30, 70, 10)) +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))







