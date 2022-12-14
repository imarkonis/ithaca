### Assessing % change in ensemble mean between two 30 years time periods 
### for each Koppen Geiger class

source('source/main.R')
source('source/graphics.R')
source('source/blueprint.R')
source('source/masks.R')

## Read data 
prec_era5_kenya <- readRDS(paste0(path_save_blueprint, "prec_era5.rds"))
prec_mean_change <- readRDS(paste0(path_save_blueprint, "prec_mean_change.rds"))
all_stats_low_bias <- readRDS(paste0(path_save_blueprint, "all_stats_low_bias.rds"))

fname_shape <- list.files(path = masks_dir_KG_beck, full.names = T, pattern = "climate_beck_level1.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

## Set variables
prec_low_bias <- all_stats_low_bias[variable == 'prec']

## Crop mask
shape_mask_crop <- st_crop(shape_mask, study_area)
shape_mask_raster <- rasterize(shape_mask_crop, prec_era5_kenya[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class')

## Quick validation
ggplot() +
  geom_raster(data = shape_mask_df, aes(lon, lat, fill = KG_class)) +
  geom_point(data = prec_low_bias,  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

## Main estimations
# All grid cells
prec_mean_change <- merge(prec_mean_change, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_mean_change <- prec_mean_change[complete.cases(prec_mean_change)]

prec_mean_change[, mean_diff_KG := round(mean(mean_diff), 1), KG_class]
prec_mean_change[, mean_perc_change_KG := round(mean(mean_perc_change), 2), KG_class]

prec_mean_change_KG <- prec_mean_change[, unique(mean_perc_change_KG), KG_class]

# Low-bias grid cells
prec_mean_change_low_bias <- prec_mean_change[prec_low_bias, on = c('lon', 'lat')]
prec_mean_change_low_bias <- prec_mean_change_low_bias[complete.cases(prec_mean_change_low_bias)]

prec_mean_change_low_bias[, mean_diff_KG := round(mean(mean_diff), 1), KG_class]
prec_mean_change_low_bias[, mean_perc_change_KG := round(mean(mean_perc_change), 2), KG_class]

prec_mean_change_KG_low_bias <- prec_mean_change_low_bias[, unique(mean_perc_change_KG), KG_class]

## Plot results
to_plot <- rbind(cbind(prec_mean_change_KG, grid_cells = "all"),
                 cbind(prec_mean_change_KG_low_bias, grid_cells = "low bias"))
colnames(to_plot)[2] <- 'value'
to_plot <- to_plot[order(to_plot$KG_class)]
to_plot[, KG_class := factor(KG_class)]
to_plot[, grid_cells := factor(grid_cells)]

ggplot(to_plot) +
  geom_bar(aes(x = KG_class, y = value, fill = grid_cells, group = grid_cells), 
           stat = 'identity', position = position_dodge()) +
  scale_fill_manual(values=c(main_cols[1], main_cols[3])) +
  labs(x = "Climate types (KG classes)", y = "Change [%]", fill = "Bias") +
  theme_light()


