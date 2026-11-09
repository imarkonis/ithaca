#Scatter plot matrix
source("source/change_prec.R")

library(trend)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs.rds"))
prec_data <- prec_data[, .(lon, lat, year, dataset, prec)]

DATASETS <- c("cpc-global", "era5-land", "gpcp-v1-3", "gpm-imerg-v7", "jra-3q",
              "merra-2", "mswep-v2-8", "ncep-doe")

prec_trends <- foreach(dataset_count = 1:length(DATASETS)) %do%{
  dummie_data <- DATASETS[dataset_count]
  dummie_yrs <- prec_data[dataset != dummie_data]
  dummie_yrs <- dummie_yrs[, .(prec = median(prec, na.rm = TRUE)),
                           .(lon, lat, year)]
  dummie_yrs <- dummie_yrs[, sens.slope(prec)[c(1,3)], .(lon, lat)]
  dummie_yrs[estimates > 0, yrs_slope := 1
             ][estimates == 0, yrs_slope := 0][estimates < 0, yrs_slope := -1]
  dummie_yrs <- dummie_yrs[, .(lon, lat, yrs_slope)]
  dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_", dummie_data,
                           "_r1x.rds"))
  dummie <- dummie[, sens.slope(prec)[c(1,3)], .(lon, lat)]
  dummie[estimates > 0, r1x_slope := 1
         ][estimates == 0, r1x_slope := 0][estimates < 0, r1x_slope := -1]
  dummie <- dummie[, .(lon, lat, r1x_slope)]
  dummie <- merge(dummie_yrs, dummie, by = c("lon", "lat"))
  dummie$dataset <- dummie_data
  return(dummie)
}

prec_trends <- rbindlist(prec_trends)

dummie_yrs <- prec_data[dataset != "mswep-v2-8" & dataset != "ncep-doe"]
dummie_yrs <- dummie_yrs[, .(prec = median(prec, na.rm = TRUE)),
                         .(lon, lat, year)]
dummie_yrs <- dummie_yrs[, sens.slope(prec)[c(1,3)], .(lon, lat)]
dummie_yrs[estimates > 0, yrs_slope := 1
           ][estimates == 0, yrs_slope := 0][estimates < 0, yrs_slope := -1]
dummie_yrs <- dummie_yrs[, .(lon, lat, yrs_slope)]
dummie <- readRDS(paste0(PATH_SAVE_CHANGE_PREC,
                         "prec_data_mswep-v2-8_ncep-doe_r1x.rds"))
dummie <- dummie[, sens.slope(prec)[c(1,3)], .(lon, lat)]
dummie[estimates > 0, r1x_slope := 1
       ][estimates == 0, r1x_slope := 0][estimates < 0, r1x_slope := -1]
dummie <- dummie[, .(lon, lat, r1x_slope)]
dummie <- merge(dummie_yrs, dummie, by = c("lon", "lat"))
dummie$dataset <- "mswep-v2-8_ncep-doe"

prec_trends <- rbind(prec_trends, dummie)

rm(prec_data, dummie, dummie_yrs)

ref_yrs <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_yrs_slopes.rds"))
ref_yrs <- ref_data[dataset == "ensemble"]
ref_yrs[slope > 0, ref := 1 ][slope == 0, ref := 0][slope < 0, ref := -1]
ref_yrs <- merge(prec_trends, ref_yrs, by = c("lon", "lat"))
ref_yrs[, control := ifelse(yrs_slope == ref, 1, 0)]
ref_yrs <- ref_yrs[, .(pct = 1 - sum(control)/9), .(lon, lat)]
ref_yrs[pct < .1, prec := 1][pct >= .1 & pct < .25, prec := 2
                             ][pct >= .25 & pct < .50, prec := 3
                               ][pct >= .50 & pct < .75, prec := 4
                                 ][pct >= .75, prec := 5]

to_plot_yrs <- ref_yrs[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_yrs$prec <- as.character(to_plot_yrs$prec)

ref_r1x <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_r1x_slopes.rds"))
ref_r1x <- ref_r1x[dataset == "ensemble"]
ref_r1x[slope > 0, ref := 1 ][slope == 0, ref := 0][slope < 0, ref := -1]
ref_r1x <- merge(prec_trends, ref_r1x, by = c("lon", "lat"))
ref_r1x[, control := ifelse(r1x_slope == ref, 1, 0)]
ref_r1x <- ref_r1x[, .(pct = 1 - sum(control)/9), .(lon, lat)]
ref_r1x[pct < .1, prec := 1][pct >= .1 & pct < .25, prec := 2
                             ][pct >= .25 & pct < .50, prec := 3
                               ][pct >= .50 & pct < .75, prec := 4
                                 ][pct >= .75, prec := 5]

to_plot_r1x <- ref_r1x[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_r1x$prec <- as.character(to_plot_r1x$prec)

save(to_plot_r1x, to_plot_yrs, file = paste0(PATH_SAVE_CHANGE_PREC,
                                             "prec_slopes_sensitivity.rda"))

ggplot(to_plot_r1x) +
#geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
geom_sf(aes(color = prec, fill = prec)) +
#geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
scale_fill_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                             "3" = "#F4CC70", "4" = "#E38B75",
                             "5" = "#CE5A57"),
                  labels = c("1" = "Very High Confidence", "2" = "High Confidence",
                             "3" = "Confidence",
                             "4" = "Low Confidence", "5" = "No Confidence")) +
  scale_color_manual(values = c("1" = "#4D648D", "2" = "#97B8C2",
                                    "3" = "#F4CC70", "4" = "#E38B75",
                                "5" = "#CE5A57"), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Domain\nRepresentative\nConfidence",
       title = NULL) +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))
