# Correlation between datasets for different masks ----
source('source/evap_trend.R')

## libraries ----
library(ggcorrplot)

## Data ----
### Input Data generated in projects/evap_trend/01_d ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon.rds"))  
### Input Data generated in projects/partition_evap/04 ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Analysis ----
evap_trend_masks <- merge(evap_trend, evap_mask, all.x = T, by = c("lon", "lat"))

### Land cover  ----
land_cover_correlation <- evap_trend_masks[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
      ), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation"
  )), .(land_cover_short_class)]


land_cover_correlation_p <- evap_trend_masks[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)), .(land_cover_short_class)]

land_cover_correlation <- merge(land_cover_correlation, land_cover_correlation_p, by = c("land_cover_short_class", "rn", "dataset"), all = T)
land_cover_correlation <- land_cover_correlation[complete.cases(land_cover_correlation)]


### Biome types ----
biome_correlation <- evap_trend_masks[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation"
)), .(biome_class)]


biome_correlation_p <- evap_trend_masks[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)), .(biome_class)]

biome_correlation <- merge(biome_correlation, biome_correlation_p, by = c("biome_class", "rn", "dataset"), all = T)

biome_correlation[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_correlation[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_correlation[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_correlation[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_correlation[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_correlation[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_correlation[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_correlation[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_correlation[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_correlation[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_correlation[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_correlation[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_correlation[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_correlation[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_correlation[, biome_short_class := factor(biome_short_class)]


biome_correlation <- biome_correlation[complete.cases(biome_correlation)]



### Elevation class ----
elev_correlation <-  evap_trend_masks[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation"
)), .(elev_class)]

elev_correlation_p <- evap_trend_masks[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)), .(elev_class)]

elev_correlation <- merge(elev_correlation, elev_correlation_p, by = c("elev_class", "rn", "dataset"), all = T)

elev_correlation <- elev_correlation[complete.cases(elev_correlation)]


### IPCC reference regions ----
ipcc_correlation <-  evap_trend_masks[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation"
)), .(IPCC_ref_region)]

ipcc_correlation_p <-  evap_trend_masks[, (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)), .(IPCC_ref_region)]


ipcc_correlation <- merge(ipcc_correlation, ipcc_correlation_p, by = c("IPCC_ref_region", "rn", "dataset"), all = T)
ipcc_correlation <- ipcc_correlation[complete.cases(ipcc_correlation)]

### Koeppen-Geiger ----
KG_3_correlation <-  evap_trend_masks[, (melt(
  as.data.table(
    cor(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "correlation"
)), .(KG_class_3)]


KG_3_correlation_p <-  evap_trend_masks[KG_class_3 != "EF", (melt(
  as.data.table(
    cor_pmat(
      dcast(
        .SD, formula = lon + lat ~ dataset, value.var = "theil_sen_slope")[,-c(1,2)]
    ), keep.rownames = T),
  variable.name = "dataset", value.name = "p-value"
)), .(KG_class_3)]

KG_3_correlation <- merge(KG_3_correlation, KG_3_correlation_p, by = c("KG_class_3", "rn", "dataset"), all = T)

KG_3_correlation <- KG_3_correlation[complete.cases(KG_3_correlation)]

## save data ----
saveRDS(land_cover_correlation, paste0(PATH_SAVE_EVAP_TREND, "land_cover_correlation.rds"))
saveRDS(biome_correlation, paste0(PATH_SAVE_EVAP_TREND, "biome_correlation.rds"))
saveRDS(elev_correlation, paste0(PATH_SAVE_EVAP_TREND, "elev_correlation.rds"))
saveRDS(ipcc_correlation, paste0(PATH_SAVE_EVAP_TREND, "ipcc_correlation.rds"))
saveRDS(KG_3_correlation, paste0(PATH_SAVE_EVAP_TREND, "KG_3_correlation.rds"))

## read data

land_cover_correlation <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_correlation.rds"))

ggplot(land_cover_correlation[correlation < 1])+
  geom_tile(aes(x = rn , y = dataset, fill = correlation))+
  facet_wrap(~land_cover_short_class)+
  scale_fill_gradient2(midpoint = 0)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "heatplot_land_cover_correlation.png"), 
       width = 8, height = 8)


