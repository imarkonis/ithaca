source('source/main.R')
load(paste0("../../shared/data_projects/ithaca/exeves_memory/paths.Rdata"))

EXTREMES_THRES <- 0.99
LOW_THRES <- 0.8

my_palette <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                          "#F4CC70", "#EBB582",  "#BF9A77",
                          "#E38B75", "#CE5A57",  "#CA3433", "#785A46")
                          






# Variable description

# TIMESTAMP_END: Timestamp indicating the measurement period (format: yyyymmddHHMM).
# P_F: Daily Mean Precipitation (average from the hourly scale) (mm).
# SW_IN_F: Daily Mean Incoming shortwave radiation (W/m²).
# LW_IN_F: Daily Mean Incoming longwave radiation (W/m²).
# TA_F: Daily Mean Air temperature (°C).
# WS_F: Daily Mean Wind speed (m/s).
# VPD_F: Daily Mean Vapor pressure deficit (kPa).
# NETRAD: Daily Mean Net radiation (W/m²).
# LE_CORR: Daily Mean Corrected latent heat flux (W/m²).
# H_CORR: Daily Mean Corrected sensible heat flux (W/m²).
# P_F_QC: Quality control flag for precipitation (aggregated; not meaningful, ignore).
# SW_IN_F_QC: Quality control flag for shortwave radiation (aggregated; not meaningful, ignore).
# LW_IN_F_QC: Quality control flag for longwave radiation (aggregated; not meaningful, ignore).
# TA_F_QC: Quality control flag for air temperature (aggregated; not meaningful, ignore).
# WS_F_QC: Quality control flag for wind speed (aggregated; not meaningful, ignore).
# VPD_F_QC: Quality control flag for vapor pressure deficit (aggregated; not meaningful, ignore).
# LE_F_MDS_QC: QC flag for latent heat flux (MDS gap-filled) (aggregated; not meaningful, ignore).
# H_F_MDS_QC: QC flag for sensible heat flux (MDS gap-filled) (aggregated; not meaningful, ignore).
# EF: Calculated daily mean Evaporative fraction (unitless; LE / (LE + H)).
# EF_median: Median evaporative fraction.
# date: Date in yyyy-mm-dd format.
# P_F_sum_daytime: Daytime total precipitation (mm).
# LE_QC_per: Proportion of good-quality latent heat flux data (0–1).
# H_QC_per: Proportion of good-quality sensible heat flux data (0–1).
# P_F_sum_24: 24-hour total precipitation (mm).
# LAI: Leaf area index (unitless).
# QC: Overall quality control flag for the record.
# QC_pass: Boolean flag indicating if the record passes QC (1 = pass, 0 = fail).

                          