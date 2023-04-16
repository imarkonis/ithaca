# Data post-processing: Subsetting data for summer months and estimation of precipitation

source('source/europe.R')
source('source/geo_functions.R')

## Load data
prec_mswep <- brick(paste0(PATH_SAVE_EUROPE_RAW, "/mswep_tp_mm_europe_198001_201912_025_monthly.nc"))
prec_gpcp <- brick(paste0(PATH_SAVE_EUROPE_RAW, "/gpcp_tp_mm_europe_198001_201912_025_monthly.nc"))

## Constants
SUMMER_MONTHS <- c(6, 7, 8)
WARM_MONTHS <- c(4, 5, 6, 7, 8, 9)

## Variables
prec_mswep_dt <- brick_to_dt(prec_mswep)
prec_gpcp_dt <- brick_to_dt(prec_gpcp)
prec_dt <- rbind(cbind(prec_mswep_dt, dataset = factor('mswep')), 
                     cbind(prec_gpcp_dt, dataset = factor('gpcp')))
colnames(prec_dt)[1:2] <- c("lon", "lat")

## Analysis
prec_summer <- prec_dt[month(time) %in% SUMMER_MONTHS]
prec_summer[, year := year(time)]
prec_summer <- prec_summer[, .(prec_sum = sum(value)), .(lon, lat, year, dataset)]

prec_warm_season <- prec_dt[month(time) %in% WARM_MONTHS]
prec_warm_season[, year := year(time)]
prec_warm_season <- prec_warm_season[, .(prec_sum = sum(value)), .(lon, lat, year, dataset)]

## Save data
saveRDS(prec_summer, paste0(PATH_SAVE_EUROPE, "prec_summer.rds"))
saveRDS(prec_warm_season, paste0(PATH_SAVE_EUROPE, "prec_warm_season.rds"))

