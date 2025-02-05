
source('source/partition_evap.R')
source('source/geo_functions.R')
evap_datasets_dt <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_datasets_dt_mean <- evap_datasets_dt[,.(evap_mean = mean(evap)), .(lat, lon, dataset)]

evap_datasets_dt_mean[, dt_count := .N, .(lon,lat)]

evap_dt_counts <- evap_datasets_dt_mean[,.(lon, lat, dt_count)]
evap_dt_counts <- unique(evap_dt_counts)

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets_complete.rds"))
evap_nc <- evap_datasets[,.(lon, lat, n_datasets)]
evap_nc <- unique(evap_nc)

evap_all <- merge(evap_nc, evap_dt_counts, by = c("lon", "lat"))

evap_all_diff <- evap_all[n_datasets != dt_count]
evap_all_diff[, diff := n_datasets - dt_count]

evap_diff <- evap_datasets[evap_all_diff, on = .(lon, lat)]

evap_diff_dt <- evap_datasets_dt_mean[evap_all_diff, on = .(lon, lat)]
evap_diff_dt_time <- evap_datasets_dt[evap_all_diff, on = .(lon, lat)]
bess_diff <- evap_diff_dt_time[dataset =="bess"]
bess_diff[, year_count := .N, .(lon, lat)]

et_diff <- evap_diff_dt_time[dataset =="etsynthesis"]
et_diff[, year_count := .N, .(lon, lat)]

plot(bess_diff$year_count)
evap_datasets_dt_nc<- evap_datasets[evap_diff_dt, on = .(lon, lat, dataset)]
evap_datasets_dt_nc_na <- evap_datasets_dt_nc[is.na(evap_mean)]
evap_datasets_dt_nc_na <- unique(evap_datasets_dt_nc_na)
evap_datasets_dt_nc_na[, .N, dataset]

ggplot(evap_datasets_dt_nc_na)+
  geom_tile(aes(x = lon, y = lat))+
  theme_bw()

evap_zero <- evap_diff[evap_mean == 0]

evap_datasets_dt_time <- evap_datasets_dt[evap_zero, on = .(lon, lat, dataset)]


evap_datasets_dt_time_na <- evap_datasets_dt_time[is.na(evap)]
evap_datasets_dt_time_na[, .N, dataset]

evap_datasets_dt_time <- evap_datasets_dt[evap_diff, on = .(lon, lat, dataset)]
evap_datasets_dt_time_na <- evap_datasets_dt_time[is.na(evap)]
evap_datasets_dt_time_na <- unique(evap_datasets_dt_time_na)
evap_datasets_dt_time_na[, .N, dataset]


ggplot(evap_all)+
  geom_point(aes(x = n_datasets, y = dt_count))+
  geom_abline(slope = 1, intercept = 0)+
  labs(y = "counts from code 2", x = "counts from code 1")+
  theme_bw()
