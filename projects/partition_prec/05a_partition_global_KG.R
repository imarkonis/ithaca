# Partitions global precipitation to different KG classes and quantify their uncertainty

source('source/partition_prec.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))
prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))

## Variables
climate_KG <- merge(prec_mask[, .(lat, lon, KG_class_1_name)], 
                    prec_grid[, .(lon, lat, area)], by = c("lon", "lat"), all = TRUE)
datasets_KG <- merge(climate_KG, prec_dataset_means, by = c("lon", "lat"))
datasets_KG[, prec_volume_year := area * M2_TO_KM2 * prec_mean * MM_TO_KM
            ][, prec_mean := NULL] # km3
datasets_KG <- datasets_KG[complete.cases(datasets_KG)]
global_mean <- data.table(Source = "All", Global = prec_annual[, mean(prec_volume, na.rm = T)])
dataset_means <- prec_annual[, .(Global = mean(prec_volume, na.rm = T)), dataset]
dataset_means <- merge(unique(prec_dataset_means[, .(dataset, dataset_type)]), dataset_means, by = 'dataset')
colnames(dataset_means)[1] <- 'Dataset'
dataset_types_means <- dataset_means[, .(Global = mean(Global)), dataset_type]
colnames(dataset_types_means)[1] <- 'Source'
dataset_types_means <- rbind(global_mean, dataset_types_means)

## Analysis
datasets_KG[, .(area = sum(area)), .(dataset, dataset_type)] #Antarctica 13.66 million km2
climate_KG[, sum(area, na.rm = TRUE) / 
             climate_KG[, sum(area, na.rm = TRUE)], KG_class_1_name] 
climate_KG[, sum(area, na.rm = TRUE), KG_class_1_name]
dataset_partition_KG <- datasets_KG[, .(prec_sum = round(sum(prec_volume_year), 0)), 
                                    .(KG_class_1_name, dataset, dataset_type)]
dataset_partition_KG[(dataset == 'cmorph' |                       #Remove as they do not cover the whole planet
                        dataset == 'persiann' | 
                        dataset == 'chirps') & 
                       (KG_class_1_name == 'Polar' | KG_class_1_name == 'Continental'), 
                     prec_sum := NA]

### Mean
partition_KG_global <- dcast(dataset_partition_KG, . ~ KG_class_1_name, 
                             fun = mean, na.rm = TRUE)
colnames(partition_KG_global)[1] <- "Source"
partition_KG_dataset_types <- dcast(dataset_partition_KG, dataset_type ~ KG_class_1_name, 
                                    fun = mean, na.rm = TRUE)
colnames(partition_KG_dataset_types)[1] <- "Source"
partition_KG <- rbind(partition_KG_global, partition_KG_dataset_types)
partition_KG$Source[1] <- 'All'
partition_KG <- merge(partition_KG, dataset_types_means, by = "Source") # KG classification has NAs and hence underestimates the total sum
partition_KG[, Source := c("All", "Stations", "Reanalyses", "Remote Sensing")]
partition_KG[, 2:7 := lapply(.SD, round, 0), .SDcols = 2:7]

partition_KG_datasets <- dcast(dataset_partition_KG, dataset ~ KG_class_1_name, fun = mean, na.rm = TRUE)
partition_KG_datasets <- merge(prec_dataset_means[, .(dataset = unique(dataset)), dataset_type], partition_KG_datasets, by = 'dataset')
colnames(partition_KG_datasets)[1] <- c("Dataset")
partition_KG_datasets <- merge(partition_KG_datasets, dataset_means[, .(Dataset, Global)], by = "Dataset")
#partition_KG_datasets[, diff_mean := round(Global - mean(Global, na.rm = TRUE), 0)]

partition_KG_datasets[, mean(Global)]
partition_KG_datasets[Dataset %in% datasets_no_outliers, mean(Global)]
partition_KG_datasets[Dataset %in% datasets_no_overlap, mean(Global)]
partition_KG_datasets[Dataset %in% datasets_no_overlap & Dataset %in% datasets_no_outliers, mean(Global)]

### St. Dev
partition_KG_global_sd <- dcast(dataset_partition_KG, . ~ KG_class_1_name, 
                                fun = sd, na.rm = TRUE)
colnames(partition_KG_global_sd)[1] <- "Source"
partition_KG_dataset_types_sd <- dcast(dataset_partition_KG, dataset_type ~ KG_class_1_name, 
                                       fun = sd, na.rm = TRUE)
colnames(partition_KG_dataset_types_sd)[1] <- "Source"
partition_KG_sd <- rbind(partition_KG_global_sd, partition_KG_dataset_types_sd)
partition_KG_sd[, Source := c("All", "Stations", "Reanalyss", "Remote Sensing")]
partition_KG_sd$Global <- partition_KG_datasets[, sd(Global, na.rm = TRUE)]
partition_KG_sd$Global[2:4] <- partition_KG_datasets[, sd(Global, na.rm = TRUE), dataset_type]$V1[c(2, 3, 1)]
partition_KG_sd[, 2:7 := lapply(.SD, round, 0), .SDcols = 2:7]

## Save data
write.csv(partition_KG, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_KG_global.csv"))
write.csv(partition_KG_sd, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_KG_sd_global.csv"))
write.csv(partition_KG_datasets[, -2], paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_KG_datasets_global.csv"))
saveRDS(datasets_KG[, .(lon, lat, dataset, prec_volume_year)] , paste0(PATH_SAVE_PARTITION_PREC, "prec_dataset_volume.rds"))



## Antarctica
13.82 * 10^6 * c(150, 200) * MM_TO_KM
13.82 * 10^6 * 175 * MM_TO_KM
