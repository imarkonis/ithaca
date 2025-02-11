# Tables for figure 3 for Zenodo ----
source('source/evap_trend.R')

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_a_land_cover_trends_by_product.rds"))
data_problem <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_b_land_cover_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_c_land_cover_problem_aggregated.rds"))

write.table(data_trend, paste0(PATH_SAVE_EVAP_TREND_TABLES, "land_cover_trends_by_product.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_problem, paste0(PATH_SAVE_EVAP_TREND_TABLES, "land_cover_problem_area_fraction.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_trend_env , paste0(PATH_SAVE_EVAP_TREND_TABLES, "land_cover_problem_aggregated.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_biome_trends_by_product.rds"))
data_problem <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_biome_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_biome_problem_aggregated.rds"))

write.table(data_trend, paste0(PATH_SAVE_EVAP_TREND_TABLES, "biome_trends_by_product.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_problem, paste0(PATH_SAVE_EVAP_TREND_TABLES, "biome_problem_area_fraction.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_trend_env , paste0(PATH_SAVE_EVAP_TREND_TABLES, "biome_problem_aggregated.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_IPCC_ref_regions_trends_by_product.rds"))
data_problem <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_IPCC_ref_regions_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_IPCC_ref_regions_problem_aggregated.rds"))

write.table(data_trend, paste0(PATH_SAVE_EVAP_TREND_TABLES, "IPCC_ref_regions_trends_by_product.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_problem, paste0(PATH_SAVE_EVAP_TREND_TABLES, "IPCC_ref_regions_problem_area_fraction.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_trend_env , paste0(PATH_SAVE_EVAP_TREND_TABLES, "IPCC_ref_regions_problem_aggregated.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_elevation_trends_by_product.rds"))
data_problem <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_elevation_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_elevation_problem_aggregated.rds"))

write.table(data_trend, paste0(PATH_SAVE_EVAP_TREND_TABLES, "elevation_trends_by_product.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_problem, paste0(PATH_SAVE_EVAP_TREND_TABLES, "elevation_problem_area_fraction.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_trend_env , paste0(PATH_SAVE_EVAP_TREND_TABLES, "elevation_problem_aggregated.csv"), 
            sep = ",", dec = ".", row.names = FALSE)

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_a_Koeppen_Geiger_trends_by_product.rds"))
data_problem <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_3_SI_b_Koeppen_Geiger_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_SI_3_c_Koeppen_Geiger_problem_aggregated.rds"))

write.table(data_trend, paste0(PATH_SAVE_EVAP_TREND_TABLES, "Koeppen_Geiger_trends_by_product.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_problem, paste0(PATH_SAVE_EVAP_TREND_TABLES, "Koeppen_Geiger_problem_area_fraction.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
write.table(data_trend_env , paste0(PATH_SAVE_EVAP_TREND_TABLES, "Koeppen_Geiger_problem_aggregated.csv"), 
            sep = ",", dec = ".", row.names = FALSE)
