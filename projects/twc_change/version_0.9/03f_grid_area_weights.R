# Libraries ====================================================================

source('source/twc_change.R')
source('source/geo_functions.R')

# Inputs =======================================================================

weights_dt <- readRDS(file.path(PATH_OUTPUT_DATA, 
                                'dataset_agreement_weights.Rds'))

# Analysis =====================================================================

grid_cell_weights <- unique(weights_dt[, .(lon, lat)]) %>% grid_area() # m2
grid_cell_weights[, area_weight := area / sum(area)]

# Outputs=======================================================================

saveRDS(grid_cell_weights, file.path(PATH_OUTPUT_DATA, 'grid_area_weights.Rds'))
