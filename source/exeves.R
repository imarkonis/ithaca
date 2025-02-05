source('source/main.R')
load(paste0("../../shared/data_projects/ithaca/exeves/paths.Rdata"))

CZECHIA_LON_MIN <- 12
CZECHIA_LON_MAX <- 19 
CZECHIA_LAT_MIN <- 48.5 
CZECHIA_LAT_MAX <- 51.3

CEU_LON_MIN <- 12
CEU_LON_MAX <- 19 
CEU_LAT_MIN <- 48.5 
CEU_LAT_MAX <- 51.3

START_PERIOD_1 <- as.Date("1981-1-1") 
END_PERIOD_1 <- as.Date("2001-12-31")
END_PERIOD_2 <- as.Date("2022-12-31")
PERIOD_LENGTH <- round(as.numeric((END_PERIOD_2 - START_PERIOD_1)/ 365.25), 0)

EXTREMES_THRES <- 0.95
LOW_THRES <- 0.8
SUB_PERIOD_YEARS <- 0.5 * PERIOD_LENGTH
GRID_CELL_N <- 161

agu_palette <- c('#00324A', '#005294', '#058ECD', '#FFFFFF') 
colset_subdued_prof = c("#90AFC5", "#336B87", "#2A3132", "#763626")

