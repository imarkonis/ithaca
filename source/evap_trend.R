source("source/main.R")
source("source/partition_evap.R")

# Output ----
PATH_SAVE_EVAP_TREND <- paste0(PATH_SAVE, "evap_trend/")
PATH_SAVE_EVAP_TREND_RAW <- paste0(PATH_SAVE, "evap_trend/raw/")
PATH_SAVE_EVAP_TREND_SPATIAL <- paste0(PATH_SAVE, "evap_trend/spatial/")
PATH_SAVE_EVAP_TREND_FIGURES <- paste0(PATH_SAVE, "evap_trend/figures/")
PATH_SAVE_EVAP_TREND_TABLES <- paste0(PATH_SAVE, "evap_trend/tables/")
PATH_SAVE_EVAP_TREND_FIGURES_SUPP <- paste0(PATH_SAVE, "evap_trend/figures/SUPPLEMENT")
PATH_SAVE_EVAP_TREND_FIGURES_EXPLORE <- paste0(PATH_SAVE, "evap_trend/figures/Exploration")

# Colors----
## Color for datasets
cols_data <- c("bess" = "chartreuse2",
               "camele" = "red",
               "era5-land" = "gold1",
               "etmonitor" = "chartreuse4",
               "etsynthesis" = "hotpink",
               "fldas" = "darkslategray1",
               "gldas-clsm" = "deepskyblue1",
               "gldas-noah" = "deepskyblue3",
               "gldas-vic" = "deepskyblue4",
               "gleam" = "darkgreen",
               "jra55" = "orange1",
               "merra2" = "orange3",
               "mod16a" = "green",
               "terraclimate" = "darkblue"
)

# IPCC
IPCC_Africa <- c("ARP", "CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SCA","SES", "SSA","SWS")

# Functions ----
## Calculate theil sen slope for each grid in parallel 
evap_trends <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }

  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("Kendall", "RobustLinearReg")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, .(  kendall_tau = Kendall(evap, year)$tau,
                                   kendall_p_value = Kendall(evap, year)$sl,
                                   theil_sen_slope = theil_sen_regression(evap~year)$coefficients[2], 
                                   theil_sen_p_value = summary(theil_sen_regression(evap~year))$coefficients[8]
    ), 
    .(lon, lat, dataset)]
  }
  return(dummie)
}


evap_trends_lon_lat <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }
  
  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("Kendall", "RobustLinearReg")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, .(  kendall_tau = Kendall(evap, year)$tau,
                                   kendall_p_value = Kendall(evap, year)$sl,
                                   theil_sen_slope = theil_sen_regression(evap~year)$coefficients[2], 
                                   theil_sen_p_value = summary(theil_sen_regression(evap~year))$coefficients[8]
    ), 
    .(lon, lat)]
  }
  return(dummie)
}


