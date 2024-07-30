source("source/main.R")

#Input ---- ####

PATH_IPCC_data <- paste0(PATH_DATA,"geodata/ipcc_v4/")

# Output ---- ####
PATH_SAVE_CHANGING_PREC <- paste0(PATH_SAVE, "changing_prec/")
load(paste0(PATH_SAVE_CHANGING_PREC, "changing_prec_paths.Rdata"))

# Colors---- ####
## Color for datasets ####
cols_data <- c("cmap" = "chartreuse2",
               "cru" = "red",
               "era5-land" = "gold1",
               "fldas" = "darkslategray1",
               "gpcc" = "deepskyblue1",
               "gpcp" = "deepskyblue3",
               "gpm-imerg" = "deepskyblue4",
               "gsmap" = "darkgreen",
               "jra55" = "orange1",
               "merra2" = "orange3",
               "mswep" = "green",
               "precl" = "darkblue"
)

# IPCC ####
IPCC_Africa <- c("CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("ARP", "EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

# Functions ---- ####
## Calculate theil sen slope for each grid in parallel 

prec_trends_lon_lat_boot <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }
  
  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("openair")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, TheilSen(.SD, pollutant = "prec", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)]
    , 
    .(lon, lat)]
  }
  return(dummie)
}


prec_trends_boot <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }

  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("openair")) %dopar% {
    dummie_row <- x[[idx]]
    # Check if there is data for more than one year
    # if (length(unique(dummie_row$year)) <= 1) {
    #   print(paste("Skipping index:", idx, "due to insufficient data (only one year)"))
    #   return(NULL)
    # }
    dummie_row <- dummie_row[, TheilSen(.SD, pollutant = "prec", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)]
    ,
    .(lon, lat, dataset)]
  }
  return(dummie)
}

##############################################################
