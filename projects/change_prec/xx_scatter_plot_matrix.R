#Scatter plot matrix
source("source/change_prec.R")

install.packages(setdiff(c("GGally"), rownames(installed.packages())))

library(GGally)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_ts.rds"))
prec_data <- prec_data[year(date) >= 1995]
prec_data[date == "2024-12-01" & dataset == "merra-2"]$prec <- NA
prec_data[dataset == "cpc-global", dataset := "CPC-Global"
          ][dataset == "era5-land", dataset := "ERA5-Land"
            ][dataset == "gpcp-v1-3", dataset := "GPCP v1.3"
              ][dataset == "gpm-imerg-v7", dataset := "GPM IMERGF v7"
                ][dataset == "jra-3q", dataset := "JRA-3Q"
                  ][dataset == "merra-2", dataset := "MERRA-2"
                    ][dataset == "mswep-v2-8", dataset := "MSWEP v2.8"
                      ][dataset == "ncep-doe", dataset := "NCEP/DOE R2"]
prec_data <- dcast(prec_data[dataset != "ensemble"], date ~ dataset, value.var = "prec")

ggpairs(prec_data, columns = 2:9) + theme_bw()

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "scatter_matrix.pdf"),
       width = 7*GOLDEN_RATIO, height = 9.5)
