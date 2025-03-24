#Scatter plot matrix
source("source/change_prec.R")

install.packages(setdiff(c("GGally"), rownames(installed.packages())))

library(GGally)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_CHANGE_PREC, "prec_data_ts.rds"))

prec_data <- dcast(prec_data[dataset != "ensemble"], date ~ dataset, value.var = "prec")


ggpairs(prec_data, columns = 2:10) + theme_bw()

ggsave(paste0(PATH_SAVE_CHANGE_PREC_FIGURES, "scatter_matrix.pdf"),
       width = 7, height = 7)
