# Creates project paths ----
source("source/main.R")
source("database/06_dataset_fnames_prec.R")

## Paths ----
### Output ----
PATH_SAVE_CHANGING_PREC <- paste0(PATH_SAVE, "changing_prec/")
PATH_SAVE_CHANGING_PREC_RAW <- paste0(PATH_SAVE, "changing_prec/raw/")
PATH_SAVE_CHANGING_PREC_SPATIAL <- paste0(PATH_SAVE, "changing_prec/spatial/")
PATH_SAVE_CHANGING_PREC_FIGURES <- paste0(PATH_SAVE, "changing_prec/figures/")
PATH_SAVE_CHANGING_PREC_TABLES <- paste0(PATH_SAVE, "changing_prec/tables/")
PATH_SAVE_CHANGING_PREC_FIGURES_SUPP <- paste0(PATH_SAVE, "changing_prec/figures/SUPPLEMENT/")
PATH_SAVE_CHANGING_PREC_FIGURES_EXPLORE <- paste0(PATH_SAVE, "changing_prec/figures/Exploration/")
PATH_SAVE_CHANGING_PREC_FIGURES_MAIN <- paste0(PATH_SAVE, "changing_prec/figures/MAIN/")

dir.create(PATH_SAVE_CHANGING_PREC, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGING_PREC_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGING_PREC_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGING_PREC_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGING_PREC_TABLES, showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGING_PREC_FIGURES_SUPP , showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGING_PREC_FIGURES_EXPLORE , showWarnings = FALSE)
dir.create(PATH_SAVE_CHANGING_PREC_FIGURES_MAIN , showWarnings = FALSE)

save(PATH_SAVE_CHANGING_PREC, 
     PATH_SAVE_CHANGING_PREC_RAW,
     PATH_SAVE_CHANGING_PREC_SPATIAL,
     PATH_SAVE_CHANGING_PREC_FIGURES,
     PATH_SAVE_CHANGING_PREC_TABLES,
     PATH_SAVE_CHANGING_PREC_FIGURES_SUPP,
     PATH_SAVE_CHANGING_PREC_FIGURES_EXPLORE,
     PATH_SAVE_CHANGING_PREC_FIGURES_MAIN,
     file = paste0(PATH_SAVE_CHANGING_PREC, "changing_prec_paths.Rdata"))
