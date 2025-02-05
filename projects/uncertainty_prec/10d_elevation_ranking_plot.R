# Maps
source("source/uncertainty_prec.R")

## Data
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "elev_ranking.csv"))

prec_data <- prec_data[elev_class != ""]

prec_data[, rankings := frankv(prec_t, order = -1), elev_class]

prec_data[rankings <= 5, rank_group := "Q1", .(dataset)
][rankings > 5 & rankings <= 10, rank_group := "Q2", .(dataset)
][rankings > 10 & rankings <= 15, rank_group := "Q3", .(dataset)
][rankings > 15 & rankings <= 20, rank_group := "Q4", .(dataset)
][, q_count := .N, .(dataset, rank_group)
][dataset == "cmap", dataset := "CMAP"
][dataset == "cmorph-cdr", dataset := "CMORPH CDR"
][dataset == "cpc", dataset := "CPC-Global"
][dataset == "cru-ts-v4-07", dataset := "CRU TS v4.07"
][dataset == "em-earth", dataset := "EM-Earth"
][dataset == "era5", dataset := "ERA5"
][dataset == "era5-land", dataset := "ERA5-Land"
][dataset == "fldas", dataset := "FLDAS"
][dataset == "gpcc-v2020", dataset := "GPCC v2020"
][dataset == "gpcp-v3-2", dataset := "GPCP v3.2"
][dataset == "gpm-imerg-v7", dataset := "GPM-IMERG v7"
][dataset == "gsmap", dataset := "GSMaP v8"
][dataset == "jra55", dataset := "JRA-55"
][dataset == "merra2-land", dataset := "MERRA-2 Land"
][dataset == "mswep-v2-8", dataset := "MSWEP v2.8"
][dataset == "ncep-doe", dataset := "NCEP/DOE R2"
][dataset == "ncep-ncar", dataset := "NCEP/NCAR R1"
][dataset == "persiann-cdr", dataset := "PERSIANN-CDR"
][dataset == "precl", dataset := "PREC/L"
][dataset == "terraclimate", dataset := "TerraClimate"]

setorder(prec_data, rank_group, -q_count)

dummie <- unique(prec_data[,.(dataset,rank_group,q_count)])

data_levels <- rev(c("GPM-IMERG v7", "EM-Earth", "MSWEP v2.8", "PERSIANN-CDR",
                     "ERA5-Land", "FLDAS", "CMAP", "CRU TS v4.07", "GPCC v2020",
                     "PREC/L", "MERRA-2 Land", "CPC-Global", "ERA5",
                     "TerraClimate", "GSMaP v8", "GPCP v3.2", "CMORPH CDR",
                     "JRA-55", "NCEP/DOE R2", "NCEP/NCAR R1"))

prec_data[, dataset := factor(dataset, levels = data_levels)]

p00 <- ggplot(prec_data, aes(y = dataset)) +
  geom_bar(aes(fill = rank_group, x = 100*after_stat(count)/6),
           color = "gray23",
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("Q1" = "#b08f3e", "Q2" = "#a2a2a2",
                               "Q3" = "#9e7b52", "Q4" = "#4c4c4c"),
                    na.value = NA) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Percentage of Elevation Classes", y = "Dataset", fill = "Rank") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks.length.y = unit(0, "cm"))

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "elevation_rank_table.png"),
       width = 4.5*GOLDEN_RATIO, height = 4.5)
