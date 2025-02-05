source('source/exeves.R')
library(stats)

axis_decimal <- function(x) sprintf("%.1f", x)

colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                          "#F4CC70", "#EBB582",  "#BF9A77",
                          "#E38B75", "#CE5A57",  "#CA3433", "#785A46" )

region <- 'random_locations'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
max_lag <- 10
n_grids <- exeves[, length(unique(KG_class_2))]

names(exeves)[5] <- "evap"
dummy <- exeves[, .(KG_class_2, std_value)]
exeves_acf <- dummy[, sapply(.SD, function(x) acf(x, lag.max = max_lag, plot = FALSE)$acf), KG_class_2]
exeves_acf[, lag := rep(1:(1 + max_lag), n_grids)]
acf_table <- dcast(exeves_acf, KG_class_2~lag, value.var = 'V1')

evap_acf <- melt(acf_table, id.vars = 'KG_class_2', variable.name = 'lag')
colnames(evap_acf)[1] <- 'KG subgroup'
gg_acf <- ggplot(evap_acf, aes(x = lag)) +
  geom_hline(yintercept = 0, col = 'grey50') + 
  geom_line(aes(y = value, col = `KG subgroup`, group = `KG subgroup`)) +
  xlab('Lag (day)') +
  ylab('Auto-correlation coef.') +
  scale_color_manual(values = rev(colset_mid)) +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))
ggsave(paste0(PATH_OUTPUT_FIGURES, "acf_kg.png"), width = 9, height = 4)

sample_year <- 2003
warm_season_start <- as.Date(paste0(sample_year, '-04-01'))
warm_season_end <- as.Date(paste0(sample_year, '-10-01'))
cold_season_start <- copy(warm_season_end)
cold_season_end <-  as.Date(paste0(sample_year + 1, '-04-01'))

definition_names <- data.frame(
  x = warm_season_start + lubridate::days(7),
  y = c(0.6, 0.4, 0.2, 0),
  text = c("Mean/Q90", "Mean/Q90*", "Q75/Q90", "Q80")
)

gg_sample_warm <- ggplot(data = exeves[date >= warm_season_start & date <= warm_season_end]) +
  geom_point(data = exeves[date >= warm_season_start & date <= warm_season_end & !is.na(event_80_95_id)],
             aes(date, 0), col = '#a9cce0', size = 2, shape = 15) +
  geom_point(data = exeves[date >= warm_season_start & date <= warm_season_end & !is.na(event_id)],
             aes(date, -0.1), col = '#7cb47c', size = 2, shape = 15) +
  geom_point(data = exeves[date >= warm_season_start & date <= warm_season_end & !is.na(event_qr_id)],
             aes(date, -0.2), col = '#fcc47c', size = 2, shape = 15) +
  geom_point(data = exeves[date >= warm_season_start & date <= warm_season_end & !is.na(event_80_id)],
             aes(date, -0.3), col = '#c07878', size = 2, shape = 15) +
  geom_point(data = exeves[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_qr_id)],
             aes(date, value), col = colset_subdued_prof[3], size = 4, shape = 0) +
  geom_line(aes(date, value), col = colset_subdued_prof[3]) +
  geom_point(data = exeves[date >= warm_season_start & date <= warm_season_end & !is.na(event_80_95_id)],
             aes(date, value), col = colset_subdued_prof[2], size = 3, alpha = 0.5) +
  geom_point(data = exeves[date >= warm_season_start & date <= warm_season_end & !is.na(extreme_id)],
             aes(date, value), col = colset_subdued_prof[4]) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(labels = axis_decimal) + 
  xlab("Time (day)") + 
  ylab("Evaporation (mm/day)") +
  facet_wrap(~KG_class_2, scales = 'free') + 
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        strip.background = element_rect(fill = 'grey30'))
ggsave(paste0(PATH_OUTPUT_FIGURES, "clustering_kg.png"), width = 9, height = 12)
