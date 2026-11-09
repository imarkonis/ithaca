library(data.table)

d <- as.data.table(region_sim_means)

# bins: you can change thresholds here (0, terciles, etc.)
d[, prec_bin  := fifelse(prec_change >= 0, "P+", "P-")]
d[, evap_bin  := fifelse(evap_change >= 0, "E+", "E-")]
d[, avail_bin := fifelse(avail_change >= 0, "Avail+", "Avail-")]
d[, flux_bin  := fifelse(flux_change  >= 0, "Flux+",  "Flux-")]

# frequency table for alluvial
paths <- d[, .(freq = .N), by = .(scenario, prec_bin, evap_bin, avail_bin, flux_bin)]

# (optional) order levels so plots are consistent
paths[, prec_bin  := factor(prec_bin,  levels = c("P-","P+"))]
paths[, evap_bin  := factor(evap_bin,  levels = c("E-","E+"))]
paths[, avail_bin := factor(avail_bin, levels = c("Avail-","Avail+"))]
paths[, flux_bin  := factor(flux_bin,  levels = c("Flux-","Flux+"))]

sc <- "base"
tmp <- paths[scenario == sc]

ggplot(tmp,
       aes(y = freq,
           axis1 = prec_bin, axis2 = evap_bin, axis3 = avail_bin, axis4 = flux_bin)) +
  geom_alluvium(alpha = 0.7) +
  geom_stratum(width = 0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("P", "E", "Flux", "Availability"),
                   expand = c(0.05, 0.05)) +
  ggtitle(paste0("Path: P & E → Availability & Flux (", sc, ")")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())


install.packages('parcats')
library(parcats)

d <- as.data.table(region_sim_means)

# base only
d <- d[scenario == "base"]

# bins (sign-based)
d[, prec_bin  := fifelse(prec_change  >= 0, "P+", "P-")]
d[, evap_bin  := fifelse(evap_change  >= 0, "E+", "E-")]
d[, avail_bin := fifelse(avail_change >= 0, "Avail+", "Avail-")]
d[, flux_bin  := fifelse(flux_change  >= 0, "Flux+",  "Flux-")]

# choose which regions to plot
regions_to_plot <- sort(unique(d$region))
# or e.g.: regions_to_plot <- c("MED","WAF","SAH")

out_dir <- "parcats_base_by_region"
dir.create(out_dir, showWarnings = FALSE)

widgets_region <- setNames(lapply(regions_to_plot, function(rg) {
  
  dd <- d[region == rg, .(prec_bin, evap_bin, avail_bin, flux_bin)]
  dd <- as.data.frame(dd)
  
  # easyalluvial plot object
  p <- easyalluvial::alluvial_wide(dd, max_variables = 4)
  
  # interactive widget
  w <- parcats::parcats(
    p,
    marginal_histograms = FALSE,
    data_input = dd
  )
  w
}), regions_to_plot)

# view one in RStudio viewer
widgets_region[[1]]
widgets_region[[3]]
