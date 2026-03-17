plot_change_vectors_pretty_no_facets <- function(mc_reg_change,
                                                 masks,
                                                 region_col = "ipcc_short_region",
                                                 xcol = "dP_minus_E",
                                                 ycol = "dP_plus_E",
                                                 highlight_regions = NULL,
                                                 topN_labels = 12,
                                                 show_quadrant_text = TRUE,
                                                 title = "Regional change vectors",
                                                 palettes = NULL,
                                                 point_alpha = 0.90) {
  
  d <- as.data.table(mc_reg_change)
  d <- d[is.finite(get(xcol)) & is.finite(get(ycol))]
  
  # summarise per region
  s <- d[, .(
    x_med = median(get(xcol), na.rm = TRUE),
    x_q05 = quantile(get(xcol), 0.05, na.rm = TRUE, names = FALSE),
    x_q95 = quantile(get(xcol), 0.95, na.rm = TRUE, names = FALSE),
    y_med = median(get(ycol), na.rm = TRUE),
    y_q05 = quantile(get(ycol), 0.05, na.rm = TRUE, names = FALSE),
    y_q95 = quantile(get(ycol), 0.95, na.rm = TRUE, names = FALSE)
  ), by = region_col]
  
  s <- s[is.finite(x_med) & is.finite(y_med)]
  
  # attach continent
  lut <- make_region_continent_lookup(masks, region_col = region_col, cont_col = "ipcc_continent")
  s <- merge(s, lut, by.x = region_col, by.y = "ipcc_short_region", all.x = TRUE)
  s[is.na(ipcc_continent) | ipcc_continent == "", ipcc_continent := "Other/Unknown"]
  
  # classify quadrant (for background + vector colour)
  s[, Conditions := "Unknown"]
  s[x_med > 0 & y_med > 0, Conditions := "WETTER - ACCELERATED"]
  s[x_med > 0 & y_med < 0, Conditions := "WETTER - DECCELERATED"]
  s[x_med < 0 & y_med > 0, Conditions := "DRIER - ACCELERATED"]
  s[x_med < 0 & y_med < 0, Conditions := "DRIER - DECCELERATED"]
  s[, Conditions := factor(Conditions, levels = c(
    "WETTER - ACCELERATED", "WETTER - DECCELERATED",
    "DRIER - ACCELERATED",  "DRIER - DECCELERATED"
  ))]
  
  # label logic
  s[, vec_len := sqrt(x_med^2 + y_med^2)]
  s[, label_flag := FALSE]
  
  if (!is.null(highlight_regions)) {
    s[get(region_col) %in% highlight_regions, label_flag := TRUE]
  }
  if (!is.null(topN_labels) && topN_labels > 0) {
    s[order(-vec_len), label_flag := label_flag | (seq_len(.N) <= topN_labels)]
  }
  
  # quadrant palette
  if (!is.null(palettes) && "water_cycle_change" %in% names(palettes)) {
    col4 <- palettes$water_cycle_change
    if (length(col4) > 4) col4 <- col4[1:4]
  } else {
    col4 <- c("#2E86AB", "#8FB6E9", "#D95F02", "#F6B26B")
  }
  names(col4) <- levels(s$Conditions)
  quad_fill <- alpha(unname(col4), 0.10)
  names(quad_fill) <- names(col4)
  
  quad_rects <- data.table(
    xmin = c(0, 0, -Inf, -Inf),
    xmax = c(Inf, Inf, 0, 0),
    ymin = c(0, -Inf, 0, -Inf),
    ymax = c(Inf, 0, Inf, 0),
    Conditions = levels(s$Conditions)
  )
  
  # continent palette for points
  cont_levels <- sort(unique(s$ipcc_continent))
  cont_cols <- scales::hue_pal()(length(cont_levels))
  names(cont_cols) <- cont_levels
  
  # quadrant labels (corners, global)
  quad_labels <- NULL
  if (show_quadrant_text) {
    xmax <- max(s$x_med, na.rm = TRUE); xmin <- min(s$x_med, na.rm = TRUE)
    ymax <- max(s$y_med, na.rm = TRUE); ymin <- min(s$y_med, na.rm = TRUE)
    dx <- ifelse(is.finite(xmax - xmin) && (xmax - xmin) > 0, (xmax - xmin), 1)
    dy <- ifelse(is.finite(ymax - ymin) && (ymax - ymin) > 0, (ymax - ymin), 1)
    buffer_x <- 0.05 * dx
    buffer_y <- 0.05 * dy
    
    quad_labels <- data.table(
      Conditions = levels(s$Conditions),
      label = c("Wetter\nAccelerated", "Wetter\nDecelerated",
                "Drier\nAccelerated",  "Drier\nDecelerated"),
      x = c(xmax - buffer_x, xmax - buffer_x, xmin + buffer_x, xmin + buffer_x),
      y = c(ymax - buffer_y, ymin + buffer_y, ymax - buffer_y, ymin + buffer_y)
    )
  }
  
  ggplot(s, aes(x = x_med, y = y_med)) +
    geom_rect(
      data = quad_rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Conditions),
      inherit.aes = FALSE, alpha = 0.18
    ) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
    geom_segment(
      aes(x = 0, y = 0, xend = x_med, yend = y_med, colour = Conditions),
      alpha = 0.55, linewidth = 0.6
    ) +
    geom_errorbarh(aes(xmin = x_q05, xmax = x_q95), height = 0, linewidth = 0.25, alpha = 0.70) +
    geom_errorbar(aes(ymin = y_q05, ymax = y_q95), width = 0, linewidth = 0.25, alpha = 0.70) +
    geom_point(shape = 21, colour = "grey15",
               size = 2.6, stroke = 0.4, alpha = point_alpha) +
    geom_point(
      data = s[label_flag == TRUE],
      size = 3.4, shape = 21, fill = "white", colour = "black", stroke = 1.0
    ) +
    geom_label_repel(
      data = s[label_flag == TRUE],
      aes(label = get(region_col)),
      size = 3.0,
      fill = "white",
      colour = "grey10",
      label.size = 0.2,
      label.r = unit(0.12, "lines"),
      label.padding = unit(0.18, "lines"),
      box.padding = unit(0.25, "lines"),
      point.padding = unit(0.12, "lines"),
      min.segment.length = 0,
      segment.colour = "grey60",
      max.overlaps = Inf
    ) +
    scale_colour_manual(values = col4, guide = "none") +
    scale_fill_manual(values = quad_fill, guide = "none") +
    scale_fill_manual(values = cont_cols, name = "Continent") +
    xlab(expression(Delta~"(P - E)")) +
    ylab(expression(Delta~"(P + E)")) +
    ggtitle(title) +
    theme_classic(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 9),
      axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.key.height = unit(0.9, "lines")
    ) +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 3.3, alpha = 1))) +
    {
      if (show_quadrant_text) {
        geom_text(
          data = quad_labels,
          aes(x = x, y = y, label = label, colour = Conditions),
          inherit.aes = FALSE,
          fontface = "bold",
          size = 3.2,
          alpha = 0.55,
          show.legend = FALSE
        )
      }
    }
}

# Example:
plot_change_vectors_pretty_no_facets(
mc_reg_change, masks,
xcol="dP_minus_E", ycol="dP_plus_E",
highlight_regions=c("MED","WCE","NEU"),
topN_labels=0,
show_quadrant_text=TRUE,
palettes=if (exists("PALETTES")) PALETTES else NULL
)



library(data.table)
library(ggplot2)

plot_mc_density2d_contour <- function(mc_reg_change,
                                      xcol = "dP_minus_E",
                                      ycol = "dP_plus_E",
                                      title = "MC ensemble 2D density (contours)",
                                      zoom_q = c(0.02, 0.98),
                                      bins = 12,
                                      show_points = FALSE,
                                      point_alpha = 0.03,
                                      point_size = 0.4,
                                      contour_colour = "grey20",
                                      contour_lwd = 0.6) {
  
  d <- as.data.table(mc_reg_change)
  d <- d[is.finite(get(xcol)) & is.finite(get(ycol))]
  setnames(d, c(xcol, ycol), c("x", "y"))
  
  # robust plot window to prevent outliers crushing the cloud
  xlim <- as.numeric(quantile(d$x, probs = zoom_q, na.rm = TRUE, names = FALSE))
  ylim <- as.numeric(quantile(d$y, probs = zoom_q, na.rm = TRUE, names = FALSE))
  
  ggplot(d, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.35) +
    geom_vline(xintercept = 0, colour = "black", linewidth = 0.35) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    {
      if (isTRUE(show_points)) {
        geom_point(alpha = point_alpha, size = point_size, colour = "black")
      }
    } +
    geom_density_2d(
      colour = contour_colour,
      linewidth = contour_lwd,
      bins = bins
    ) +
    labs(
      x = expression(Delta~"(P - E)"),
      y = expression(Delta~"(P + E)"),
      title = title
    ) +
    theme_classic(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 9),
      axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0))
    )
}

# Example:
plot_mc_density2d_contour(mc_reg_change,
                               xcol="dP_minus_E", ycol="dP_plus_E",
                               zoom_q=c(0.02,0.98), bins=14,
                               show_points=TRUE)
