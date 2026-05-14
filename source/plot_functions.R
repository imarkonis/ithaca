# Plot a nice map

ggmap <- function(dummie){
  p00 <- ggplot(dummie) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    borders(colour = "black") +
    coord_cartesian(xlim = c(min(dummie$x), max(dummie$x)), 
                    ylim = c(min(dummie$y), max(dummie$y))) +  
    labs(x = "Lon", y = "Lat", fill = prec_name_short) +
    scale_fill_gradient2(low ="#B2182B", mid = "#F7F7F7", high = "#2166AC", midpoint = 0) +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
          panel.grid = element_line(color = "black"))
  y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
  x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
  p01 <- p00 + scale_x_continuous(labels = paste0(x_labs, "\u00b0")) +
    scale_y_continuous(labels = paste0(y_labs, "\u00b0"))
  return(p01)
}

build_region_hex_map <- function(
    dt,
    color_scale   = scale_fill_gradient2(
      low      = "#2166AC",
      mid      = "#F7F7F7",
      high     = "#B2182B",
      midpoint = 0.5,
      na.value = "grey88",
      name     = NULL
    ),
    hexagon_path  = "/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv",
    label_size    = 2.7,
    label_colour  = "black",
    border_colour = "grey40",
    border_width  = 0.35,
    title         = NULL
) {
  
  # Inline helper: apply standard manuscript layout shifts -------------------
  shift_ipcc_hexagons <- function(dt) {
    dt <- copy(as.data.table(dt))
    
    rows_aus <- which(dt$Acronym %in% c("NAU", "CAU", "EAU", "SAU"))
    rows_nz  <- which(dt$Acronym == "NZ")
    rows_mdg <- which(dt$Acronym == "MDG")
    rows_gic <- which(dt$Acronym == "GIC")
    
    dt$long[rows_gic] <- dt$long[rows_gic] - 7
    dt$lat[rows_gic]  <- dt$lat[rows_gic]  - 4
    dt$V1[rows_gic]   <- dt$V1[rows_gic]   - 7
    dt$V2[rows_gic]   <- dt$V2[rows_gic]   - 4
    
    dt$long[rows_mdg] <- dt$long[rows_mdg] - 7
    dt$lat[rows_mdg]  <- dt$lat[rows_mdg]  - 3
    dt$V1[rows_mdg]   <- dt$V1[rows_mdg]   - 7
    dt$V2[rows_mdg]   <- dt$V2[rows_mdg]   - 3
    
    dt$long[rows_aus] <- dt$long[rows_aus] + 5
    dt$lat[rows_aus]  <- dt$lat[rows_aus]  + 12
    dt$V1[rows_aus]   <- dt$V1[rows_aus]   + 5
    dt$V2[rows_aus]   <- dt$V2[rows_aus]   + 12
    
    dt$long[rows_nz]  <- dt$long[rows_nz]  + 10
    dt$lat[rows_nz]   <- dt$lat[rows_nz]   + 9
    dt$V1[rows_nz]    <- dt$V1[rows_nz]    + 10
    dt$V2[rows_nz]    <- dt$V2[rows_nz]    + 9
    
    dt
  }
  
  # Validate & load -----------------------------------------------------------
  dt <- copy(as.data.table(dt))
  stopifnot(all(c("region", "value") %in% names(dt)))
  
  hex_dt <- data.table(read.csv(hexagon_path))
  
  out <- merge(
    hex_dt,
    dt[, .(Acronym = region, value)],
    by              = "Acronym",
    all.x           = TRUE,
    allow.cartesian = TRUE,
    sort            = FALSE
  )
  
  out <- shift_ipcc_hexagons(out)
  
  # Plot ----------------------------------------------------------------------
  p <- ggplot(out) +
    geom_polygon(
      aes(x = long, y = lat, group = group, fill = value),
      colour    = border_colour,
      linewidth = border_width
    ) +
    color_scale +
    coord_equal(expand = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.title        = element_text(face = "bold", hjust = 0.5, size = 12),
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.key.width  = unit(0.80, "cm"),
      legend.key.height = unit(0.62, "cm"),
      legend.spacing.x  = unit(0.22, "cm"),
      legend.text       = element_text(size = 8),
      plot.margin       = margin(2, 2, 2, 2)
    )
  
  if (!is.null(label_size) && label_size > 0 && !is.null(label_colour)) {
    label_dt <- unique(out[, .(Acronym, V1, V2)])
    p <- p + geom_text(
      data   = label_dt,
      aes(x = V1, y = V2, label = Acronym),
      size   = label_size,
      colour = label_colour
    )
  }
  
  p
}