install.packages('scatterpie')
library(scatterpie)
library(ggrepel)

source('source/twc_change.R')
source('source/evap_trend.R')
source('source/geo_functions.R')

avail_flux_change <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_hybrid.rds'))
avail_flux_change_all <- readRDS(paste0(PATH_OUTPUT, 'avail_flux_change_grid_all.rds'))

masks <- pRecipe::pRecipe_masks()
avail_flux_change_classes <- merge(avail_flux_change, masks[land_mask == 'land']) 

# Plots

to_plot <- copy(avail_flux_change_classes)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

ggplot(to_plot[KG_class_1 != "Ocean"]) +
  geom_bar(aes(KG_class_1, fill = Conditions), position="fill") +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  theme_minimal() 

ggplot(to_plot[KG_class_1 != "Ocean"]) +
  geom_bar(aes(KG_class, fill = Conditions), position="fill") +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(to_plot) +
  geom_bar(aes(biome_short_class , fill = Conditions), position="fill") +
  scale_fill_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(to_plot) +
  geom_point(aes(x = avail_change, flux_change, col = Conditions)) +
  scale_color_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  theme_light()

ggplot(to_plot[land_cover_class != "Water"]) +
  geom_point(aes(x = avail_change, flux_change, col = KG_class)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(facets = ~ipcc_region, scales = 'free') +
  theme_light()

# GLOBAL

# Data prep
# === Data prep ===
avail_flux_change_all <- merge(avail_flux_change_classes[, .(lon, lat)], avail_flux_change_all)
grid_cell_area <- unique(avail_flux_change_all[, .(lon, lat)]) %>% grid_area()
avail_flux_change_all <- merge(avail_flux_change_all, grid_cell_area, by = c("lon", "lat"))

avail_flux_change_global <- avail_flux_change_all[!is.na(avail_change), .(
  avail_change = sum(avail_change * area, na.rm = TRUE) / sum(area, na.rm = TRUE),
  flux_change = sum(flux_change * area, na.rm = TRUE) / sum(area, na.rm = TRUE)
), by = dataset]

# === Classify and tag dataset ===
to_plot <- copy(avail_flux_change_global)
to_plot[, dataset := as.character(dataset)]
to_plot[, c("P_dataset", "E_dataset") := tstrsplit(dataset, "-", fixed = TRUE)]
to_plot[dataset %in% c("HYBRID", "BEST KG", "WEIGHTED"), P_dataset := NA_character_]
to_plot$other_dataset <- NA_character_
to_plot[dataset %in% c("HYBRID", "BEST KG", "WEIGHTED"), other_dataset := dataset]

to_plot[, Conditions := "Unknown"]
to_plot[flux_change > 0 & avail_change > 0, Conditions := 'WETTER - ACCELERATED']
to_plot[flux_change < 0 & avail_change > 0, Conditions := 'WETTER - DECCELERATED']
to_plot[flux_change > 0 & avail_change < 0, Conditions := 'DRIER - ACCELERATED']
to_plot[flux_change < 0 & avail_change < 0, Conditions := 'DRIER - DECCELERATED']
to_plot[, Conditions := factor(Conditions, levels = c(
  'WETTER - ACCELERATED', 'WETTER - DECCELERATED',
  'DRIER - ACCELERATED', 'DRIER - DECCELERATED'))]

to_plot[, highlight_group := fifelse(dataset %in% c('HYBRID', 'BEST KG', 'WEIGHTED'),
                                     dataset, 'OTHER')]
to_plot[, highlight_group := factor(highlight_group, levels = c('HYBRID', 'BEST KG', 'WEIGHTED', 'OTHER'))]
to_plot[, fontface := ifelse(highlight_group == "OTHER", "plain", "bold")]

# === Quadrants and label positions ===
quad_rects <- data.table(
  xmin = c(0, 0, -Inf, -Inf),
  xmax = c(Inf, Inf, 0, 0),
  ymin = c(0, -Inf, 0, -Inf),
  ymax = c(Inf, 0, Inf, 0),
  Conditions = levels(to_plot$Conditions)
)
quad_colors <- scales::alpha(PALETTES$water_cycle_change[c(3, 4, 1, 2)], 0.08)

P_labels <- to_plot[, .(x = min(avail_change), y = max(flux_change)), by = P_dataset]
E_labels <- to_plot[, .(x = min(avail_change), y = min(flux_change)), by = E_dataset]
other_labels <- to_plot[, .(x = mean(avail_change), y = min(flux_change)), by = other_dataset]

xmax <- max(to_plot$avail_change, na.rm = TRUE)
xmin <- min(to_plot$avail_change, na.rm = TRUE)
ymax <- max(to_plot$flux_change, na.rm = TRUE)
ymin <- min(to_plot$flux_change, na.rm = TRUE)
buffer_x <- 0.07 * (xmax - xmin)
buffer_y <- 0.001 * (ymax - ymin)

quad_labels <- data.table(
  label = levels(to_plot$Conditions),
  x = c(xmax - buffer_x, xmax - buffer_x, xmin + buffer_x, xmin + buffer_x),
  y = c(ymax - buffer_y, ymin + buffer_y, ymax - buffer_y, ymin + buffer_y)
)

# === Final plot ===
p <- ggplot(to_plot, aes(x = avail_change, y = flux_change)) +
  
  # Background quadrant fill
  geom_rect(data = quad_rects,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Conditions),
            inherit.aes = FALSE, alpha = 0.15) +
  
  # Regression lines (P and E)
  geom_smooth(aes(group = P_dataset), method = "lm", se = FALSE,
              color = "grey70", linewidth = 0.3, linetype = 'dashed') +
  geom_smooth(aes(group = E_dataset), method = "lm", se = FALSE,
              color = "grey70", linewidth = 0.3, linetype = 'dashed') +
  
  # Reference lines
  geom_segment(aes(x = 0, y = 0, xend = avail_change, yend = flux_change, col = Conditions),
               alpha = 0.5, linewidth = 0.4) +
  
  # Base points
  geom_point(data = to_plot[highlight_group == 'OTHER'],
             aes(x = avail_change, y = flux_change), size = 2, color = 'grey40') +
  
  # Highlighted dataset points
  geom_point(data = to_plot[highlight_group != 'OTHER'],
             aes(x = avail_change, y = flux_change, shape = highlight_group),
             size = 3, stroke = 1.1, color = 'black', fill = 'white') +
  
  # Optional connecting lines
  geom_line(aes(group = dataset, col = Conditions), linewidth = 1.5) +
  
  # Manual shape/color scales
  scale_color_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  scale_fill_manual(values = quad_colors, guide = "none") +
  scale_shape_manual(values = c('HYBRID' = 21, 'BEST KG' = 21, 'WEIGHTED' = 21)) +
  
  # Axis lines
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  
  # Regression labels
  geom_label_repel(data = P_labels,
                   aes(x = x, y = y, label = P_dataset),
                   inherit.aes = FALSE,
                   size = 3.5,
                   fill = "lightskyblue3",
                   color = "grey20",
                   label.size = 0.3,
                   label.r = unit(0.15, "lines"),
                   label.padding = unit(0.25, "lines"),
                   segment.color = "black") +
  geom_label_repel(data = E_labels,
                   aes(x = x, y = y, label = E_dataset),
                   inherit.aes = FALSE,
                   size = 3.5,
                   fill = "goldenrod1",
                   color = "grey20",
                   label.size = 0.3,
                   label.r = unit(0.15, "lines"),
                   label.padding = unit(0.25, "lines"),
                   segment.color = "grey50") +
  geom_label_repel(data = other_labels,
                   aes(x = x, y = y, label = other_dataset),
                   inherit.aes = FALSE,
                   size = 3.5,
                   fonttype = "bold",
                   fill = "mediumseagreen",
                   color = "grey20",
                   label.size = 0.3,
                   label.r = unit(0.15, "lines"),
                   label.padding = unit(0.25, "lines"),
                   segment.color = "grey50") +
  
  # Quadrant text
  geom_text(data = quad_labels, aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            fontface = "bold",
            size = 4.2,
            color = PALETTES$water_cycle_change) +
  
  # Axes labels and theme
  xlab(expression(Delta~"(P - E)")) +
  ylab(expression(Delta~((P + E)/2))) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  )

print(p)


# IPCC
ipcc_hexagon <- data.table(read.csv("/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv"))[
  , .(region = Acronym, x = CENTROIX, y = CENTROIY)]

setnames(avail_flux_change_classes, "ipcc_short_region", "region")

to_plot <- avail_flux_change_classes[, .(avail_change, flux_change, region)]

to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot <- merge(to_plot, ipcc_hexagon, by = 'region', allow.cartesian = TRUE)
to_plot <- to_plot[complete.cases(to_plot)]

to_plot_pie <- merge(data.table(table(to_plot[, .(region, Conditions)])), unique(to_plot[, .(region, x, y)]), by = 'region')
to_plot_pie <- dcast(to_plot_pie, region + x + y ~ Conditions, value.var = 'N')

world <- map_data('world')
base_map <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()

base_map + 
  geom_scatterpie(data = to_plot_pie, aes(x, y, group = region), 
                  cols = names(to_plot_pie)[4:ncol(to_plot)]) +
  scale_fill_manual(values = PALETTES$water_cycle_change) +
  theme_void() 



#############################
avail_flux_change_all <- merge(avail_flux_change_classes[, .(lon, lat)], 
                               avail_flux_change_all)

grid_cell_area <- unique(avail_flux_change_all[, .(lon, lat)]) %>% grid_area()
avail_flux_change_all <- merge(avail_flux_change_all, grid_cell_area, by = c("lon", "lat"))

avail_flux_change_global <- avail_flux_change_all[!is.na(avail_change), .(
  avail_change = sum(avail_change * area, na.rm = TRUE) / sum(area, na.rm = TRUE),
  flux_change = sum(flux_change * area, na.rm = TRUE) / sum(area, na.rm = TRUE)
), by = dataset]

to_plot <- copy(avail_flux_change_global)
to_plot[, Conditions := factor("Uknown")]
levels(to_plot$Conditions) <- c('Wetter - Accelerated', 'Wetter - Deccelerated', 'Drier - Accelerated', 'Drier - Deccelerated')
to_plot[flux_change > 0 & avail_change  > 0, Conditions := factor('Wetter - Accelerated')]
to_plot[flux_change < 0 & avail_change  > 0, Conditions := factor('Wetter - Deccelerated')]
to_plot[flux_change > 0 & avail_change  < 0, Conditions := factor('Drier - Accelerated')]
to_plot[flux_change < 0 & avail_change  < 0, Conditions := factor('Drier - Deccelerated')]

to_plot[, dataset := as.character(dataset)]
to_plot[, highlight_group := ifelse(dataset %in% c('HYBRID', 'BEST KG', 'WEIGHTED'), 
                                    dataset, 
                                    'OTHER')]
to_plot[, highlight_group := factor(highlight_group, levels = c('HYBRID', 'BEST KG', 'WEIGHTED', 'OTHER'))]
to_plot[, fontface := ifelse(dataset %in% c('HYBRID', 'BEST KG', 'WEIGHTED'), "bold", "plain")]

ggplot(to_plot) +
  
  # Reference lines to origin
  geom_segment(aes(x = 0, y = 0, xend = avail_change, yend = flux_change, col = Conditions), 
               alpha = 0.5, linewidth = 0.4) +
  
  # All points (light gray base)
  geom_point(data = to_plot[highlight_group == 'OTHER'], 
             aes(y = flux_change, x = avail_change), 
             size = 2, color = 'grey60') +
  
  # Highlighted important dataset pairs
  geom_point(data = to_plot[highlight_group != 'OTHER'], 
             aes(y = flux_change, x = avail_change, shape = highlight_group), 
             size = 3, stroke = 1.1, color = 'black', fill = 'white') + 
  
  # Connecting lines (optional)
  geom_line(aes(y = flux_change, x = avail_change, group = dataset, col = Conditions), 
            alpha = 0.5) +
  
  # Colors, shapes
  scale_color_manual(values = PALETTES$water_cycle_change[c(1, 2, 3, 4)]) +
  scale_shape_manual(values = c('HYBRID' = 21, 'BEST KG' = 21, 'WEIGHTED' = 21)) +
  
  # Labels
  geom_text(
    aes(y = flux_change, x = avail_change, label = dataset, fontface = fontface),
    vjust = -1.5, size = 3, check_overlap = TRUE
  )+
  
  # Axes, theme
  xlab(expression(atop(P - E))) +
  ylab(expression(atop((P + E) / 2))) +
  theme_linedraw() + 
  theme(axis.title.y = element_text(margin = margin(0, -15, 0, 0)))


###############################################

