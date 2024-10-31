source('source/exeves.R')

library(dplyr)
library(purrr)
library(cowplot)

# Functions 
equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
    d <- s * diff(range(x)) / (1+2*s)
    seq(min(x)+d, max(x)-d, length=n)
  }
}

variable_names <- c('Evaporation', 'Precipitation', 'SW Radiation', 'LW Radiation')
variable_labeller <- function(variable, value){
  return(variable_names[value])
}

# Data 
spatial_changes <- readRDS(file = paste0(PATH_OUTPUT, 'spatial_changes.rds'))
monthly_changes <- readRDS(file = paste0(PATH_OUTPUT, 'monthly_changes.rds'))
borders <- read_sf('../../shared/data/geodata/maps/admin/czechia/CZE_adm0.shp')

# Pre-processing 
dummy <- melt(monthly_changes,  id.vars = c('grid_id', 'month', 'period', 'conditions'))
dummy <- dcast(dummy, grid_id + month + conditions + variable ~ period)
dummy[, total_value_up_to_2001 := sum(up_to_2001), .(grid_id, variable, month)]
dummy[, total_value_after_2001 := sum(after_2001), .(grid_id, variable, month)]

to_plot_1 <- dummy[, .(median = median(ratio_total),
                       q95 = quantile(ratio_total, 0.99),
                       q05 = quantile(ratio_total, 0.01), Conditions = factor('All days')), .(month, variable)]
to_plot_2 <- dummy[, .(median = median(ratio_exeves),
                       q95 = quantile(ratio_exeves, 0.99),
                       q05 = quantile(ratio_exeves, 0.01), Conditions = factor('ExEvEs')), .(month, variable)]
to_plot <- rbind(to_plot_1, to_plot_2)
to_plot <- melt(to_plot,  id.vars = c('month', 'variable', 'Conditions'), variable.name = 'stat')

# Plots 
monthly_plot <- ggplot() +
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(1:3)),
    color = "lightgrey"
  ) + 
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0, 4)),
    color = '#536878', alpha = .9
  ) + 
  geom_col(data = to_plot[stat == 'median' & Conditions == 'ExEvEs'],
           aes(
             x = month,
             y = value,
             fill = value
           ),
           position = "dodge2",
           show.legend = TRUE,
           alpha = .8
  ) +
  geom_point(data = to_plot[stat == 'median' & Conditions == 'All days'],
             aes(
               x = month,
               y = value
             ),
             size = 1.5,
             color =  '#536878'
  ) +
  geom_line(data = to_plot[Conditions == 'All days'],
            aes(
              x = month,
              y = value
            ),
            color =  '#536878'
  ) +
  
  geom_segment(data = to_plot[stat == 'median' & Conditions == 'ExEvEs'],
               aes(
                 x = month,
                 y = 0,
                 xend = month,
                 yend = 4
               ),
               color =  '#536878',
               linetype = 'dotted'
  ) + 
  facet_wrap(~variable, nrow = 2, labeller = variable_labeller) + 
  coord_polar() +
  annotate(
    x = 11.5, 
    y = 1.3, 
    label = "1", 
    geom = "text", 
    color = 'grey50', 
    family = "Bell MT",
    size = 2.6
  ) +
  annotate(
    x = 11.5, 
    y = 2.3, 
    label = "2", 
    geom = "text", 
    color = 'grey50', 
    family = "Bell MT",
    size = 2.6
  ) +
  annotate(
    x = 11.5, 
    y = 3.3, 
    label = "3", 
    geom = "text", 
    color = 'grey50', 
    family = "Bell MT",
    size = 2.6
  ) +
  annotate(
    x = 11.5, 
    y = 4.3, 
    label = "4", 
    geom = "text", 
    color = 'grey50', 
    family = "Bell MT", 
    size = 2.6
  ) +
  scale_y_continuous(
    limits = c(-1.5, 4.5),
  ) + 
  scale_fill_gradientn(
    "Ratio",
    colours =  c('grey97', colset_subdued_prof[2])
  ) +
  guides(
    fill = guide_colorsteps(
      barwidth = 15, 
      barheight = .5, 
      title.position = "top", 
      title.hjust = .5,
      size = 6
    )
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = colset_subdued_prof[3], size = 10, vjust = 0.1),
    legend.position = "bottom",
    legend.margin = margin(-15, 0, 10, 0),
    legend.box.margin = margin(-5, 0, 5, 0),
    legend.title = element_text(size = 10),
    text = element_text(color = colset_subdued_prof[3]),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 12, color = colset_subdued_prof[3]),
    plot.margin = margin(
      b = -5,
      unit = "cm"
    )
  )
monthly_plot
ggsave(paste0(PATH_OUTPUT_FIGURES, "changes_monthly.png"), width = 9, height = 12)


spatial_plot <- spatial_changes[period == "up_to_2001"] %>%
  group_split(variable) %>%
  map(
    ~ggplot(.) +
      geom_tile(
        aes(
          lon, 
          lat, 
          fill = Ratio
        )
      ) +
      geom_sf(
        data = borders, 
        alpha = 0.1, 
        col = 'black', 
        lwd = 0.4
      ) +
      scale_x_continuous(
        breaks = seq(CZECHIA_LON_MIN, CZECHIA_LON_MAX, 2)
      ) +
      scale_y_continuous(
        breaks = seq(CZECHIA_LAT_MIN - 0.5, CZECHIA_LAT_MAX, 1)
      ) +
      scale_fill_gradient2(
        low = 'dodgerblue', 
        mid = "grey90", 
        high = colset_subdued_prof[4], 
        midpoint = 0.97, 
        labels = function(x) sprintf("%.1f", x), 
        breaks = equal_breaks(n = 4, s = 0.05), 
        expand = c(0.05, 0)
      ) +
      facet_wrap(
        ~variable, 
        nrow = 1
      ) +
      xlab('') +
      ylab('') +
      theme_minimal() +
      theme(
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(color = colset_subdued_prof[3]),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.margin = margin(-15, 0, 10, 0),
        legend.box.margin = margin(-5, 0, 5, 0),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.spacing = unit(-5, "cm"),
        plot.margin = margin(
          l = -0.5,
          unit = "cm"
        )
      ) +
      guides(
        fill = guide_colorsteps(
          title.position = "top",
          title.hjust = 0.5,
          barwidth = 6, 
          barheight = .5, 
        ),
        color = guide_legend(
          title.position = "top",
          label.position = "bottom",
          order = 1
        )
      )
  ) %>% 
  plot_grid(
    plotlist = ., 
    align = 'hv', 
    ncol = 4, 
    nrow = 2
  )

ggarrange(monthly_plot, spatial_plot, nrow = 2) + bgcolor("white")      
ggsave(paste0(PATH_OUTPUT_FIGURES, "changes_no_grid.png"), width = 9, height = 12)



