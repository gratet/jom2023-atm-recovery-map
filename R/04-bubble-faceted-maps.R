# Prepare validations per stop and month map
validations_per_stop_and_month_map <- ggplot(validations_per_stop_and_month_map_stops) +
  facet_grid(format(yearmon, '%Y') ~ monthname, switch = "y") +
  labs(caption = 'Active stops (AS) — Total validations per month (V)') +
  geom_sf(data = atm_municipalities, fill = "lightgray", color = "darkgray", size = 0.3) +
  geom_sf(data = borders_atm, fill = NA, color = "darkgray") +
  geom_sf(data = coastline, color = "black", fill = NA, size = 0.3) +
  geom_sf(data = validations_per_stop_and_month_map_stops, aes(color = validations, size = validations), alpha = 0.9) +
  geom_text(mapping = aes(x = 0.7, y = 41.55, label = active_stops), size = 3, hjust = 0) +
  geom_text(mapping = aes(x = 1.6, y = 40.95, label = validations_total), size = 3, hjust = 1) +
  coord_sf(
    xlim = st_bbox(borders_atm_simplified)$xlim,
    ylim = st_bbox(borders_atm_simplified)$ylim,
    datum = NA,
    expand = TRUE
  ) +
  scale_color_viridis(name = "", trans = "log", breaks = c(1, 10, 100, 1000, 10000, 100000), labels = c('1', '10', '100', '1K', '10K', '100K')) +
  scale_size_continuous(name = "Validations per stop", range = c(1, 12), breaks = c(10, 10000, 50000, 100000, 200000), labels = c('1', '10K', '50K', '100K', '200K')) +
  theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    legend.key = element_blank(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    strip.background = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) +
  guides(
    color = guide_colourbar(barwidth = 0.5, barheight = 8, label.position = "left", order = 2),
    size = guide_legend(order = 1)
  )

# Save validations per stop and month map as png and pdf
ggsave(
  plot = validations_per_stop_and_month_map,
  filename = paste0("dist/img/png/validations-per-stop-and-month-map.png"),
  height = 12, width = 50, units = 'cm',
  dpi = 300,
  bg = 'transparent'
)
ggsave(
  plot = validations_per_stop_and_month_map,
  filename = paste0("dist/img/pdf/validations-per-stop-and-month-map.pdf"),
  height = 12, width = 50, units = 'cm',
  dpi = 300,
  bg = 'transparent'
)

