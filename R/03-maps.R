# Prepare reference map
reference_map <- ggplot() + 
  labs(title = "", x = "", y = "") +
  geom_sf(data = regions_10m, color = "gray40", fill = "#A3A3A3") +
  geom_sf(data = catalonia_10m, color = "darkgray", fill = "darkgray") +
  geom_sf(data = regions_10m, color = "gray40", fill = NA) +
  geom_sf(data = country_boundaries_50m, color = "black", size = 2.5) +
  geom_sf(data = borders_atm_simplified, color = "darkgray", fill = "lightgray", size = 0.8) +
  geom_sf(data = cities, color = "black") +
  geom_text_repel(inherit.aes = FALSE, data = cities, 
                  aes(x = LONGITUDE, y = LATITUDE, label = NAME), size = 3, nudge_x = 0.025) +
  annotate(geom = "text", x = 2.45, y = 40.8, angle = 45, label = 'M E D I T E R R A N E A N    S E A',
           fontface = "bold.italic", color = "black", size = 4) +
  annotate(geom = "text", x = 1.06, y = 43.1, angle = 0, label = "F r a n c e",
           fontface = "bold", color = "black", size = 6) +
  annotate(geom = "text", x = 0.57, y = 42.41, angle = 0, label = "S p a i n", 
           fontface = "bold", color = "black", size = 6) +
  annotate(geom = "text", x = 1.7, y = 41.8, angle = 0, label = "Catalonia",
           fontface = "bold", color = "black", size = 4) +
  coord_sf(xlim = st_bbox(study_area_context)$xlim, 
           ylim = st_bbox(study_area_context)$ylim,
           datum = st_crs(4326),
           expand = FALSE) +
  scale_y_continuous(breaks = seq(40, 43, by = 1)) +
  scale_x_continuous(breaks = seq(0, 3, by = 1)) +
  theme_void() +
  theme(
    panel.grid.major = element_line(color = "gray30", linetype = "dashed", size = 0.5),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.ontop = TRUE,
    axis.text = element_text(color = "gray30"),
    axis.ticks = element_line(color = "black"),
    plot.background = element_rect(fill = 'transparent', color = NA)  # transparent plot bg
  )

# Save reference map as png and pdf
ggsave(plot = reference_map,
       filename = paste0("dist/img/png/reference-map.png"),
       height = 14, width = 14, units = 'cm',
       dpi = 300,
       bg = 'transparent')
ggsave(plot = reference_map,
       filename = paste0("dist/img/pdf/reference-map.pdf"),
       height = 14, width = 14, units = 'cm',
       dpi = 300,
       bg = 'transparent')

# Prepare ATM stops map
atm_stops_map <- ggplot(data = stops) + 
  labs(x = "", y = "", fill = "") +
  geom_sf(data = atm_municipalities, color = "darkgray", fill = "lightgray", size = 0.2) +
  geom_sf(data = borders_atm, color = "gray30", fill = NA, size = 0.6) +
  geom_sf(data = coastline, color = "black", fill = NA, size = 0.6) +
  geom_sf(aes(fill = type), color = 'black', alpha = 0.9, show.legend = TRUE, size = 1.8, shape = 21) +
  scale_fill_discrete(labels = c(paste0("Interurban stops (n=", interurban_stops_count, ")"), 
                                 paste0("Urban stops (n=", urban_stops_count, ")"))) +
  geom_text_repel(data = filter(atm_municipalities, pop_2018 > 20000),
                  aes(x = x, y = y, label = label, size = pop_2018), color = 'black',
                  nudge_x = c(0, 0, -0.025, 0.05, 0, 0, 0.075, -0.025),
                  nudge_y = c(-0.075, -0.075, 0.08, -0.08, 0, -0.075, -0.075, -0.025)) +
  scale_size(
    name = waiver(),
    breaks = waiver(),
    labels = waiver(),
    limits = NULL,
    range = range(atm_municipalities$pop_2018),
    trans = 'sqrt',
    guide = 'none'
  ) +
  coord_sf(xlim = st_bbox(study_area_atm)$xlim, ylim = st_bbox(study_area_atm)$ylim, datum = st_crs(4326), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent")
  )

# Save ATM stops map as png and pdf
ggsave(plot = atm_stops_map,
       filename = paste0("dist/img/png/atm-stops-map.png"),
       height = 14, width = 14, units = 'cm',
       dpi = 300,
       bg = 'transparent')
ggsave(plot = atm_stops_map,
       filename = paste0("dist/img/pdf/atm-stops-map.pdf"),
       height = 14, width = 14, units = 'cm',
       dpi = 300,
       bg = 'transparent')

# Repeat the above steps for Reus Urban Stops Map and Tarragona Urban Stops Map 
# (Replace the relevant data in the above script)

