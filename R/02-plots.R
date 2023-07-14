# Plot results
# First, filter the data by date (after 2018-12-31)
atm_data_long %>% 
  filter(yearmon > "2018-12-31") %>% 
  
  # Group the data by year/month, fare, and type, and summarize the total validations
  group_by(yearmon, fare, type) %>% 
  summarise(validations = sum(validations)) %>% 
  
  # Add a new column for the abbreviated month name
  mutate(monthname = factor(format(yearmon,'%b'), 
                            levels = c('Jan','Feb','Mar',
                                       'Apr','May','Jun','Jul',
                                       'Aug','Sep','Oct',
                                       'Nov','Dec'))) %>% 
  
  # Begin constructing the plot
  ggplot(., aes(x = monthname, y = validations, fill = factor(fare))) +
  
  # Add a bar for each combination of month and fare
  geom_bar(stat = "identity")+
  
  # Specify the color palette, labels, and guide for the fill (fare) aesthetic
  scale_fill_brewer(name = "Fares",
                    labels = c('T-10','T10/30','T12','T50/30','T70/90','TMES','TP'),
                    palette = "Dark2",
                    direction = -1) +
  
  # Specify the labels and breaks for the y axis
  scale_y_continuous(labels = c('500k','1M','1.5M','2M'),
                     breaks = c(500e3, 1e6, 1.5e6, 2e6))+
  
  # Add a facet for each combination of type and year
  facet_grid(type ~ format(yearmon, "%Y"), 
             scales = "free_x", # Let the x axis vary across facets.
             space = "free_x"#,  # Let the width of facets vary and force all bars to have the same width.
  )+ 
  
  # Add labels for the x and y axes
  labs(x = "Month", y="Validations")

# Save the plot as a PNG and a PDF
ggsave("dist/img/png/atm-trips-per-month-fare-type.png", height=7, width=28, units='cm')
ggsave("dist/img/pdf/atm-trips-per-month-fare-type.pdf", height=7, width=28, units='cm')

