########################
# Bivariate choroplets #
########################

neighbourhood_validations<-
  atm_data_long %>% 
  filter(type=='Urban') %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_join(neighbourhoods,left = FALSE) %>% 
  # mutate(validations=tp+t10+t10_30+t50_30+t70_90+t12+tmes) %>%
  mutate(year=as.integer(format(yearmon, "%Y"))) %>%
  select(label,year,validations,lat,lon) %>% 
  group_by(label,lat,lon,year) %>% 
  summarise(validations=sum(validations)) 
st_geometry(neighbourhood_validations)<-NULL

neighbourhood_validations<-
  neighbourhood_validations %>% 
  pivot_wider(names_from = year,values_from=validations) %>% 
  full_join(neighbourhoods,by=c("label"="label")) %>% 
  select(label,'2019','2021',geom) %>% 
  replace(is.na(.), 0) %>% 
  mutate(perc_change=case_when(
    `2019`==0 & `2021`== 0 ~ 0,
    `2019`==0 & `2021`>= 0 ~ ((`2021`)-`2019`/`2021`*100),
    TRUE ~ ((`2021`-`2019`)/`2019`*100))*-1
  ) %>% 
  st_as_sf()


# create classes using biscale
# data_quantile <- bi_class(neighbourhood_validations, x = 'perc_change', y = '2019', style = "quantile", dim = 3)

# quantile(neighbourhood_validations$perc_change, probs = c(0,0.33,0.66,1))
# quantile(neighbourhood_validations$`2019`, probs = c(0,0.33,0.66,1))

# create classes manually
urban_data_quantile<-
  neighbourhood_validations %>% 
  mutate(bi_class=
           case_when(
             `2019`<1000 && perc_change <3 ~ '1-1',
             `2019`<1000 && between(perc_change,3,50) ~ '2-1',
             `2019`<1000 && perc_change >50 ~ '3-1',
             
             between(`2019`,1000,50000)  && perc_change <3 ~ '1-2',
             between(`2019`,1000,50000) && between(perc_change,3,50) ~ '2-2',
             between(`2019`,1000,50000)  && perc_change >50  ~ '3-2',
             
             `2019`>50000&& perc_change <3 ~ '1-3',
             `2019`>50000 && between(perc_change,3,50) ~ '2-3',
             `2019`>50000 && perc_change >50 ~ '3-3',
           )
         )

# create map
urban_quantile_map <- 
  ggplot() +
  labs(title = "", x = "",y="") +  
  geom_sf(data = urban_data_quantile, mapping = aes(fill = bi_class), color = "antiquewhite3", size = 0.2, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  geom_sf(data = francoli, color="grey40", fill = "grey40") +
  geom_sf(data = roads, color="antiquewhite3", fill = "antiquewhite3",alpha=0.8,size=0.1) +
  geom_sf(data = filter(atm_municipalities,label%in%c('Tarragona','Reus')), color="grey40", fill = NA) +
  geom_sf(data = coastline, color="grey20", fill = NA, size=0.8) +
  annotation_scale(location = "br", width_hint = 0.4,height = unit(0.2, "cm"),style="ticks") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,
                         width = unit(1, "cm"), height = unit(1, "cm"))+
  # annotate('text', x = 1.3, y = 41.11, label = 'M E D I T E R R A N E A N    S E A',
           # color = 'grey', size = 6, fontface = 'bold', angle = 20,)+
  coord_sf(xlim = st_bbox(roads_clip)$xlim,
           ylim = st_bbox(roads_clip)$ylim,
           datum = NA,
           expand=FALSE) +
  theme_void()


# quantile(neighbourhood_validations$perc_change, probs = c(0,0.33,0.66,1))
# quantile(neighbourhood_validations$`2019`, probs = c(0,0.33,0.66,1))

urban_legend <- 
  bi_legend(pal = "DkCyan",
            dim = 3,
            xlab = "% loss in 2021",
            ylab = "Validations in 2019",
            size = 12) +
  # scale_x_continuous(labels = c('0','2.4','44.5','80%',NA)) +
  # scale_y_continuous(labels = c('0','545','35K','1.9M',NA)) +
  scale_x_continuous(labels = c('0','3','50','80%',NA)) +
  scale_y_continuous(labels = c('0','1K','50K','1.9M',NA)) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size=10,angle = 45),#hjust = -2.8, vjust = 10),
        axis.text.y = element_text(size=10,angle = 45),#vjust = -7, hjust = 3),
        panel.grid.major = element_line(color = NA),
        axis.ticks = element_line(color=NA),
        panel.background = element_rect(fill = NA,colour = NA),
        plot.background = element_rect(fill=NA, color=NA))



##############
# Sparklines #
##############
urban_interanual_perc_changes<-
  atm_data_long %>% 
  filter(type=='Urban') %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_join(neighbourhoods,left = FALSE) %>% 
  # mutate(validations=tp+t10+t10_30+t50_30+t70_90+t12+tmes) %>%
  select(label,yearmon,validations,lat,lon) %>% 
  group_by(label,yearmon,lat,lon) %>% 
  summarise(validations=sum(validations))
st_geometry(urban_interanual_perc_changes)<-NULL


urban_interanual_perc_changes<-
  urban_interanual_perc_changes %>%
  pivot_wider(names_from = yearmon, values_from = validations) %>% 
  replace(is.na(.), 0) %>% 
  mutate("01"=case_when(
    `2019-01-01`==0 & `2020-01-01`== 0 ~ 0,
    `2019-01-01`==0 & `2020-01-01`>= 0 ~ ((`2020-01-01`)-`2019-01-01`/`2020-01-01`*100),
    TRUE ~ ((`2020-01-01`-`2019-01-01`)/`2019-01-01`*100))
  ) %>% 
  mutate("02"=case_when(
    `2019-02-01`==0 & `2020-02-01`== 0 ~ 0,
    `2019-02-01`==0 & `2020-02-01`>= 0 ~ ((`2020-02-01`)-`2019-02-01`/`2020-02-01`*100),
    TRUE ~ ((`2020-02-01`-`2019-02-01`)/`2019-02-01`*100))
  ) %>% 
  mutate("03"=case_when(
    `2019-03-01`==0 & `2020-03-01`== 0 ~ 0,
    `2019-03-01`==0 & `2020-03-01`>= 0 ~ ((`2020-03-01`)-`2019-03-01`/`2020-03-01`*100),
    TRUE ~ ((`2020-03-01`-`2019-03-01`)/`2019-03-01`*100))
  ) %>% 
  mutate("04"=case_when(
    `2019-04-01`==0 & `2020-04-01`== 0 ~ 0,
    `2019-04-01`==0 & `2020-04-01`>= 0 ~ ((`2020-04-01`)-`2019-04-01`/`2020-04-01`*100),
    TRUE ~ ((`2020-04-01`-`2019-04-01`)/`2019-04-01`*100))
  ) %>%
  mutate("05"=case_when(
    `2019-05-01`==0 & `2020-05-01`== 0 ~ 0,
    `2019-05-01`==0 & `2020-05-01`>= 0 ~ ((`2020-05-01`)-`2019-05-01`/`2020-05-01`*100),
    TRUE ~ ((`2020-05-01`-`2019-05-01`)/`2019-05-01`*100))
  ) %>% 
  mutate("06"=case_when(
    `2019-06-01`==0 & `2020-06-01`== 0 ~ 0,
    `2019-06-01`==0 & `2020-06-01`>= 0 ~ ((`2020-06-01`)-`2019-06-01`/`2020-06-01`*100),
    TRUE ~ ((`2020-06-01`-`2019-06-01`)/`2019-06-01`*100))
  ) %>% 
  mutate("07"=case_when(
    `2019-07-01`==0 & `2020-07-01`== 0 ~ 0,
    `2019-07-01`==0 & `2020-07-01`>= 0 ~ ((`2020-07-01`)-`2019-07-01`/`2020-07-01`*100),
    TRUE ~ ((`2020-07-01`-`2019-07-01`)/`2019-07-01`*100))
  ) %>% 
  mutate("08"=case_when(
    `2019-08-01`==0 & `2020-08-01`== 0 ~ 0,
    `2019-08-01`==0 & `2020-08-01`>= 0 ~ ((`2020-08-01`)-`2019-08-01`/`2020-08-01`*100),
    TRUE ~ ((`2020-08-01`-`2019-08-01`)/`2019-08-01`*100))
  ) %>% 
  mutate("09"=case_when(
    `2019-09-01`==0 & `2020-09-01`== 0 ~ 0,
    `2019-09-01`==0 & `2020-09-01`>= 0 ~ ((`2020-09-01`)-`2019-09-01`/`2020-09-01`*100),
    TRUE ~ ((`2020-09-01`-`2019-09-01`)/`2019-09-01`*100))
  ) %>% 
  mutate("10"=case_when(
    `2019-10-01`==0 & `2020-10-01`== 0 ~ 0,
    `2019-10-01`==0 & `2020-10-01`>= 0 ~ ((`2020-10-01`)-`2019-10-01`/`2020-10-01`*100),
    TRUE ~ ((`2020-10-01`-`2019-10-01`)/`2019-10-01`*100))
  ) %>% 
  mutate("11"=case_when(
    `2019-11-01`==0 & `2020-11-01`== 0 ~ 0,
    `2019-11-01`==0 & `2020-11-01`>= 0 ~ ((`2020-11-01`)-`2019-11-01`/`2020-11-01`*100),
    TRUE ~ ((`2020-11-01`-`2019-11-01`)/`2019-11-01`*100))
  ) %>% 
  mutate("12"=case_when(
    `2019-12-01`==0 & `2020-12-01`== 0 ~ 0,
    `2019-12-01`==0 & `2020-12-01`>= 0 ~ ((`2020-12-01`)-`2019-12-01`/`2020-12-01`*100),
    TRUE ~ ((`2020-12-01`-`2019-12-01`)/`2019-12-01`*100))
  ) %>%
  mutate("13"=case_when(
    `2019-01-01`==0 & `2021-01-01`== 0 ~ 0,
    `2019-01-01`==0 & `2021-01-01`>= 0 ~ ((`2021-01-01`)-`2019-01-01`/`2021-01-01`*100),
    TRUE ~ ((`2021-01-01`-`2019-01-01`)/`2019-01-01`*100))
  ) %>% 
  mutate("14"=case_when(
    `2019-02-01`==0 & `2021-02-01`== 0 ~ 0,
    `2019-02-01`==0 & `2021-02-01`>= 0 ~ ((`2021-02-01`)-`2019-02-01`/`2021-02-01`*100),
    TRUE ~ ((`2021-02-01`-`2019-02-01`)/`2019-02-01`*100))
  ) %>% 
  mutate("15"=case_when(
    `2019-03-01`==0 & `2021-03-01`== 0 ~ 0,
    `2019-03-01`==0 & `2021-03-01`>= 0 ~ ((`2021-03-01`)-`2019-03-01`/`2021-03-01`*100),
    TRUE ~ ((`2021-03-01`-`2019-03-01`)/`2019-03-01`*100))
  ) %>% 
  mutate("16"=case_when(
    `2019-04-01`==0 & `2021-04-01`== 0 ~ 0,
    `2019-04-01`==0 & `2021-04-01`>= 0 ~ ((`2021-04-01`)-`2019-04-01`/`2021-04-01`*100),
    TRUE ~ ((`2021-04-01`-`2019-04-01`)/`2019-04-01`*100))
  ) %>%
  mutate("17"=case_when(
    `2019-05-01`==0 & `2021-05-01`== 0 ~ 0,
    `2019-05-01`==0 & `2021-05-01`>= 0 ~ ((`2021-05-01`)-`2019-05-01`/`2021-05-01`*100),
    TRUE ~ ((`2021-05-01`-`2019-05-01`)/`2019-05-01`*100))
  ) %>% 
  mutate("18"=case_when(
    `2019-06-01`==0 & `2021-06-01`== 0 ~ 0,
    `2019-06-01`==0 & `2021-06-01`>= 0 ~ ((`2021-06-01`)-`2019-06-01`/`2021-06-01`*100),
    TRUE ~ ((`2021-06-01`-`2019-06-01`)/`2019-06-01`*100))
  ) %>% 
  mutate("19"=case_when(
    `2019-07-01`==0 & `2021-07-01`== 0 ~ 0,
    `2019-07-01`==0 & `2021-07-01`>= 0 ~ ((`2021-07-01`)-`2019-07-01`/`2021-07-01`*100),
    TRUE ~ ((`2021-07-01`-`2019-07-01`)/`2019-07-01`*100))
  ) %>% 
  mutate("20"=case_when(
    `2019-08-01`==0 & `2021-08-01`== 0 ~ 0,
    `2019-08-01`==0 & `2021-08-01`>= 0 ~ ((`2021-08-01`)-`2019-08-01`/`2021-08-01`*100),
    TRUE ~ ((`2021-08-01`-`2019-08-01`)/`2019-08-01`*100))
  ) %>% 
  mutate("21"=case_when(
    `2019-09-01`==0 & `2021-09-01`== 0 ~ 0,
    `2019-09-01`==0 & `2021-09-01`>= 0 ~ ((`2021-09-01`)-`2019-09-01`/`2021-09-01`*100),
    TRUE ~ ((`2021-09-01`-`2019-09-01`)/`2019-09-01`*100))
  ) %>% 
  mutate("22"=case_when(
    `2019-10-01`==0 & `2021-10-01`== 0 ~ 0,
    `2019-10-01`==0 & `2021-10-01`>= 0 ~ ((`2021-10-01`)-`2019-10-01`/`2021-10-01`*100),
    TRUE ~ ((`2021-10-01`-`2019-10-01`)/`2019-10-01`*100))
  ) %>% 
  mutate("23"=case_when(
    `2019-11-01`==0 & `2021-11-01`== 0 ~ 0,
    `2019-11-01`==0 & `2021-11-01`>= 0 ~ ((`2021-11-01`)-`2019-11-01`/`2021-11-01`*100),
    TRUE ~ ((`2021-11-01`-`2019-11-01`)/`2019-11-01`*100))
  ) %>% 
  mutate("24"=case_when(
    `2019-12-01`==0 & `2021-12-01`== 0 ~ 0,
    `2019-12-01`==0 & `2021-12-01`>= 0 ~ ((`2021-12-01`)-`2019-12-01`/`2021-12-01`*100),
    TRUE ~ ((`2021-12-01`-`2019-12-01`)/`2019-12-01`*100))
  ) %>% 
  select(label,lat,lon,
         "01","02","03",
         "04","05","06",
         "07","08","09",
         "10","11","12",
         "13","14","15",
         "16","17","18",
         "19","20","21",
         "22","23","24"
  ) %>%
  pivot_longer(cols = c(
    "01","02","03",
    "04","05","06",
    "07","08","09",
    "10","11","12",
    "13","14","15",
    "16","17","18",
    "19","20","21",
    "22","23","24"
  ),
  names_to = "monthname", values_to = "validations_perc_change"
  ) %>% 
  mutate(validations_perc_change=as.integer(validations_perc_change)) %>% 
  ungroup() %>% 
  mutate(validations_perc_change_intervals=
           case_when(
             validations_perc_change < -50 ~ '( ,-50%)',
             (validations_perc_change >= -50) & (validations_perc_change < -25) ~ '[-50%, -25%)',
             (validations_perc_change >= -25) & (validations_perc_change < -5) ~ '[-25%, -5%)',
             (validations_perc_change >= -5) & (validations_perc_change < 5) ~ '[-5%, 5%)',
             (validations_perc_change >= 5) & (validations_perc_change < 25) ~ '[5%, 25%)',
             (validations_perc_change >= 25) & (validations_perc_change < 50) ~ '[25%, 50%)',
             validations_perc_change >= 50 ~ '[50%,)'
           )
  ) %>% 
  mutate(validations_perc_change_intervals=factor(validations_perc_change_intervals, 
                                                  levels=c('( ,-50%)',
                                                           '[-50%, -25%)',
                                                           '[-25%, -5%)',
                                                           '[-5%, 5%)',
                                                           '[5%, 25%)',
                                                           '[25%, 50%)',
                                                           '[50%,)')))

urban_sparklines_data<-
  urban_interanual_perc_changes %>% 
  mutate(start_covid=if_else(monthname=='03', ceiling(validations_perc_change),NULL) ) %>% 
  mutate(new_normal=if_else(monthname=='06', ceiling(validations_perc_change),NULL) ) %>% 
  mutate(third_alarm=if_else(monthname=='11', ceiling(validations_perc_change),NULL) ) %>% 
  mutate(end_restrictions=if_else(monthname=='17', ceiling(validations_perc_change),NULL) ) %>% 
  filter(!is.na(monthname)) %>% 
  arrange(monthname)

urban_sparklines_list <- 
  lapply(unique(urban_sparklines_data$label), function(label) { 
    sparkline_data<-urban_sparklines_data[urban_sparklines_data$label == label,]
    
    gt_plot <- ggplotGrob(
      ggplot(data=sparkline_data) +
        geom_line(aes(x=monthname, y=validations_perc_change),size=0.8,color='dark orange',group = 1,lineend='round',linejoin='round') +
        # stat_smooth(aes(x=monthname, y=validations_perc_change))+
        geom_point(aes(x=monthname,y=start_covid),shape=21,color='dark red',fill='red',size=0.8)+
        geom_point(aes(x=monthname,y=new_normal),shape=21,color='dark blue',fill='blue',size=0.8)+
        geom_point(aes(x=monthname,y=third_alarm),shape=21,color='dark red',fill='red',size=0.8)+
        geom_point(aes(x=monthname,y=end_restrictions),shape=21,color='dark blue',fill='blue',size=0.8)+
        geom_hline(color='black',size=0.5,yintercept=0)+
        geom_point(x=12,y=0,shape=108,color='black',fill='black',size=0.8)+
        # annotate("rect", xmin = '03', xmax = '06', ymin = -100, ymax = 100, alpha = .1,fill = "blue") +
        # geom_text(aes(x=monthname,y=start_covid, label = scales::comma(start_covid)), size=2.25, angle=0, vjust = 0.5, hjust = 1,color='red') +
        labs(x = NULL, y = NULL) + 
        ylim(-110,110)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })


urban_sparkline_sizes<-
  neighbourhood_validations %>% 
  mutate(sparkline_sizes=case_when(
    `2019` > 1950000 ~ 'a',
    `2019` < 1950000 & `2019` > 1000000 ~ 'b',
    `2019` < 1000000 & `2019` > 100000 ~ 'c',
    `2019` < 100000 & `2019` > 10000 ~ 'd',
    TRUE ~ 'e'))

very_big_urban_sparklines<-urban_sparkline_sizes[urban_sparkline_sizes$sparkline_sizes=='a',]$label
big_urban_sparklines<-urban_sparkline_sizes[urban_sparkline_sizes$sparkline_sizes=='b',]$label
medium_urban_sparklines<-urban_sparkline_sizes[urban_sparkline_sizes$sparkline_sizes=='c',]$label
small_urban_sparklines<-urban_sparkline_sizes[urban_sparkline_sizes$sparkline_sizes=='d',]$label
very_small_urban_sparklines<-urban_sparkline_sizes[urban_sparkline_sizes$sparkline_sizes=='e',]$label

# Same width
urban_annotation_list <- lapply(1:length(urban_sparklines_list), function(i)
  annotation_custom(urban_sparklines_list[[i]],
                    xmin = urban_sparklines_data$lon[i] - if(urban_sparklines_data$label[i]%in% very_big_urban_sparklines){0.005} else {0.0025},
                    xmax = urban_sparklines_data$lon[i] + if(urban_sparklines_data$label[i]%in% very_big_urban_sparklines){0.005} else {0.0025},
                    ymin = urban_sparklines_data$lat[i] - if(urban_sparklines_data$label[i]%in% very_big_urban_sparklines){0.007} else if (urban_sparklines_data$label[i]%in% big_urban_sparklines){0.005}else if (urban_sparklines_data$label[i]%in% medium_urban_sparklines) {0.004}else if (urban_sparklines_data$label[i]%in% small_urban_sparklines) {0.003}else {0.002},
                    ymax = urban_sparklines_data$lat[i] + if(urban_sparklines_data$label[i]%in% very_big_urban_sparklines){0.007} else if (urban_sparklines_data$label[i]%in% big_urban_sparklines){0.005}else if (urban_sparklines_data$label[i]%in% medium_urban_sparklines) {0.004}else if (urban_sparklines_data$label[i]%in% small_urban_sparklines) {0.003}else {0.002}
  )
)


urban_bivariate_sparklines_map <- 
  Reduce(`+`, urban_annotation_list, urban_quantile_map)


png("dist/img/png/urban-bivariate-sparklines-map.png",width = 14, height = 9)
print(urban_bivariate_sparklines_map)
#print(legend, vp = viewport(x = 0.45,y = 0.75,width = unit(0.25, "npc"),
#                            height = unit(0.25, "npc"), angle = -45))
dev.off()

pdf("dist/img/pdf/urban-bivariate-sparklines-map.pdf",width = 32, height = 16)
print(urban_bivariate_sparklines_map)
#print(urban_legend, vp = viewport(x = 0.45,y = 0.75,width = unit(0.25, "npc"),
#                            height = unit(0.25, "npc"), angle = -45))
dev.off()

