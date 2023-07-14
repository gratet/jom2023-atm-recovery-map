########################
# Bivariate choroplets #
########################

atm_municipalities_validations<-
  atm_data_long %>% 
  filter(type=='Interurban') %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_join(atm_municipalities,left = FALSE) %>% 
  # mutate(validations=tp+t10+t10_30+t50_30+t70_90+t12+tmes) %>%
  mutate(year=as.integer(format(yearmon, "%Y"))) %>%
  mutate(lat=y,lon=x) %>% 
  select(label,year,validations,lat,lon) %>% 
  group_by(label,lat,lon,year) %>% 
  summarise(validations=sum(validations)) 
st_geometry(atm_municipalities_validations)<-NULL

atm_municipalities_validations<-
  atm_municipalities_validations %>% 
  pivot_wider(names_from = year,values_from=validations) %>% 
  full_join(atm_municipalities,by=c("label"="label")) %>% 
  select(label,'2019','2021',geom) %>% 
  replace(is.na(.), 0) %>% 
  mutate(perc_change=case_when(
    `2019`==0 & `2021`== 0 ~ 0,
    `2019`==0 & `2021`>= 0 ~ ((`2021`)-`2019`/`2021`*100),
    TRUE ~ ((`2021`-`2019`)/`2019`*100))*-1
  ) %>% 
  st_as_sf()
# plot(atm_municipalities_validations)

# create classes using biscale
# data_quantile <- bi_class(atm_municipalities_validations, x = 'perc_change', y = '2019', style = "quantile", dim = 3)

# quantile(atm_municipalities_validations$perc_change, probs = c(0,0.33,0.66,1))
# quantile(atm_municipalities_validations$`2019`, probs = c(0,0.33,0.66,1))

# create classes manually
interurban_data_quantile<-
  atm_municipalities_validations %>% 
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
interurban_quantile_map <- 
  ggplot() +
  labs(title = "", x = "",y="") +  
  geom_sf(data = interurban_data_quantile, mapping = aes(fill = bi_class), color = "antiquewhite3", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  geom_sf(data = borders_atm, color="grey40", fill = NA,size=0.3) +
  geom_sf(data = coastline, color="black", fill = NA, size=0.3) +
  geom_text_repel(data = filter(atm_municipalities,pop_2018 > 20000),
                  aes(x = x, y = y, label = label,size=pop_2018), color='black',
                  nudge_x = c(0,0,-0.025,0.05,0,0,0.075,-0.025),
                  nudge_y = c(-0.075,-0.075,0.08,-0.08,0,-0.075,-0.075,-0.025))+
  scale_size(
    name = waiver(),
    breaks = waiver(),
    labels = waiver(),
    limits = NULL,
    range = c(4, 6),
    trans = "identity",
    guide = NULL #"legend"
  )+
  annotation_scale(location = "br", width_hint = 0.4,height = unit(0.2, "cm"),style="ticks") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,
                         width = unit(1, "cm"), height = unit(1, "cm"))+
  annotate('text', x = 1.3, y = 41, label = 'M E D I T E R R A N E A N    S E A',
           color = 'grey', size = 6, fontface = 'bold', angle = 20,)+
  coord_sf(xlim = st_bbox(atm_municipalities)$xlim,
           ylim = st_bbox(atm_municipalities)$ylim,
           datum = NA,
           expand=TRUE) +
  theme_void()

# quantile(atm_municipalities_validations$perc_change, probs = c(0,0.33,0.66,1))
# quantile(atm_municipalities_validations$`2019`, probs = c(0,0.33,0.66,1))

# quantile(atm_municipalities_validations$perc_change, probs = c(0,0.25,0.5,0.75,1))
# quantile(atm_municipalities_validations$`2019`, probs = c(0,0.25,0.5,0.75,1))

interurban_legend <- 
  bi_legend(pal = "DkCyan",
            dim = 3,
            xlab = "% loss in 2021",
            ylab = "Validations in 2019",
            size = 12) +
  # scale_x_continuous(labels = c('0','26.3','41.6','100',NA)) +
  # scale_y_continuous(labels = c('0','513 ','4.5K','2.2M',NA)) +
  scale_x_continuous(labels = c('0','3','50','100%',NA)) +
  scale_y_continuous(labels = c('0','1K','50K','2.2M',NA)) +
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
atm_municipalities_interanual_perc_changes<-
  atm_data_long%>% 
  filter(type=='Interurban') %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_join(atm_municipalities,left = FALSE) %>% 
  # mutate(validations=tp+t10+t10_30+t50_30+t70_90+t12+tmes) %>%
  mutate(lat=y,lon=x) %>% 
  select(label,yearmon,validations,lat,lon) %>% 
  group_by(label,yearmon,lat,lon) %>% 
  summarise(validations=sum(validations))
st_geometry(atm_municipalities_interanual_perc_changes)<-NULL


atm_municipalities_interanual_perc_changes<-
  atm_municipalities_interanual_perc_changes %>%
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

interurban_sparklines_data<-
  atm_municipalities_interanual_perc_changes %>% 
  mutate(start_covid=if_else(monthname=='03', ceiling(validations_perc_change),NULL) ) %>% 
  mutate(new_normal=if_else(monthname=='06', ceiling(validations_perc_change),NULL) ) %>% 
  mutate(third_alarm=if_else(monthname=='11', ceiling(validations_perc_change),NULL) ) %>% 
  mutate(end_restrictions=if_else(monthname=='17', ceiling(validations_perc_change),NULL) ) %>% 
  filter(!is.na(monthname)) %>% 
  arrange(monthname)

interurban_sparklines_list <- 
  lapply(unique(interurban_sparklines_data$label), function(label) { 
    sparkline_data<-interurban_sparklines_data[interurban_sparklines_data$label == label,]
    
    gt_plot <- ggplotGrob(
      ggplot(data=sparkline_data) +
        geom_line(aes(x=monthname, y=validations_perc_change),size=0.8,color='dark orange',group = 1,lineend='round',linejoin='round') +
        # stat_smooth(aes(x=monthname, y=validations_perc_change))+
        geom_point(aes(x=monthname,y=start_covid),shape=21,color='dark red',fill='red',size=0.8)+
        geom_point(aes(x=monthname,y=new_normal),shape=21,color='dark blue',fill='blue',size=0.8)+
        geom_point(aes(x=monthname,y=third_alarm),shape=21,color='dark red',fill='red',size=0.8)+
        geom_point(aes(x=monthname,y=end_restrictions),shape=21,color='dark blue',fill='blue',size=1)+
        geom_hline(color='black',size=0.3,yintercept=0)+
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


interurban_sparkline_sizes<-
  atm_municipalities_validations %>% 
  mutate(sparkline_sizes=case_when(
    `2019` > 2000000 ~ 'a',
    `2019` < 2000000 & `2019` > 1000000 ~ 'b',
    `2019` < 1000000 & `2019` > 100000 ~ 'c',
    `2019` < 100000 & `2019` > 20000 ~ 'd',
    TRUE ~ 'e'
  )
  )
very_big_interurban_sparklines<-interurban_sparkline_sizes[interurban_sparkline_sizes$sparkline_sizes=='a',]$label
big_interurban_sparklines<-interurban_sparkline_sizes[interurban_sparkline_sizes$sparkline_sizes=='b',]$label
medium_interurban_sparklines<-interurban_sparkline_sizes[interurban_sparkline_sizes$sparkline_sizes=='c',]$label
small_interurban_sparklines<-interurban_sparkline_sizes[interurban_sparkline_sizes$sparkline_sizes=='d',]$label
very_small_interurban_sparklines<-interurban_sparkline_sizes[interurban_sparkline_sizes$sparkline_sizes=='d',]$label

# Same width
interurban_annotation_list <- lapply(1:length(interurban_sparklines_list), function(i)
  annotation_custom(interurban_sparklines_list[[i]],
                    xmin = interurban_sparklines_data$lon[i] - if(interurban_sparklines_data$label[i]%in% very_big_interurban_sparklines){0.03} else {0.02},
                    xmax = interurban_sparklines_data$lon[i] + if(interurban_sparklines_data$label[i]%in% very_big_interurban_sparklines){0.03} else {0.02},
                    ymin = interurban_sparklines_data$lat[i] - if(interurban_sparklines_data$label[i]%in% very_big_interurban_sparklines){0.04} else if (interurban_sparklines_data$label[i]%in% big_interurban_sparklines){0.03}else if (interurban_sparklines_data$label[i]%in% medium_interurban_sparklines) {0.03}else if (interurban_sparklines_data$label[i]%in% small_interurban_sparklines) {0.01}else {0.010},
                    ymax = interurban_sparklines_data$lat[i] + if(interurban_sparklines_data$label[i]%in% very_big_interurban_sparklines){0.04} else if (interurban_sparklines_data$label[i]%in% big_interurban_sparklines){0.03}else if (interurban_sparklines_data$label[i]%in% medium_interurban_sparklines) {0.03}else if (interurban_sparklines_data$label[i]%in% small_interurban_sparklines) {0.01}else {0.010}
  )
)

interurban_bivariate_sparklines_map <- 
  Reduce(`+`, interurban_annotation_list, interurban_quantile_map)


png("dist/img/png/interurban-bivariate-sparklines-map.png")
print(interurban_bivariate_sparklines_map)
print(interurban_legend, vp = viewport(x = 0.2,y = 0.8,width = unit(0.25, "npc"),
                            height = unit(0.25, "npc"), angle = -45))
dev.off()

pdf("dist/img/pdf/interurban-bivariate-sparklines-map.pdf",width = 12, height = 10)
print(interurban_bivariate_sparklines_map)
# print(interurban_legend, vp = viewport(x = 0.25,y = 0.8,width = unit(0.2, "npc"),
                            # height = unit(0.2, "npc"), angle = -45))
dev.off()

