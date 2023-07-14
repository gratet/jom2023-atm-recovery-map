suppressPackageStartupMessages(library(dplyr))
library(sf)
library(ggplot2)
theme_set(theme_bw())
theme_update(panel.background = element_rect(fill = "aliceblue"))
library(viridis)

library(ggrepel)
library(ggspatial)
suppressPackageStartupMessages(library(cowplot))


##########################
# Study area and context #
##########################
# Reference map area
study_area_context<-
  data.frame(wkt='POLYGON((-1 39.75, 4 39.75, 4 43.5, -1 43.5, -1 39.75))') %>% 
  st_as_sf(wkt = 1, crs = 4326)

# st_write(study_area_context, dsn = file.path(getwd(), 'data/gis-data.gpkg'), 
#          layer = 'study_area_context', quiet = TRUE)

study_area <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  st_union() %>% 
  st_buffer(0.1) %>% 
  st_bbox() %>% 
  st_as_sfc()

#wkt_study_area<-st_as_text(study_area)

regions_10m <- st_read(dsn = "data/gis-data.gpkg", layer = "regions_10m")

tarragona <- st_read(dsn = "data/gis-data.gpkg", layer = "provinces_10m",query="select * from provinces_10m where name='Tarragona'")
catalonia <- st_read(dsn = "data/gis-data.gpkg", layer = "regions_10m",query="select * from regions_10m where name='Cataluña'")

atm_municipalities <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326)

borders_atm <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>%
  st_transform(4326) %>%
  st_union()

borders_atm_simplified <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>%
  st_transform(4326) %>%
  st_union() %>% 
  st_transform(25831) %>% 
  st_simplify(dTolerance = 1000) %>% 
  st_transform(4326) 

borders_other <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326)

urban <- st_read(dsn = "data/gis-data.gpkg", layer = 'urban', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  filter(habitantes>100)

roads <- st_read(dsn = "data/gis-data.gpkg", layer = 'rt_tramo_vial', stringsAsFactors = F,
                 query = "select * from rt_tramo_vial where st_intersects(geom,st_geomfromtext('POLYGON ((0.985406 41.0519, 1.375216 41.0519, 1.375216 41.18993, 0.985406 41.18993, 0.985406 41.0519))'));") %>% 
  st_transform(4326)

trains <- st_read(dsn = "data/gis-data.gpkg", layer = 'trains', stringsAsFactors = F) %>% 
  st_transform(4326)


#colours
stops_interanual_perc_changes<-
  atm_data_long %>% 
  # mutate(validations=tp+t10+t10_30+t50_30+t70_90+t12+tmes) %>%
  group_by(lat, lon, municipality, stop,
           yearmon, 
           type) %>% 
  summarise(validations=sum(validations)) %>%
  pivot_wider(names_from = yearmon, values_from = validations) %>% 
  replace(is.na(.), 0) %>% 
  mutate("January"=case_when(
    `2019-01-01`==0 & `2021-01-01`== 0 ~ 0,
    `2019-01-01`==0 & `2021-01-01`>= 0 ~ ((`2021-01-01`)-`2019-01-01`/`2021-01-01`*100),
    TRUE ~ ((`2021-01-01`-`2019-01-01`)/`2019-01-01`*100))
  ) %>% 
  mutate("February"=case_when(
    `2019-02-01`==0 & `2021-02-01`== 0 ~ 0,
    `2019-02-01`==0 & `2021-02-01`>= 0 ~ ((`2021-02-01`)-`2019-02-01`/`2021-02-01`*100),
    TRUE ~ ((`2021-02-01`-`2019-02-01`)/`2019-02-01`*100))
  ) %>% 
  mutate("March"=case_when(
    `2019-03-01`==0 & `2021-03-01`== 0 ~ 0,
    `2019-03-01`==0 & `2021-03-01`>= 0 ~ ((`2021-03-01`)-`2019-03-01`/`2021-03-01`*100),
    TRUE ~ ((`2021-03-01`-`2019-03-01`)/`2019-03-01`*100))
  ) %>% 
  mutate("April"=case_when(
    `2019-04-01`==0 & `2021-04-01`== 0 ~ 0,
    `2019-04-01`==0 & `2021-04-01`>= 0 ~ ((`2021-04-01`)-`2019-04-01`/`2021-04-01`*100),
    TRUE ~ ((`2021-04-01`-`2019-04-01`)/`2019-04-01`*100))
  ) %>%
  mutate("May"=case_when(
    `2019-05-01`==0 & `2021-05-01`== 0 ~ 0,
    `2019-05-01`==0 & `2021-05-01`>= 0 ~ ((`2021-05-01`)-`2019-05-01`/`2021-05-01`*100),
    TRUE ~ ((`2021-05-01`-`2019-05-01`)/`2019-05-01`*100))
  ) %>% 
  mutate("June"=case_when(
    `2019-06-01`==0 & `2021-06-01`== 0 ~ 0,
    `2019-06-01`==0 & `2021-06-01`>= 0 ~ ((`2021-06-01`)-`2019-06-01`/`2021-06-01`*100),
    TRUE ~ ((`2021-06-01`-`2019-06-01`)/`2019-06-01`*100))
  ) %>% 
  mutate("July"=case_when(
    `2019-07-01`==0 & `2021-07-01`== 0 ~ 0,
    `2019-07-01`==0 & `2021-07-01`>= 0 ~ ((`2021-07-01`)-`2019-07-01`/`2021-07-01`*100),
    TRUE ~ ((`2021-07-01`-`2019-07-01`)/`2019-07-01`*100))
  ) %>% 
  mutate("August"=case_when(
    `2019-08-01`==0 & `2021-08-01`== 0 ~ 0,
    `2019-08-01`==0 & `2021-08-01`>= 0 ~ ((`2021-08-01`)-`2019-08-01`/`2021-08-01`*100),
    TRUE ~ ((`2021-08-01`-`2019-08-01`)/`2019-08-01`*100))
  ) %>% 
  mutate("September"=case_when(
    `2019-09-01`==0 & `2021-09-01`== 0 ~ 0,
    `2019-09-01`==0 & `2021-09-01`>= 0 ~ ((`2021-09-01`)-`2019-09-01`/`2021-09-01`*100),
    TRUE ~ ((`2021-09-01`-`2019-09-01`)/`2019-09-01`*100))
  ) %>% 
  mutate("October"=case_when(
    `2019-10-01`==0 & `2021-10-01`== 0 ~ 0,
    `2019-10-01`==0 & `2021-10-01`>= 0 ~ ((`2021-10-01`)-`2019-10-01`/`2021-10-01`*100),
    TRUE ~ ((`2021-10-01`-`2019-10-01`)/`2019-10-01`*100))
  ) %>% 
  mutate("November"=case_when(
    `2019-11-01`==0 & `2021-11-01`== 0 ~ 0,
    `2019-11-01`==0 & `2021-11-01`>= 0 ~ ((`2021-11-01`)-`2019-11-01`/`2021-11-01`*100),
    TRUE ~ ((`2021-11-01`-`2019-11-01`)/`2019-11-01`*100))
  ) %>% 
  mutate("December"=case_when(
    `2019-12-01`==0 & `2021-12-01`== 0 ~ 0,
    `2019-12-01`==0 & `2021-12-01`>= 0 ~ ((`2021-12-01`)-`2019-12-01`/`2021-12-01`*100),
    TRUE ~ ((`2021-12-01`-`2019-12-01`)/`2019-12-01`*100))
  ) %>%
  select(lat, lon, municipality, stop,
         type,
         January,February,March,
         April,May,June,July,
         August,September,October,
         November,December) %>% 
  pivot_longer(cols = c('January','February','March',
                        'April','May','June','July',
                        'August','September','October',
                        'November','December'),
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

# Size
stops_2021_validations_per_month<-
  atm_data_long %>% 
  filter(yearmon>'2020-12-31') %>% 
  # mutate(validations=tp+t10+t10_30+t50_30+t70_90+t12+tmes) %>%
  group_by(stop, yearmon) %>% 
  summarise(validations_2021=sum(validations)) %>%
  mutate(monthname=format(yearmon,'%B')) %>% 
  select(stop,monthname,validations_2021)
  
stops_interanual_perc_changes_and_2021<-
  stops_interanual_perc_changes %>% 
  left_join(stops_2021_validations_per_month, by=c('monthname','stop')) %>% 
  replace(is.na(.), 0) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(monthname=factor(monthname, 
                          levels=c('January','February','March',
                                   'April','May','June','July',
                                   'August','September','October',
                                   'November','December')))


map<-
  ggplot(filter(stops_interanual_perc_changes_and_2021, type=='Interurban'))+
  facet_wrap( ~monthname, strip.position = "top",ncol = 4) +
  # labs(x = "Longitude",y="Latitude") +  
  geom_sf(data = catalonia,fill="gray90",color = "black") + 
  geom_sf(data = atm_municipalities,fill="white",color = "gray85",size=0.3) + 
  geom_sf(data = borders_atm,fill=NA,size=0.5, color = "black") +
  geom_sf(data=filter(stops_interanual_perc_changes_and_2021,type=='Interurban'),
          aes(color=validations_perc_change_intervals,size=validations_2021), alpha = 1,shape=21) +
  # geom_sf(data=filter(stops_interanual_perc_changes_and_2020,type=='Interurban'),
  #         aes(size=validations_2020), alpha = 0.5,shape=21,colour = "black") +
  coord_sf(xlim = st_bbox(borders_atm_simplified)$xlim, 
           ylim = st_bbox(borders_atm_simplified)$ylim, 
           # datum = NA,
           expand = TRUE)+
  scale_size_continuous(name="Validations in 2021",range=c(1,12),breaks=c(1000,10000,50000,100000,150000),labels=c('1000','10,000','50,000','100,000','150,000')) +
  scale_color_brewer(name = "% change (2019-2021)",type = 'div',palette = 'RdYlBu') +  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "right",
        legend.justification = "top",
        legend.title=element_text(size=14),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "light blue"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  theme(#strip.text = element_blank(), #remove strip text
        strip.background = element_blank()) +
  guides(colour=guide_legend(override.aes=list(size=5)))


ggsave(filename = paste0("dist/img/png/atm-changes-in-validations-per-interurban-stop-and-month-faceted-map.png"),
       height=200, width=300, units='mm',
       dpi = 300)


# 
# map<-
#   ggplot(filter(stops_interanual_perc_changes_and_2020,type=='Urban'))+
#   facet_wrap( ~monthname, strip.position = "top",ncol = 4) +
#   # labs(x = "Longitude",y="Latitude") +  
#   # geom_sf(data = catalonia,fill="gray90",color = "black") + 
#   geom_sf(data = atm_municipalities,fill="white",color = "gray85",size=0.3) + 
#   geom_sf(data = borders_atm,fill=NA,size=0.5, color = "black") +
#   geom_sf(data=filter(stops_interanual_perc_changes_and_2020,type=='Urban'),
#           aes(color=validations_perc_change_intervals,size=validations_2020), alpha = 1,shape=21) +
#   # geom_sf(data=filter(stops_interanual_perc_changes_and_2020,type=='Interurban'),
#   #         aes(size=validations_2020), alpha = 0.5,shape=21,colour = "black") +
#   coord_sf(xlim = st_bbox(filter(atm_municipalities,name=='Tarragona'))$xlim, 
#            ylim = st_bbox(filter(atm_municipalities,name=='Tarragona'))$ylim, 
#            # datum = NA,
#            expand = TRUE)+
#   scale_size_continuous(name="Validations in 2020",range=c(1,12),breaks=c(1000,10000,50000,100000,150000),labels=c('1000','10,000','50,000','100,000','150,000')) +
#   scale_color_brewer(name = "% change (2019-2020)",type = 'div',palette = 'RdYlBu') +  
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position = "right",
#         legend.justification = "top",
#         legend.title=element_text(size=14),
#         legend.key=element_blank(),
#         panel.background = element_rect(fill = "light blue"),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank()) +
#   theme(#strip.text = element_blank(), #remove strip text
#     strip.background = element_blank()) +
#   guides(colour=guide_legend(override.aes=list(size=5)))
# 
# 
# ggsave(filename = paste0("dist/img/png/atm-changes-in-validations-per-urban-stop-and-month-faceted-map.png"),
#        height=200, width=300, units='mm',
#        dpi = 300)