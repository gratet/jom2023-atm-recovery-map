# Load required libraries
# library(sf)

######################
# Reference map data #
######################

# Define the study area
study_area_context <- data.frame(wkt='POLYGON((-1 39.75, 4 39.75, 4 43.5, -1 43.5, -1 39.75))') %>% 
  st_as_sf(wkt = 1, crs = 4326)

# Define the study area using WKT format
wkt_study_area_context <- 'POLYGON((-1 39.75, 4 39.75, 4 43.5, -1 43.5, -1 39.75))'

# Read and process the study area data from a Geopackage file
study_area <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326) %>% # Transform the coordinate system to WGS 84
  st_union() %>%         # Merge features into a single feature
  st_buffer(0.1) %>%     # Create a buffer around the feature
  st_bbox() %>%          # Get the bounding box of the feature
  st_as_sfc()            # Convert the bounding box to simple feature

# Convert the study area to WKT format
wkt_study_area <- st_as_text(study_area)

# Read country boundaries data from the Geopackage
country_boundaries_50m <- st_read(dsn = "data/gis-data.gpkg", layer = "country_boundaries_50m")

# Read coastline data from the Geopackage and transform it to WGS 84
coastline <- st_read(dsn = "data/gis-data.gpkg", layer = "linea_costa_ign_Peninsula_l") %>% 
  st_transform(4326)

# Read regional data from the Geopackage
regions_10m <- st_read(dsn = "data/gis-data.gpkg", layer = "regions_10m")

# Read specific regional data (Tarragona, Catalonia) from the Geopackage using SQL queries
tarragona_10m <- st_read(dsn = "data/gis-data.gpkg", layer = "provinces_10m",
                          query="select * 
                          from provinces_10m 
                          where name='Tarragona';")

catalonia_10m <- st_read(dsn = "data/gis-data.gpkg", layer = "regions_10m",
                          query="select * 
                          from regions_10m 
                          where name='Cataluña';")

# Read city data from the Geopackage using SQL queries
cities <- st_read(dsn = "data/gis-data.gpkg", layer = "ne_10m_populated_places", 
                  query="SELECT * 
                  FROM ne_10m_populated_places 
                  WHERE name IN ('Barcelona','Tarragona','Andorra','Perpignan','Castello','Zaragoza')
                  AND adm0_a3 IN('FRA','ESP','AND');")

# Read and transform municipal data from the Geopackage
atm_municipalities <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326)

municipalities_cat <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities_cat', stringsAsFactors = F) %>% 
  st_transform(4326)

# Process municipal borders
borders_atm <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>%
  st_transform(4326) %>%
  st_union()

# Simplify municipal borders
borders_atm_simplified <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>%
  st_transform(4326) %>%
  st_union() %>% 
  st_transform(25831) %>% 
  st_simplify(dTolerance = 1000) %>% 
  st_transform(4326) 

# Read and transform urban area data from the Geopackage
urban <- st_read(dsn = "data/gis-data.gpkg", layer = 'urban', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  filter(habitantes>100) # Filter by population size

# Define the region for roads data
roads_clip<- atm_municipalities %>% 
  filter(label %in% c('Tarragona','Reus','la Canonja','Vila-seca','Almoster')) %>% # Filter by municipalities
  st_union() %>% 
  st_bbox() %>% 
  st_as_sfc()
rclip<-roads_clip

# Read and process roads data from the Geopackage
roads <- st_read(dsn = "data/gis-data.gpkg", layer = 'rt_tramo_vial', stringsAsFactors = F,
                 query = paste("select clased,geom 
                 from rt_tramo_vial 
                 where clased NOT IN ('Camino','Senda') AND st_intersects(geom,st_geomfromtext('", st_as_text(roads_clip),"'))")) %>% 
  st_transform(4326) %>% # Transform the coordinate system to WGS 84
  st_intersection(rclip) # Find the common part of two layers

# Read and transform trains data from the Geopackage using a SQL query
trains <- st_read(dsn = "data/gis-data.gpkg", layer = 'trains', stringsAsFactors = F,
                  query = "select * from trains where st_intersects(geom,st_geomfromtext('POLYGON ((0.5585751 40.83526, 1.75326 40.83526, 1.75326 41.68285, 0.5585751 41.68285, 0.5585751 40.83526))'));") %>% 
  st_transform(4326)

# Read and transform river data from the Geopackage
francoli <- st_read(dsn = "data/gis-data.gpkg", layer = 'francoli', stringsAsFactors = F) %>%
  st_transform(4326)

# Read, transform and process neighbourhoods data from the Geopackage
tarragona_neighbourhoods <- st_read(dsn = "data/gis-data.gpkg", layer = 'tarragona_neighbourhoods_jom', stringsAsFactors = F) %>% 
  st_transform(4326)

reus_neighbourhoods <- st_read(dsn = "data/gis-data.gpkg", layer = 'reus_neighbourhoods_jom', stringsAsFactors = F) %>% 
  st_transform(4326)

# Calculate the centroid of neighbourhoods
tarragona_neighbourhoods_centroid <- st_centroid(tarragona_neighbourhoods)
reus_neighbourhoods_centroid <- st_centroid(reus_neighbourhoods)

#####################
# Transit stop data #
#####################

# Read stop data from the Geopackage
stops <- st_read(dsn = "data/gis-data.gpkg", layer = 'stops', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  filter(X_stop_code %in% c(stop_urban,stop_interurban))

# Split the stops into interurban and urban categories
stops_interurban <- filter(stops, X_stop_code %in% stop_interurban)
stops_urban <- filter(stops, X_stop_code %in% stop_urban)

# Split the urban stops by municipalities
stops_urban_tarragona <- filter(stops_urban, X_stop_name %in% stop_names_tarragona)
stops_urban_reus <- filter(stops_urban, X_stop_name %in% stop_names_reus)

#############################
# Transit validation counts #
#############################

# Group and summarize the validation data by stop and month
validations_per_stop_month <- group_by(validations, X_stop_code, month) %>% 
  summarise(validations = sum(validations))

# Join the summarized validation data back to the stop data
stops <- left_join(stops, validations_per_stop_month)

# Add a column for the month name
stops <- mutate(stops, month_name = month.abb[month])

