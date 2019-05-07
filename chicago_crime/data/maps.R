# This script creates a map of the locations of reported crimes over time. 

# Sides of Chicago based on Community Area Number -- Will be Used for Legend

far_north = as.character(c(1:4, 9:14, 76, 77))
north = as.character(c(5:7, 21, 22))
central = as.character(c(8, 32, 33))
northwest = as.character(c(15:20))
west = as.character(c(23:31))
south = as.character(c(34:43, 60, 69))
southwest = as.character(c(56:59, 61:68))
far_southeast = as.character(c(44:55))
far_southwest = as.character(c(70:75))

# Shapefile with Community Area divisions -- Taken from ChicagoDataPortal website

community_areas <- read_sf(dsn = "data/boundaries/geo_export_95b93e3f-b597-421e-b4c4-c4126c9335a6.shp") %>% 
  mutate(side = fct_collapse(area_numbe,
                             `Far North` = far_north,
                             `Northwest` = northwest,
                             `North` = north,
                             `West` = west,
                             `Central` = central,
                             `Southwest` = southwest,
                             `South` = south,
                             `Far Southwest` = far_southwest,
                             `Far Southeast` = far_southeast))

# Ggplot of map with colored community area divisions


p <- ggplot(data = community_areas) +
  theme_map() +
  geom_sf(aes(fill = side)) +
  theme(legend.position = "right") +
  
  # Color palette slightly mimics Chicago flag colors
  
  scale_fill_brewer(palette = "RdYlBu") +
  labs(fill = "Side of Chicago")

# Data for locations of shootings

shooting_locations <- chicago %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sample_n(2000)


