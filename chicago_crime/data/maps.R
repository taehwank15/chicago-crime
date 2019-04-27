# This script creates a map of the locations of reported crimes over time. 
# Perhaps I'll add the option to select if the person was arrested, and
# also specify the type of shooting.

library(sf)
library(tigris)

x <- tigris::pumas(class = "sf", state = "il") %>% 
  filter(str_detect(NAMELSAD10, "Chicago"))

ggplot(data = x) +
  geom_sf(aes(fill = NAMELSAD10))

shapes <- places(class = "sf", state = "il") %>% 
  filter(NAME == "Chicago")

valid_chicago_shootings <- chicago %>% 
  filter(!is.na(latitude))

shooting_locations <- st_as_sf(valid_chicago_shootings, coords = c("longitude", "latitude"), crs = 4326) %>% 
  sample_n(1000)

ggplot(data = shapes) +
  geom_sf(aes(fill = )) +
  geom_sf(data = shooting_locations)
