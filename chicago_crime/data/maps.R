# This script creates a map of the locations of reported crimes over time. 
# Perhaps I'll add the option to select if the person was arrested, and
# also specify the type of shooting.

shapes <- pumas(class = "sf", state = "il") %>%
  filter(str_detect(NAMELSAD10, "Chicago"))

shooting_locations <- chicago %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sample_n(2000)
