# This script creates a map of the locations of reported crimes over time. 
# Perhaps I'll add the option to select if the person was arrested, and
# also specify the type of shooting.
library(fs)
library(janitor)
library(lubridate)
library(ggthemes)
library(knitr)
library(scales)
library(data.table)
library(shiny)
library(tidyverse)
library(sf)
library(tigris)

shapes <- pumas(class = "sf", state = "il") %>%
  filter(str_detect(NAMELSAD10, "Chicago"))


# ggplot(data = x) +
#   geom_sf(aes(fill = NAMELSAD10))

# shapes <- places(class = "sf", state = "il") %>% 
#   filter(NAME == "Chicago")

shooting_locations <- chicago %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sample_n(2000)

ggplot(data = shapes) +
  theme_map() +
  geom_sf(aes(fill = NAMELSAD10)) +
  geom_sf(data = shooting_locations) +
  theme(legend.position = "none")
