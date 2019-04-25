# This script is used to download data from the Internet and
# cut the data so that it only contains necessary data. The
# edited data is then written into a csv to use in my shiny
# app after zipping.


# Load in libraries

library(fs)
library(janitor)
library(data.table)
library(tidyverse)

# Read csv into environment and delete from folder

download.file("https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD",
              destfile = "chicago.csv",
              mode = "wb")

# Write edited version of csv with only relevant data

chicago_edit <- fread(file = "chicago.csv" , sep = ",")

chicago_edit <- chicago_edit %>% 
  clean_names() %>% 
  select(id, date, block, primary_type, description, location_description,
         arrest, domestic, beat, district, ward, community_area, 
         year, latitude, longitude) %>% 
  filter(year >= 2008) %>% 
  filter(year <= 2018)
  
write_csv(chicago_edit, "chicago_edit.csv") 

