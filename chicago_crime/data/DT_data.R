# Data for DT

# Chicago Population Data taken from CMAP Data Hub, based on Census 2010
# https://datahub.cmap.illinois.gov/dataset/community-data-snapshots-raw-data/resource/8c4e096e-c90c-4bef-9cf1-9028d094296e

chicago_pops <- read_csv("data/chicago_comm_area_populations.csv",
                         col_types = cols(.default = col_character())) %>% 
  select(GEOG, `2010_POP`) %>% 
  mutate(pop_2010 = as.numeric(`2010_POP`)) %>% 
  mutate(community = tools::toTitleCase(tolower(GEOG))) %>% 
  select(community, pop_2010)

# Clean data from main crime dataset

chicago_dt <- chicago %>% 
  mutate(area_numbe = as.numeric(community_area)) %>% 
  filter(!is.na(area_numbe))
  
# List containing community area name, number, and side
  
list_comm_area <- community_areas %>% 
  mutate(community_area = as.numeric(area_numbe)) %>% 
  mutate(community = tools::toTitleCase(tolower(community))) %>% 
  group_by(community_area, community, side) %>% 
  count() %>% 
  select(community_area, community, side)


# Convert from sf to df object

st_geometry(list_comm_area) <- NULL

# Fix names that were different in datasets

list_comm_area[32, 2] <- "The Loop"
list_comm_area[76, 2] <- "O'hare"

# Create datatable to show which areas are the most dangerous 

dt <- inner_join(x = chicago_dt, y = list_comm_area, by = "community_area") %>% 
  group_by(community_area, community, side) %>% 
  summarize(crimes = n()) %>% 
  inner_join(chicago_pops, by = "community") %>% 
  mutate(crimes_per_capita = crimes / 10 / pop_2010) %>% 
  mutate(crimes_per_capita = round(crimes_per_capita, 4)) %>% 
  arrange(desc(crimes_per_capita))


