# This script reads in the foundational data for Chicago crime.

chicago <- readr::read_rds(path = "data/chicago_edit.rds") %>% 
  
  # Removes outliers in map location

  filter(floor(longitude) != -92) %>% 
  filter(!is.na(date))

# Change date from char to POSIXct

chicago$date <- as.POSIXct(chicago$date, format = "%m/%d/%Y %I:%M:%S %p") 


# sample for easy use

c1 <- chicago %>%
  sample_n(500)