# Unzip compressed chicago file that has been edited to only have necessary cols

# unzip("data/chicago_edit.csv.zip")

# setwd(dir = "chicago_crime/data/")

# Read edited version of csv
# chicago <- fread('unzip -p data/chicago_edit.csv.zip', sep = ",")

 chicago <- readr::read_rds(path = "data/chicago_edit.rds") %>% 
   filter(floor(longitude) != -92)

# chicago <- fread(file = "chicago_crime/data/chicago_edit.rds")
  
# file_delete("chicago_edit.csv")

# Change date from char to POSIXct

chicago$date <- as.POSIXct(chicago$date, format = "%m/%d/%Y %I:%M:%S %p") 

chicago %>% 
  group_by(floor(longitude)) %>% 
  count()

# sample

c1 <- chicago %>%
  sample_n(500)

# Calculate data for graph showing reported cases per year

# chicago %>%
#   select(year) %>%
#   
#   # Because 2019 is not finished, graph would be misleading if included
#   
#   filter(year < 2019) %>%
#   
#   # Calculate number of cases per year
# 
#   group_by(year) %>%
#   summarize(cases = n()) %>%
#   ungroup() %>%
#   
# 
# 
#   # Graph cases
# 
#   ggplot(aes(x = year, y = cases)) +
#     geom_line() +
#     theme_calc() +
#     labs(title = "Number of Reported Cases",
#     subtitle = "In Chicago, IL from 2001 to 2018",
#     caption = "Source: data.cityofchicago.org") +
#     xlab("Year") +
#     ylab("Number of Reported Cases") +
#     scale_y_continuous(labels = comma) +
#     scale_x_continuous(breaks = c(2001, 2005, 2010, 2015, 2018),
#     labels = c("2001", "2005","2010", "2015", "2018"))

c1 %>% 
ggplot(aes(x = fct_infreq(primary_type))) +
  geom_bar()

  