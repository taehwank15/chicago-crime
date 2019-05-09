#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load in libraries

library(fs)
library(sf)
library(janitor)
library(ggthemes)
library(stringr)
library(knitr)
library(scales)
library(data.table)
library(lubridate)
library(shinythemes)
library(shiny)
library(DT)
library(tidyverse)

# Run scripts that read in data into environment

source("data/date_graph.R")
source("data/maps.R")
source("data/DT_data.R")

# Define UI for application

ui <- fluidPage(
  
  # Application title
  
  navbarPage(
    "Crime in Chicago",
    theme = shinytheme("flatly"),
    
    
    # Decided to use a use a navbarMenu because these are all similar
    
    navbarMenu(
      "About:",
      
      # Description of project and outcomes
      
      tabPanel(
        "Project",
        titlePanel("Analyzing Crime in Chicago, 2008-2018"),
        h3("Goal"),
        p(
          "Often called ", tags$i("Chiraq, "), "Chicago is often known for its problems in crime. This project 
          seeks to analyze recent patterns of crime in Chicago. Using visualizatoins and models, I hope to answer 
          questions such as ", tags$i("In which areas is crime most common?"), ", ", tags$i("When is crime most fervent?"),
          ", and ", tags$i("What types of crime are common?")
        ),
        h3("Overall Findings"),
        p(
          "Fortunately, there has been a steady and significant decrease in crime in Chicago, based on the number of cases
          of crime. Looking into Crime by Time in Chicago, it seems to be most dangerous during the first nights of summer months.
          Interestingly, much of the arrests and crime reports came from the Western and Southern sides of Chicago.
          This is consistent with commonly-held beliefs in Chicago that these sides are comparatively more dangerous than Northern
          areas."
        ),
        h3("Data"),
        p(
          "The crime data for this project was downloaded form the Chicago Data Portal, available ",
          tags$a("here.", href = "https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2"),
          "The shapefile for the Chicago map was always exported from this portal."
        ),
        p(
          "Furthermore, this data extracts the decade from 2008 to 2018 and samples 900,000 data points out of 
          the over 3.2 million cases from this decade."
        ),
        p(
          "The population data has been taken from the CMAP (Chicago Metropolitan Agency for Planning) Data Hub and
          can be found ", tags$a("here.", 
                                 href = "https://datahub.cmap.illinois.gov/dataset/community-data-snapshots-raw-data/resource/8c4e096e-c90c-4bef-9cf1-9028d094296e")
        )
      ),
      
      # Bit of information about me *WILL UPDATED*
      
      tabPanel(
        "Author",
        p(
          "I am a student at Harvard College studying Social Studies and Computer Science."
        ),
        p(
          "The repository for my project can be found ",
          tags$a("here", href = "https://github.com/taehwank15/chicago-crime")
        )
      )
    ),
    # Tab for Basic plot per year
    
    tabPanel("Yearly Trends",
             
             # Sidebar with slider for year range
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "years",
                   "Year Range",
                   min = 2008,
                   max = 2018,
                   value = c(2008, 2018),
                   sep = ""
                 )
               ),
               
               # Show a plot of the generated distribution
               
               mainPanel(plotOutput("yearGraph"),
                         p("Based on pure count, crime seems to be on a rapid decline in Chicago. Though there have been
                           some spikes in 2015, the general trend of the past decade has been a decrease."))
             )),
    
    # Tab for cases per divisions of time
    
    tabPanel("By Time",
             
             # Sidebar with options to check for arrested and choosing division
             
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "timeArrest",
                   "Arrested?",
                   selected = c(TRUE, FALSE),
                   choices = c("No" = FALSE,
                               "Yes" = TRUE)
                 ),
                 selectInput(
                   "time_frame",
                   "See crime by divisions of time: ",
                   choices = c(
                     "Month" = "month",
                     "Day" = "day",
                     "Hour" = "hour"
                   ),
                   selected = "month"
                 )
               ),
               
               # Show a plot of the generated distribution
               
               mainPanel(plotOutput("timeGraph"), 
                         br(),
                         
                         # Conditional panels to offer explanation based on which time division was selected.
                         # I used length because the quotes made it difficult to process strings.
                         
                         conditionalPanel("input.time_frame.length == 5",
                                          p("It seems that, in each year, there is an increase of cases of crime 
                                            during the summer months compared to the rest of the months. This can be
                                            potentially explained by ",
                                            tags$a("certain psychology reports ",
                                                   href = "https://www.nature.com/articles/s41598-017-06720-z"),
                                            "that show that increased temperatures lead to more aggression. It is also possible
                                            that--during summer months--schools are on break, so students have more free
                                            time, which could incite more crime. Additionally, the drop in February may
                                            seem significant, but it is important to note that February has less days than all other
                                            months.")),
                         conditionalPanel("input.time_frame.length == 3",
                                          p("There is a significant decrease on the 31st. However, this is expected, as not all months have
                                            31 days. Additionally, it seems that there is a significant spike of crime on the first of every month.
                                            However, when only considering arrests (done by unchecking No in Arrested?), the spike 
                                            seems to largely go away. Yet there needs to be further analysis into this strange occurence.
                                            Below, there is a visualization showing cases per day by every month as well as an explanation.")),
                         conditionalPanel("input.time_frame.length == 4",
                                          p("There is a significant decrease in cases during the early morning.
                                            Perhaps this can be explained, as most people are asleep, away from criminal
                                            hotspots, during this time. Furthermore, as nighttime approaches, there seems to
                                            be more crime. Thus late hours seem to be correlated with more crime.")))),
             br(),
             
             # Only produce faceted graph when Day has been selected.
             # Outside of mainPanel as it should not be affected by the 
             # side panel.
             
             conditionalPanel("input.time_frame.length == 3",
                              plotOutput("conditional_day", width = "100%", height = "750px"),
                              br(),
                              p("Here we can see more of why there is such a significant increase on the first of every month.
                                The data is largely skewed by the immense spike on New Years Day. If we ignore January, the
                                intial spike is somewhat lessened, but a noticeable difference from the rest of the days still remain.
                                This graph also provides insight into anomalies. We see in the December graph, there is a 
                                significant decrease on Christmas Day. Perhaps people are more likely to be at home, rather than
                                participate in criminal activities in hotspots. There is also a noticeable difference during
                                Halloween, as crimes spike on the last day of October.")
             )),
    
    # Tab for basic map of locations of crimes
    
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 
                 # Description of sampling of data
                 
                 h3("A Simplified Map"),
                 p(
                   "Because of how long it takes to load images, I sampled only seven thousand of the hundreds and thousands of reports."
                 ),
                 
                 # Check arrested
                 
                 checkboxGroupInput(
                   "arrest",
                   "Arrested?",
                   selected = c(TRUE, FALSE),
                   choices = c("No" = FALSE,
                               "Yes" = TRUE)
                 )
               ),
               
               # Shows output for map
               
               mainPanel(plotOutput("map", height = "700px", width = "700px"),
                         p("The map has been divided into Chicago's 77 community areas. More analysis into the community areas can be seen
                           in the next tab. There does seem to be clusters on the Central, Western, Southern sides of Chicago, compared to the rest. 
                           Interestingly, this is in line with many generalizations about the West Side and South Sides of Chicago."))
             )),
    tabPanel("Community Areas",
             br(),
             titlePanel("Crime Rates in Community Areas in Chicago"),
             DTOutput("comm_area"),
             h3("Note:"), 
             p("It is crucial to remember that the data for this project samples actual data. There are actually more
               than 3.2 million reported cases of crime in this decade, yet this project samples 900,000. Therefore, the
               per capita crime and number of crimes are not truly reflective of the real situation. ", tags$strong("However"),
               ", the proportions should still be similar as it was a sample. Hence the", tags$i("order"), 
               " based on per capita crime should be accurate."),
             p("Interestingly, here it can be seen that the Southern and Western regions of Chicago are relatively more dangerous
               compared to other regions. On the opposite end, the community areas with the lowest crime rate per capita are
               in the Northern regions of Chicago. There seems to be a clear separation in location of crimes in Chicago.")),
    
    # Tab for number of reports per type of crime
    
    tabPanel("Crime Types",
             sidebarLayout(
               sidebarPanel(
                 
                 # Description of tab
                 
                 h3("Counting Crimes by Type"),
                 p(
                   "In this section, we are able to see the differences in number of crime reports by crime type per year"
                 ),
                 
                 # Choose year
                 
                 sliderInput(
                   "year",
                   "Year",
                   min = 2008,
                   max = 2018,
                   value = 2018,
                   sep = ""
                 ),
                 checkboxGroupInput(
                   "numArrest",
                   "Arrested?",
                   selected = c(TRUE, FALSE),
                   choices = c("No" = FALSE,
                               "Yes" = TRUE)
                 )
               ),
               mainPanel(
                 
                 # Plot showing distribution and explanation
                 
                 plotOutput("typeChart"),
                 p(
                   "It seems that for all of the years, theft is the most reported crime, 
                   while narcotics is the largest reason for arrests. Interestingly, when only graphing
                   non-arrests, it can be seen that there are barely any non-arrests for Narcotic-related crimes.
                   Using this, we can see that Chicago views narcotics to be a serious problem and is cracking down on
                   it. "
                 )
               )
             ))
  )
)

# Define server logic required to draw plots

server <- function(input, output) {
  output$yearGraph <- renderPlot({
    
    # generate distribution of crimes per year based on ui input
    
    year_range <- chicago %>%
      filter(year >= input$years[1]) %>%
      filter(year <= input$years[2]) %>%
      group_by(year) %>%
      summarize(cases = n()) %>%
      ungroup()
    
    # Draw a line graph showing change over years
    
    year_range %>%
      ggplot(aes(x = year, y = cases)) +
      geom_line() +
      theme_calc() +
      labs(title = "Number of Reported Cases",
           subtitle = "In Chicago, IL",
           caption = "Source: data.cityofchicago.org") +
      xlab("Year") +
      ylab("Number of Reported Cases") +
      scale_y_continuous(labels = comma)
  })
  
  # Reactive calculation of reports per time divisions
  filtered_range <- reactive({
    chicago %>%
      
      # Create new columns of time divisions to choose during plot-making
      
      mutate(month = month(date)) %>% 
      mutate(day = day(date)) %>% 
      mutate(hour = hour(date)) %>% 
      
      # Filter by year selected
      
      filter(arrest %in% input$timeArrest)
  })
  
  # Generate distribution of crimes per division of time
  
  output$timeGraph <- renderPlot({
    
    filtered_range() %>% 
      
      # aes_string allows me to use the input whose value is a string
      
      ggplot(aes_string(x = input$time_frame)) +
      geom_bar() +
      theme_calc() +
      
      # Based on what was selected, labels are changed
      
      labs(title = str_c("Reported Cases by ", str_to_title(input$time_frame)),
           subtitle = "in Chicago, IL",
           caption = "Source: data.cityofchicago.org") +
      xlab(str_to_title(input$time_frame)) +
      ylab("Number of Reported Cases") +
      scale_y_continuous(labels = comma)
  })
  
  output$conditional_day <- renderPlot({
    filtered_range() %>% 
      
      # aes_string allows me to use the input whose value is a string
      
      ggplot(aes_string(x = input$time_frame)) +
      geom_bar() +
      theme_calc() +
      
      # Label Graph 
      
      labs(title = str_c("Reported Cases by Day Every Month"),
           subtitle = "in Chicago, IL - Labeled by Month Number",
           caption = "Source: data.cityofchicago.org") +
      xlab("Day") +
      ylab("Number of Reported Cases") +
      scale_y_continuous(labels = comma) +
      
      # Show by month
      
      facet_wrap(~month) 
  })
  
  # Reactive of map to speed up computation
  
  makeMap <- reactive({
    
    # Checks whether report led to an arrest
    
    shooting_locations %>%
      filter(arrest %in% input$arrest)
  })
  
  
  
  # Generates map showing distribution of crime 
  
  output$map <- renderPlot({
    
    # Graphs locations using data calculated from Reactive
    
    p +
      geom_sf(data = makeMap(), aes(alpha = 0.45)) +
      
      # Disable Legend for alpha valueru
      scale_alpha(guide = "none") +
      
      labs(title = "Locations of Crimes in Chicago",
           subtitle = "with Divisions by Community Area, 2008 - 2018")
    
  })
  
  output$comm_area <- renderDT(dt, 
                               options = list(
                                 pageLength = 10
                               ),
                               colnames = c("Community Num.", 
                                            "Community Area", 
                                            "Side",
                                            "Total Crime Cases (Sampled)",
                                            "Population in 2010", 
                                            "Proportionate Crime Cases per Capita"),
                               selection = "none"
  )
  
  
  # Calculations to output graphs on Crime Type
  
  output$typeChart <- renderPlot({
    
    # Check for year and if report led to arrest
    
    type_year <- chicago %>%
      filter(year == input$year) %>%
      filter(arrest %in% input$numArrest)
    
    # Draw the bar graph with count of crime type
    
    type_year %>%
      
      # Orders each generated graph by frequency 
      
      ggplot(aes(x = fct_rev(fct_infreq(primary_type)))) +
      geom_bar() +
      theme_calc() +
      labs(title = "Number of Reported Cases",
           subtitle = "In Chicago, IL",
           caption = "Source: data.cityofchicago.org") +
      xlab("Type of Crime") +
      ylab("Number of Reported Cases") +
      scale_y_continuous(labels = comma) +
      
      # Flips x-y for easier viewing and reading of labels
      
      coord_flip()
  })
}

# Run the application

shinyApp(ui = ui, server = server)
