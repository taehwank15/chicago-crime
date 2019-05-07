#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# TO DO
# Reactive/lazy
# More analysis into days -- see why there's a bump on 1st day of month (April Fools?)
# Text to explain findings, guide people; different texts per output image possibly
# Upload image of pop. density map?
# Fluid page
# Facet by crime type?
# Text label of map community areas 
# Visualization to show most dangerous areas

# Load in libraries

library(fs)
library(sf)
library(janitor)
library(ggthemes)
library(stringr)
library(knitr)
library(tigris)
library(scales)
library(data.table)
library(lubridate)
library(shiny)
library(tidyverse)

# Run scripts that read in data into environment

source("data/date_graph.R")
source("data/maps.R")

# Define UI for application

ui <- fluidPage(
  
  # Application title
  
  navbarPage(
    "Crime in Chicago",
    
    # Tab for Basic plot per year
    
    tabPanel("Plot",
             
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
               
               mainPanel(plotOutput("yearGraph"))
             )),
    
    # Tab for cases per divisions of time
    
    tabPanel("Time",
             
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
               
               mainPanel(plotOutput("timeGraph"))
             )),
    
    # Tab for basic map of locations of crimes
    
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 
                 # Description of sampling of data
                 
                 h3("A Simplified Map"),
                 p(
                   "Because of how long it takes to load images, I sampled only two thousand of the hundreds and thousands of reports."
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
               
               mainPanel(plotOutput("map"))
             )),
    
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
                   while narcotics is the largest reason for arrests."
                 )
               )
             )),
    
    # Decided to use a use a navbarMenu because these are all similar
    
    navbarMenu(
      "About:",
      
      # Description of project and outcomes
      
      tabPanel(
        "Project",
        h3("Goal"),
        p(
          "This project seeks to analyze patterns in reported cases of Chicago Crime over time."
        ),
        h3("Overall Findings"),
        p(
          "It seems that ther has been a constant decrease in the number of reported cases of crime."
        ),
        p(
          "Another interesting thing to note is that much of arrests come from the West and Southern sides of Chicago.
          This is consistent with commonly-held beliefs in Chicago that the Western and Southern neighborhoods are
          often comparatively more dangerous than the Northern area."
        )
        ),
      
      # Source of data and how I sampled
      
      tabPanel(
        "Data",
        p(
          "The data for this project was downloaded form the Chicago Data Portal, available ",
          tags$a("here", href = "https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2")
        ),
        p(
          "This data extracts the decade from 2008 to 2018 and samples 900,000 data points out of the over 3.2 million."
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
        )
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
  
  # Generate distribution of crimes per division of time
  
  output$timeGraph <- renderPlot({
    filtered_range <- chicago %>%
      
      # Create new columns of time divisions to choose during plot-making
      
      mutate(month = month(date)) %>% 
      mutate(day = day(date)) %>% 
      mutate(hour = hour(date)) %>% 
      
      # Filter by year selected
      
      filter(arrest %in% input$timeArrest)
    
    filtered_range %>% 
      
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
  
  # Generates map showing distribution of crime 
  
  output$map <- renderPlot({
  
    # Checks whether report led to an arrest
      
    filtered_locations <- shooting_locations %>%
      filter(arrest %in% input$arrest)
    
    # Graphs locations
    
    p +
      geom_sf(data = filtered_locations, aes(alpha = 0.45)) +
      
      # Disable Legend for alpha value
      scale_alpha(guide = "none") +
      
      labs(title = "Locations of Crimes in Chicago",
           subtitle = "2008 - 2018")
    
  })
  
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
