#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(fs)
library(janitor)
library(lubridate)
library(ggthemes)
library(knitr)
library(scales)
library(data.table)
library(shiny)
library(tidyverse)

source("data/date_graph.R")
source("data/maps.R")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  navbarPage(
    "Crime in Chicago",
    tabPanel("Plot",
             # Sidebar with a slider input for number of bins
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
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 h3("A Simplified Map"),
                 p(
                   "Because of how long it takes to load images, I sampled only two thousand of the hundreds and thousands of reports."
                 ),
                 checkboxGroupInput(
                   "arrest",
                   "Arrested?",
                   selected = c(TRUE, FALSE),
                   choices = c("No" = FALSE,
                               "Yes" = TRUE)
                 )
               ),
               mainPanel(plotOutput("map"))
             )),
    tabPanel("Crime Types",
             sidebarLayout(
               sidebarPanel(
                 h3("Counting Crimes by Type"),
                 p(
                   "In this section, we are able to see the differences in number of crime reports by crime type per year"
                 ),
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
               mainPanel(plotOutput("typeChart"),
                         p("It seems that for all of the years, theft is the most reported crime, while narcotics is the largest reason for arrests."))
             )),
    navbarMenu(
      "About:",
      tabPanel(
        "Project",
        h3("Goal"),
        p(
          "This project seeks to analyze patterns in reported cases of Chicago Crime over time."
        ),
        h3("Overall Findings"),
        p("It seems that ther has been a constant decrease in the number of reported cases of crime."),
        p("Another interesting thing to note is that much of arrests come from the West and Southern sides of Chicago. This is consistent with commonly-held beliefs in Chicago that the Western and Southern neighborhoods are often comparatively more dangerous")
      ),
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
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$yearGraph <- renderPlot({
    # generate bins based on input$bins from ui.R
    year_range <- chicago %>%
      filter(year >= input$years[1]) %>%
      filter(year <= input$years[2]) %>%
      group_by(year) %>%
      summarize(cases = n()) %>%
      ungroup()
    
    # draw the histogram with the specified number of bins
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
  
  output$map <- renderPlot({
    filtered_locations <- shooting_locations %>%
      filter(arrest %in% input$arrest)
    
    ggplot(data = shapes) +
      theme_map() +
      geom_sf(aes(fill = NAMELSAD10)) +
      geom_sf(data = filtered_locations) +
      theme(legend.position = "none")
    
  })
  
  output$typeChart <- renderPlot({
    type_year <- chicago %>%
      filter(year == input$year) %>%
      filter(arrest %in% input$numArrest)
    # draw the bar graph with count of crime type
    
    type_year %>%
      ggplot(aes(x = fct_rev(fct_infreq(primary_type)))) +
      geom_bar() +
      theme_calc() +
      labs(title = "Number of Reported Cases",
           subtitle = "In Chicago, IL",
           caption = "Source: data.cityofchicago.org") +
      xlab("Type of Crime") +
      ylab("Number of Reported Cases") +
      scale_y_continuous(labels = comma) +
      coord_flip()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
