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
ui <- fluidPage(
   
   # Application title
   navbarPage("Crime in Chicago",
     tabPanel("Plot",
     # Sidebar with a slider input for number of bins 
       sidebarLayout(
          sidebarPanel(
             sliderInput("years",
                         "Year Range",
                         min = 2008,
                         max = 2018,
                         value = c(2008,2018),
                         sep = "")
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("yearGraph")
          )
        )
      ),
     tabPanel("Map",
              sidebarLayout(
                sidebarPanel(
                  checkboxInput("arrest",
                                "Show Only Arrested?",
                                value = FALSE)
                ),
              mainPanel(
                plotOutput("map")
              )
              )),
      navbarMenu("About:",
      tabPanel("Project",
               p("This project seeks to analyze patterns in reported cases of Chicago Crime over time.")),
      tabPanel("Data",
               p("The data for this project was downloaded form the Chicago Data Portal, available ", 
                 tags$a("here", href = "https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2")),
               p("This data extracts the decade from 2008 to 2018 and samples 900,000 data points out of the over 3.2 million.")),
      tabPanel("Author",
               p("I am a student at Harvard College studying Social Studies and Computer Science."),
               p("The repository for my project can be found ", 
                 tags$a("here", href = "https://github.com/taehwank15/chicago-crime"))
  ))))

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
    # shooting_locations %>% 
    #    if (input$arrest == TRUE){
    #    filter(arrest == TRUE)
    #    }
     
     ggplot() +
       geom_sf(data = shapes) +
       geom_sf(data = shooting_locations)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

