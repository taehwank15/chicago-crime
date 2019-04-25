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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Reported Crime in Chicago Over Time"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("years",
                     "Year Range",
                     min = 2008,
                     max = 2018,
                     value = c(2008,2018))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("yearGraph")
      )
   )
)

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
}

# Run the application 
shinyApp(ui = ui, server = server)

