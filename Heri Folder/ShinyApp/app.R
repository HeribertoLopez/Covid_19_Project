#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application t
ui <- fluidPage(
    selectInput(inputId = "location",
                label = "Choose a Country",
                choices = unique(asia.covid.df$location)), # list of non-duplicated countries 
    plotOutput("line")
) 

server <- function(input, output) { 
    output$line <- renderPlot({
        ggplot(asia.covid.df %>%  
                   filter(location == input$location)) + 
            geom_line(mapping = aes(x = date, y = total_cases, color = "Total Cases")) +
            geom_line(mapping = aes(x = date, y = population, color = "Population")) +
            geom_line(mapping = aes(x = date, y = people_vaccinated, color = "Total Vaccinations")) +
            geom_line(mapping = aes(x = date, y = total_deaths, color = "Total Deaths"))  
    }
    ) 
}
# Run the application 
shinyApp(ui = ui, server = server) 


