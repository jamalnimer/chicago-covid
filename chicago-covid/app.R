#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(cowplot)

# Define UI for application that draws a histogram

ui <- navbarPage(
    
    theme = shinytheme("simplex"),
    "COVID-19 Prevalence in Chicago by Race",
    
    tabPanel("Geographic Distribution",
             h3("COVID-19 Prevalence in Chicago by Race"),
             selectInput("var", 
                         label = "Choose a variable to display",
                         choices = c("Percent White", 
                                     "Percent Black",
                                     "Percent Hispanic",
                                     "Percent Non White"),
                         selected = "Percent White"),
             mainPanel(
                 plotOutput("race_map", width = "140%"))
    )
    
    
)

server <- function(input, output) {
    
    output$corona_map <- renderPlot({
        plot_corona()
    })
    
    output$race_map <- renderPlot({
        
        data <- switch(input$var,
                       "Percent White" = chicago$percent_white,
                       "Percent Black" = chicago$percent_black,
                       "Percent Hispanic" = chicago$percent_hispanic,
                       "Percent Non White" = chicago$percent_non_white)
        
        fill <- switch(input$var, 
                       "Percent White" = "darkgreen",
                       "Percent Black" = "black",
                       "Percent Hispanic" = "darkorange",
                       "Percent Non White" = "darkviolet")
        
        legend <- switch(input$var,
                         "Percent White" = "% White",
                         "Percent Black" = "% Black",
                         "Percent Hispanic" = "% Hispanic",
                         "Percent Non White" = "% Non White")
        
        subtitle <- switch(input$var,
                           "Percent White" = "White Population",
                           "Percent Black" = "Black Population",
                           "Percent Hispanic" = "Hispanic Population",
                           "Percent Non White" = "Non White Population")
        
        
        race_graph <- plot_race(data,
                                fill,
                                legend,
                                 subtitle)
        
        corona_graph <- plot_corona()
        
        plot_grid(race_graph, corona_graph)
        
        
    })
    
}

shinyApp(ui, server)


