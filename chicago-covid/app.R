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
library(extrafont)


# Define UI for application that draws a histogram

ui <- navbarPage(
    theme = shinytheme("simplex"),
    "COVID-19 Prevalence in Chicago by Race",
    
    tabPanel("Graphical Display",
             h1("COVID-19 Disproportionately Affects Chicago's Minorities"),
             br(),
             h3("Select a racial population to graph its population density next to a map of COVID-19 cases in Chicago by zip code."),
             selectInput("var", 
                         label = "",
                         choices = c("Percent White", 
                                     "Percent Black",
                                     "Percent Hispanic",
                                     "Percent Non White"),
                         selected = "Percent White"),
             mainPanel(
                 plotOutput("race_map", width = "140%"),
                 br(),
                 br(),
                 br(),
                 br()
             ),
             
    ),
    
    tabPanel("Statistical Analysis",
             h1("Statistical Correlation: Racial Distribution and COVID-19 Cases"),
             br(),
             h2("Data Collection:"),
             h4("To conduct this analysis, I relied on two data sets. The first data set came from the Illinois Department of Public Health which contained the number of confirmed cases of COVID-19 in each zip code. The second data set came from the 2010 census which provided the racial breakdown of zip codes in Chicago. I joined the datasets to correlate racial distribution and COVID-19 cases."),
             br(),
             h2("Linear Regression:"),
             h4("The graph below shows that as the White population increases, the number of cases per 1,000 people decreases (the size of each dot corresponds to the population size of the zip code)."),
             plotOutput("linear_regression", width = "100%"),
             p("The correlation coefficient between the proportion of the White population and the number of positive cases per 1,000 people is -0.6032. This suggests a moderately strong negative linear relationship between the two variables. The slope of the regression shows that for every 1% increase in the White population, we expect on average that the number of cases per 1,000 people will decrease by 0.0612. In other words, every time the proportion of the White population increases by 15%, there is 1 fewer positive case per 1,000 people. The graph below provides a more straightforward conceptualization of this relationship by showing the average number of cases for each racial community."),
             br(),
             h2("Average Cases per Racial Group:"),
             h4("The graph below shows that zip codes where at least 50% of the population was White have far fewer cases on average compared to their non-White majority counterparts."),
             br(),
             plotOutput("average_cases"),
             p("Majority White zip codes had 5 cases per 1,000 people. Black majority zip codes had 9 cases per 1,000 people, and Hispanic majority zip codes had 10 cases per 1,000 people."),
             br(),
             br(),
             br(),
             br()
             ),
    
    tabPanel("Policy Recommendations",
             h1("Coordinated Policy Design: Health and Non-Health Sector Response"),
             br(),
             h2("Public Health Infrastructure"),
             h4("The lack of public health infrastructure makes it difficult to provide reliable service to these at-risk communities. To address gaps in public health infrastructure, there needs to be a reallocation of resources from both the health and non-health sector towards minority areas to ensure that appropriate care is provided."),
             br(),
             h2("Population Density"),
             h4("Minority communities may also have greater population density which is conducive to the spread of disease. The greater population density demands that the health sector be more vigilant in administering tests and the non-health sector enforce strict social distancing guidelines."),
             br(),
             h2("Financial Constraints"),
             h4("Many people living in minority areas are unable to afford taking time off work. So long as the government fails to ensure that employers provide paid time off, employees in these areas are unjustly forced to compromise their health (and the health of their communities). Leadership from the non-health sector must consider the financial constraints that force employees in these areas to continue working. Passing budget plans that accommodate for these financial barriers would create a more holistic and effective response to the health crisis."),
             br(),
             br(),
             br(),
             br()
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
    
    output$linear_regression <- renderPlot({
        regression()
    })
    

    output$average_cases <- renderPlot({
        average_cases()
    })
    
}

shinyApp(ui, server)


