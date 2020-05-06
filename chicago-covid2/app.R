#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinythemes)
library(extrafont)
library(ggplot2)
library(gridExtra)


# read in the data from the .rmd

chicago <- readRDS("chicago.RDS")
chicago_analysis <- readRDS("chicago_analysis.RDS")

# create functions to plot graphs:

average_cases <- function(){
    table <- chicago_analysis %>% 
        group_by(majority) %>% 
        summarize(average_cases = round(mean(cases_per_1000), 0)) 
    
    table$majority <- c("Black", "Hispanic", "Other", "White")
    
    ggplot(table, aes(x = majority, y = average_cases)) +
        geom_col(fill = "lightblue") +
        theme_classic() + 
        labs(title = "Average Confirmed Cases of COVID-19 in Chicago by Racial Group",
             x = "Majority Racial Group",
             y = "Average Confirmed Cases of COVID-19") + 
        theme(text=element_text(family="Tahoma"))
    
    
}

regression <- function(){
    ggplot(data = chicago_analysis,
           aes(x = percent_white,
               y = cases_per_1000,
               size = summary_est,
               color = majority)) +
        geom_point() +
        geom_smooth(inherit.aes = FALSE,
                    aes(x = percent_white,
                        y = cases_per_1000),
                    method = "lm",
                    se = FALSE, color = '#dc143c',
                    linetype = "dashed") +
        theme_classic() +
        labs(title = "Confirmed Cases of COVID-19 per 1,000 People in Chicago by Racial Group",
             x = "Percent White",
             y = "Confirmed Cases of COVID-19 per 1,000 People",
             color = "Racial Group") +
        scale_color_manual(values=c("#228B22", "#E69F00", "#999999", "#56B4E9")) +
        guides(size = FALSE,
               colour = guide_legend(override.aes = list(size=5))) +
        theme(text=element_text(family="Tahoma"))
}

plot_race <- function(data, fill, subtitle, legend){
    
    ggplot(chicago) +
        geom_sf(aes(fill = data, color = data))+
        labs(title = "Racial Distribution of Chicago by Zipcode",
             subtitle = subtitle,
             fill = legend) +
        theme_void() +
        scale_fill_gradient(low = "white", 
                            high = fill,
                            breaks = c(0, 20, 40, 60, 80, 100)) +
        scale_color_gradient(low = "white", high = fill) +
        theme(text = element_text(family = "Tahoma"),
              legend.position = c(1., 0.75),
              legend.key.size = unit(0.4, "cm"),
              legend.title = element_text(size =10),
              legend.text = element_text(size = 8)) +
        guides(color = FALSE)
    
}

plot_corona <- function(){
    ggplot(chicago) +
        geom_sf(aes(fill= cases_per_1000, color = cases_per_1000)) +
        labs(title = "Confirmed Cases of COVID-19 in Chicago by Zipcode",
             fill = "Cases Per 1000") +
        theme_void() +
        scale_fill_gradient(low = "white", 
                            high = "brown") +
        scale_color_gradient(low = "white", high = "brown") +
        theme(text= element_text(family = "Tahoma"),
              legend.position = c(1.08, 0.72),
              legend.key.size = unit(0.4, "cm"),
              legend.title = element_text(size =10),
              legend.text = element_text(size = 8)) +
        guides(color = FALSE)
}


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
    
    output$race_map <- renderPlot({
        
        # data <- switch(input$var,
        #                "Percent White" = chicago$percent_white,
        #                "Percent Black" = chicago$percent_black,
        #                "Percent Hispanic" = chicago$percent_hispanic,
        #                "Percent Non White" = chicago$percent_non_white)
        # 
        # fill <- switch(input$var, 
        #                "Percent White" = "darkgreen",
        #                "Percent Black" = "black",
        #                "Percent Hispanic" = "darkorange",
        #                "Percent Non White" = "darkviolet")
        # 
        # legend <- switch(input$var,
        #                  "Percent White" = "% White",
        #                  "Percent Black" = "% Black",
        #                  "Percent Hispanic" = "% Hispanic",
        #                  "Percent Non White" = "% Non White")
        # 
        # subtitle <- switch(input$var,
        #                    "Percent White" = "White Population",
        #                    "Percent Black" = "Black Population",
        #                    "Percent Hispanic" = "Hispanic Population",
        #                    "Percent Non White" = "Non White Population")
        # 
        # 
        # race_graph <- plot_race(data,
        #                         fill,
        #                         legend,
        #                         subtitle)
        # 
        # corona_graph <- plot_corona()
        
        # grid.arrange(race_graph,corona_graph, ncol=2)
        
        # is it an issue working with the "chicago" data or is it an issue with geom_sf?
        
        ggplot(chicago_analysis, aes(x = percent_white)) +
            geom_bar()
        
        
    })
    
    output$linear_regression <- renderPlot({
        regression()
    })
    

    output$average_cases <- renderPlot({
        average_cases()
    })
    
}

shinyApp(ui, server)


