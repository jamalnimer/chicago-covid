#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# loads in the necessary libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(extrafont)
library(ggplot2)

# create functions to plot graphs:

average_cases <- function(){
    
    # I took in the data on cases and racial distribution and then grouped by the majority
    # for each zip code.
    
    table <- chicago_analysis %>% 
        group_by(majority) %>% 
        
        # I took the average number of cases for each racial group
        summarize(average_cases = round(mean(cases_per_1000), 0)) 
    
    # I changed the name of the majority communities so I could display it in
    # the graph more cleanly
    
    table$majority <- c("Black", "Hispanic", "Other", "White")
    
    # Creates a column that displays the average number of cases per 1000 people
    # for each racial group on the x axis.
    
    ggplot(table, aes(x = majority, y = average_cases)) +
        geom_col(fill = "lightblue") +
        theme_classic() + 
        labs(title = "Average Confirmed Cases of COVID-19 in Chicago per 1,000 People by Racial Group",
             x = "Majority Racial Group",
             y = "Average Confirmed Cases of COVID-19 per 1,000 People") + 
        theme(text=element_text(family="Tahoma"))
}

regression <- function(){
    
    # creates a point on the graph for each zip code. I put the percent_white of the
    # zip code on the x axis and the number of cases on the y-axis. I use the color
    # and size aesthetics to provide more information even though it isn't used by 
    # the regression. The size shows the number of people in each zip code and the
    # color shows the majority racial group for each zip code.
    
    ggplot(data = chicago_analysis,
           aes(x = percent_white,
               y = cases_per_1000,
               size = summary_est,
               color = majority)) +
        geom_point() +
        
        # Uses a linear model to perform the regression between the percent of the 
        # white population for each zip code and the number of cases per 1000 people.
        # I remove the standard error because it would only complicate the appearance
        
        geom_smooth(inherit.aes = FALSE,
                    aes(x = percent_white,
                        y = cases_per_1000),
                    method = "lm",
                    color = '#dc143c',
                    linetype = "dashed",
                    se = FALSE) +
        
        # cleans the appearance of the graph and labels all necessary components
        
        theme_classic() +
        labs(title = "Confirmed Cases of COVID-19 per 1,000 People in Chicago by Racial Group",
             x = "Percent White",
             y = "Confirmed Cases of COVID-19 per 1,000 People",
             color = "Racial Group") +
        
        # provides manual colors to each racial group
        
        scale_color_manual(values=c("#262626", "#FEBD68", "#999999", "#6C985D")) +
        
        # modifies the size of the dots for the racial group majority so that the
        # dots look more similar to their size on the graph
        
        guides(size = FALSE,
               colour = guide_legend(override.aes = list(size=5))) +
        theme(text=element_text(family="Tahoma"))
}

# read in the data from the .rmd

chicago_analysis <- readRDS("chicago_analysis.RDS")

# Define UI for application that draws a histogram

ui <- navbarPage(
    theme = shinytheme("simplex"),
    "COVID-19 Prevalence in Chicago by Race",
    
    # Presents the maps of Corona and racial distribution
    
    tabPanel("Graphical Display",
             h1("COVID-19 Disproportionately Affects Chicago's Minorities"),
             br(),
             h3("Select a racial population to graph its population density next to a map of COVID-19 cases in Chicago by zip code."),
             
             # allows the user to select the racial group distribution they are interested in
             
             selectInput("var", 
                         label = "",
                         choices = c("Percent White", 
                                     "Percent Black",
                                     "Percent Hispanic",
                                     "Percent Non White"),
                         selected = "Percent White"),
             
             # places the graphs side by side
             
             fluidRow(
                 column(6, imageOutput("race_map")),
                 column(6, imageOutput("corona_map"))
             ),
                 br(),
                 br(),
                 br(),
             
    ),
    
    tabPanel("Statistical Analysis",
             h1("Statistical Correlation: Racial Distribution and COVID-19 Cases"),
             br(),
             h2("Data Collection:"),
             h4("To conduct this analysis, I relied on two data sets. The first data set came from the Illinois Department of Public Health which contained the number of confirmed cases of COVID-19 in each zip code. The second data set came from the 2010 census which provided the racial breakdown of zip codes in Chicago. I joined the datasets to correlate racial distribution and COVID-19 cases."),
             br(),
             
             # provides a graph of the linear regression and an interpretation of its results
             
             h2("Linear Regression:"),
             h4("The graph below shows that as the White population increases, the number of cases per 1,000 people decreases (the size of each dot corresponds to the population size of the zip code)."),
             plotOutput("linear_regression", width = "100%"),
             p("The correlation coefficient between the proportion of the White population and the number of positive cases per 1,000 people is -0.6032. This suggests a moderately strong negative linear relationship between the two variables. The slope of the regression shows that for every 1% increase in the White population, we expect on average that the number of cases per 1,000 people will decrease by 0.0612. In other words, every time the proportion of the White population increases by 15%, there is 1 fewer positive case per 1,000 people. The graph below provides a more straightforward conceptualization of this relationship by showing the average number of cases for each racial community."),
             br(),
             
             # provides a graph of the average cases for each racial group as well as basic analysis
             
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
             
             # Provides 3 relevant policy insights
             
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
    
    # produces the map for the user's selected racial demographic
    
    output$race_map <- renderImage({
        
        # change input to the filename path.
        
        data <- switch(input$var,
                       "Percent White" = "white.png",
                       "Percent Black" = "black.png",
                       "Percent Hispanic" = "hispanic.png",
                       "Percent Non White" = "non_white.png")
        
        # set the race_filename to the value passed from the user
        
        race_filename <- normalizePath(file.path(data))
        
        # pulls the image and sets its width to 100% so it takes up the entire space 
        # of the columns assigned in the UI.
        # I do not provide a height because I want the height to change according to 
        # the user's screen
        
        race_image <- list(src = race_filename, width = "100%")
        
        race_image

        # I don't delete the file because I want to continue using it. The image is 
        # loaded from the .rmd so it won't be reloaded from the shiny if I delete it.
    }, deleteFile = FALSE)
    
    # produces the map of COVID-19 cases in Chicago
    
    output$corona_map <- renderImage({
        
        # sets the file path to its saved location from the .rmd
        
        corona_filename <- normalizePath(file.path("corona.png"))
        
        # pulls the image and sets its width to 100% so it takes up the entire space 
        # of the columns assigned in the UI.
        # I do not provide a height because I want the height to change according to 
        # the user's screen
        
        corona_image <- list(src = corona_filename, width = "100%")
    }, deleteFile = FALSE)
    
    output$linear_regression <- renderPlot({
        
        # calls the regression() function which produces the graph of 
        # COVID-19 cases per 1,000 people by percent_white. I placed
        # the function above to make the server more readable
        
        regression()
    })
    

    output$average_cases <- renderPlot({
        
        # calls the average_cases() function which produces the graph of 
        # COVID-19 cases per 1,000 people by racial group majority I placed
        # the function above to make the server more readable.
        
        average_cases()
    })
    
}

shinyApp(ui, server)


