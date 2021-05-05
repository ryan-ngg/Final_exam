#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(rsconnect)

library(tidyverse)
library(shiny)
library(lubridate)
library(usmap)

wage_data <- read_csv("min_data.csv")

plotted_data <- wage_data %>% 
  group_by(State, Year) %>% 
  summarise(eff_wage = mean(Effective.Minimum.Wage)) 
  

region_options <- wage_data %>% 
    distinct(State) %>% 
    arrange(State)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("US Minimum Wage Dashboard"),

    sidebarLayout(
      sidebarPanel(
        h3("Minimum Wage in the U.S. over time"),
        p("Click on tabs to choose different visualizations of the minimum wage, the map plot shows the 2020 minimum wage and the bar plot shows the 2020 minimum wage as well. The timeline illustrates the effective minimum wage in each state over time."),
        HTML('Data from the <a href="https://www.kaggle.com/lislejoem/us-minimum-wage-by-state-from-1968-to-2017"> US DOL dataset </a>')
      ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Map",
                                plotOutput("map_plot")),
           tabPanel("Bar Plot",
                    sliderInput("State",
                                "US States ranked by 2020 minimum wage:",
                                min = 1,
                                max = 50,
                                value = 10),
                    plotOutput("bar_Plot",
                      height = "800px")),
           tabPanel("Timeline",
                    selectInput("region_options",
                               "Select Region to Highlight",
                               choices = region_options),
                    plotOutput("timeline_plot"))
                    
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bar_Plot <- renderPlot({
        wage_data %>% 
            filter(Year == "2020") %>% 
            group_by(State) %>%
            summarise(top_wage = mean(Effective.Minimum.Wage)) %>% 
            top_n(as.numeric(input$State)) %>% 
            ggplot(aes(y = reorder(State, top_wage),
                                   x = top_wage)) +
            geom_col() +
            theme_linedraw() +
            labs(x = "Minimum Wage ($)",
                 y = "") +
            geom_label(aes(label = top_wage)) +
        ggtitle("2020 Minimum Wage ranked from highest to lowest states ")
        
        # generate bins based on input$bins from ui.R

        # draw the histogram with the specified number of bins
        
    })
    
    output$map_plot <- renderPlot({
       wage_data %>% 
            group_by(state = State) %>% 
            filter(Year == "2020") %>% 
            summarise(eff_wage = mean(Effective.Minimum.Wage)) %>%  
                plot_usmap(data = .,
                       values = "eff_wage") +
            theme(legend.position = "right") +
            scale_fill_continuous(name = "Minimum Wage Highest to Lowest",
                                  low = "gray72",
                                  high = "gray23") +
        ggtitle("2020 minimum wage by U.S. state")
    })
    
    output$timeline_plot <- renderPlot({
        plotted_data <- wage_data %>% 
          group_by(State, Year) %>% 
          summarise(eff_wage = mean(Effective.Minimum.Wage))
          
        highlighted_area <- plotted_data %>% 
          filter(State == input$region_options)
        
       plotted_data %>% 
         ggplot(aes(y = eff_wage,
                x = Year,
                group = State)) +
         geom_line(color = "grey") +
         geom_point(data = highlighted_area,
                    color = "red") +
         geom_line(data = highlighted_area,
                   color = "red") +
         theme_linedraw() +
         labs(x = "Minimum Wage",
              y = "") +
         ggtitle("Minimum Wage over time by State")
       
       
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


