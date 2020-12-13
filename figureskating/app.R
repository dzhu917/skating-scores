
library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(janitor)

# import scraped olympic data, use for running shinyapp 
d <- read.csv("olympicgames.csv") 

d <- read.csv("figureskating/olympicgames.csv")

new_d <- d[,-1] %>%
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    rename(year = x2010)

new_d <- new_d %>% filter((points != "FNR") & (points != "Points"))
new_d$points <- as.double(new_d$points) 

# Olympic Men
olympic_men <- read.csv("example/olympic_men.csv")
new_men <- olympic_men[,-1] %>%
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    rename(year = x2006)

new_men <- new_men %>% filter((points != "FNR") & (points != "Points"))
new_men$points <- as.double(new_men$points) 

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Figure Skating Scores",
    tabPanel("Home", 
             titlePanel("Home"),
             h3("Trends in International and Domestic Figure Skating Scores")),
    
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "X variable", choices = c("year")),
                 selectInput("y", "Y variable", choices = c("points", "sp", "fs")),
                 selectInput("geom", "geom", c("point", "jitter", "line")),
                 plotOutput("plot"),
                 plotOutput("plot2")
             )),
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project combines two of my interests, figure skating and data visualization, to analyze the trends in competitive
               figure skating scores on both the national and international circuits. All international scoring data was scraped from the",
             a("International Skating Union (ISU) website.", href = "https://www.isu.org/figure-skating/events/grand-prix-of-figure-skating/gp-archives")),
             h3("About Me"),
             p("My name is Diana and I study Applied Mathematics and Energy and Environment. Feel free to reach out at", 
             a("dianazhu@college.harvard.edu", href = "mailto:dianazhu@college.harvard.edu"), "with any comments or questions!"))
    )

server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               column = geom_col(),
               #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter(),
               line = geom_line()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(new_d, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
    
    # make sure it's recognized as integers
    output$plot2 <- renderPlot({
        ggplot(new_d, aes(x = year, y = points)) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
