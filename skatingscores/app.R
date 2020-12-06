#    http://shiny.rstudio.com/
library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(janitor)
library(shinythemes)

# extra libraries
# library(wordcloud)
# library(tm)
# library(wordcloud2)
# library(gganimate)
# library(broom.mixed)
# library(gtsummary)
# library(rstanarm)
# library(markdown)

# get rid of skatingscores/ when trying to run the app, leave without for final 
men <- read.csv("data/worlds_men.csv") 
ladies <- read.csv("data/worlds_ladies.csv") 
pairs <- read.csv("data/worlds_pairs.csv") 
dance <- read.csv("data/worlds_dance.csv") 
complete_dataset <- read.csv("data/complete_dataset.csv")

# Define a ui 
ui <- navbarPage(
    "Figure Skating Scores",
    # Add a theme 
    theme = shinytheme("flatly"),
    tabPanel("Home", 
             titlePanel("Home"),
             h3("Trends in International and Domestic Figure Skating Scores")),
    
    tabPanel("Model",
             ),
    
    tabPanel("Historical International Scores",
             titlePanel("The World Championships Over the Years"),
             p("Over time, it's clear that overall scores have increased."),
             
             # Select a discipline from the dropdown menu for its time series
             # Multiple can be selected for comparison
             
             sidebarLayout(
                 sidebarPanel(
                     selectInput("discipline",label = strong("Discipline"), # FIX "name" 
                                 choices = unique(complete_dataset$Discipline),
                                 selected = "Men",
                                 multiple = TRUE
                     )),
                 
                 # Show scores curve 
                 
                 mainPanel(
                     plotOutput("linePlot"),
                     br(),
                     br(),
                     h4("As you will notice by searching and comparing different players, not everyone
                  has the same career trajectories. The nice curves from the previous page
                  are only the averages of all the various shaped curves. Not all players peak exactly 
                  around 28 and not everyone will begin to decline shortly after, many players 
                  plateau for awhile. However, this is a fun tool to compare many of the greats
                  in history!")
                 )
             )
             
            ),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project combines two of my interests, figure skating and data visualization, to analyze the trends in competitive
               figure skating scores on both the national and international circuits. All international scoring data was scraped from the",
               a("International Skating Union (ISU) website.", href = "https://www.isu.org/figure-skating/events/grand-prix-of-figure-skating/gp-archives")),
             h3("About Me"),
             p("My name is Diana and I study Statistics & Data Science with a secondary in Energy and Environment. Feel free to reach out at", 
               a("dianazhu@college.harvard.edu", href = "mailto:dianazhu@college.harvard.edu"), "with any comments or questions!"))
)

# Define server logic required
server <- function(input, output, session) {
    
    # necessary libraries
    library(ggthemes)
    library(directlabels)
    
    # plot displaying time series of worlds scores 
    subset<-reactive({complete_dataset%>% filter(Discipline %in%input$discipline)})
    
    output$linePlot <- renderPlot({
        
        # draws line plot for player chosen
        
        ggplot(subset(), aes(x=subset()$Year, y=subset()$Total.Points, color = subset()$Discipline))+
            
            #smoothed trend line of performance  
            
            geom_smooth(formula = y~x+x^2, se = FALSE, size = 1.5)+
            
            # actual datapoints and WARs from players career. 
            
            #geom_line(alpha = 0.20)+
            geom_jitter(alpha = 0.7)+
            
            # ranges of WAR and Age in the dataset
            
            scale_y_continuous(breaks = seq(50,350,10), limits = c(50,350)) + 
            scale_x_continuous(breaks = seq(2006,2019,1), limits = c(2006,2019)) +
            
            
            labs(x="Year", y = "Total Competition Score", color = "discipline",
                 caption = "The smoothed line represents the general trend 
           of average World Championships scores while the transparent points
           represent the competitors' scores with a small amount of random variation added.")+
            
            # for aesthetic purpose and including axis labels 
            
            theme_fivethirtyeight()+
            theme(axis.title = element_text(colour = "black" ))
    }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
