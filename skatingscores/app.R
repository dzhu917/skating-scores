#    http://shiny.rstudio.com/
library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(janitor)
library(shinythemes)
library(gt)
library(gtsummary)

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

model_men <- stan_glm(Total.Points ~ Home, 
                      data = worlds_men, 
                      refresh = 0)

table_men <- tbl_regression(model_men, intercept = TRUE) %>% 
    as_gt() %>% 
    tab_header(title = "Regression of Host Country",
               subtitle = "How Competing in Their Home Country Affects Athletes' Scores")

# Define a ui 
ui <- navbarPage(
    "Analyzing Figure Skating Scores",
    # Add a theme 
    theme = shinytheme("flatly"),
    
    # Create a histogram of number of medals from each country over time
    # tabPanel("Top Scoring Countries",
    #          box(sliderInput("yearSlide", label = NULL, min = 2006, max = 2019,
    #                          sep = '', ticks = 14, value = 2019),
    #              plotOutput("barPlot")),
    #          ),
    
    tabPanel("Historical International Scores",
             titlePanel("The World Championships Over the Years"),

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
                     h3("Discussion"),
                     h5("By searching and comparing different disciplines, it is clear that each discipline
                     exihibits a general positive trend throughout the years, which is likely in part due to 
                     the increasing ability of athletes to successfully complete high-value elements and in part 
                     due to scoring inflation. We can display the data as jittered points to reduce overplotting 
                     and overcome the dicreteness of the dataset as it adds a small amount of random variation to 
                     the location of each point. The smoothed line represents the general trend 
                     of average World Championships scores while the transparent points
                     represent the competitors' scores with a small amount of random variation added.")
                 )
             )
             
            ),
    tabPanel("Statistical Modeling", 
             titlePanel("Building a Statistical Model"),
             splitLayout(
                 gt_output(outputId = "mensTable"),
                 h4("Discussion") ### FINISH HERE 
             )),
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background"),
             p("This project combines two of my interests, figure skating and data visualization, to analyze the trends in competitive
               figure skating scores on both the national and international circuits. While millions of viewers tune into watch figure skating
               during the Winter Olympics, much fewer people understand the nuances of IJS (International Judging System) scoring, the current scoring system
               used to judge all international competitions sanctioned by the ISU (International Skating Union). This system replaced the previous",
               a("6.0 system", href = "https://en.wikipedia.org/wiki/6.0_system"), "beginning in 2004-05 season as an attempt to make judging more objective in 
               response to the",
               a("2002 Winter Olympics figure skating scandal.", href = "https://en.wikipedia.org/wiki/2002_Winter_Olympics_figure_skating_scandal"), 
               "I was interested in exploring how truly unbiased this updated system is by looking at scores over the years and examining factors such 
               as home country and host nation. All of the scoring data was scraped from the",
               a("ISU results", href = "https://www.isu.org/figure-skating/events/grand-prix-of-figure-skating/gp-archives"), 
               "which can also be found as data tables on each skating competition's Wikipedia pages like", 
               a("this", href = "https://en.wikipedia.org/wiki/2019_World_Figure_Skating_Championships"), "one."),
             h3("About Me"),
             p("My name is Diana and I created this project as a sophomore at Harvard College graduating in the Class of 2023. I'm pursuing a B.A. degree in Statistics & Data Science with a secondary in Energy and Environment. 
               I've always been interested in learning how to apply R to my personal interests, and this project was the perfect opportunity to do so! If you're interested in learning more about
               the project or myself, please feel free to reach out through email at", 
               a("dianazhu@college.harvard.edu", href = "mailto:dianazhu@college.harvard.edu"), "connect with me on",
               a("LinkedIn", href = "https://www.linkedin.com/in/diana-zhu/"), "or view the project's",
               a("GitHub repo link", href = "https://github.com/dzhu917/skating-scores"), "page. Thanks for visiting!"),
             h3("Acknowledgements"),
             p("I'd like to thank my TF, Dan Baissa, for guiding me throughout this course and the creation of this project. 
               I really appreciate how incredibly supportive he has been this semester, especially during such tumultuous times."))
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
            
            scale_y_continuous(breaks = seq(50,350,10), limits = c(90,350)) + 
            scale_x_continuous(breaks = seq(2006,2019,1), limits = c(2006,2019)) +
            
            
            labs(x = "Year", y = "Total Competition Score", color = "Discipline",
                 title = "Figure Skating World Championship Scores Over Time") +
            
            # for aesthetic purpose and including axis labels 
            
            theme_fivethirtyeight()+
            theme(axis.title = element_text(colour = "black" ))
    }
    )
    output$mensTable<-render_gt({
        expr = table_men
    })
    
    # output$barPlot <- renderPlot({
    #     create.non.ranked.plot(input$yearSlide)
    # })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
