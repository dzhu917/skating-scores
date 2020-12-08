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
library(rstanarm)
library(broom)
library(broom.mixed)

# extra libraries
# library(wordcloud)
# library(tm)
# library(wordcloud2)
# library(gganimate)
# library(broom.mixed)
# library(markdown)

# get rid of skatingscores/ when trying to run the app, leave without for final 
men <- read.csv("data/worlds_men.csv") 
ladies <- read.csv("data/worlds_ladies.csv") 
pairs <- read.csv("data/worlds_pairs.csv") 
dance <- read.csv("data/worlds_dance.csv") 
complete_dataset <- read.csv("data/complete_dataset.csv")

# Modeling code 
worlds_men <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Men")
worlds_ladies <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Ladies")
worlds_pairs <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Pairs")
worlds_dance <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Dance")

# Men 
model_men <- stan_glm(Total.Points ~ Home,
                      data = worlds_men,
                      refresh = 0)

table_men <- tbl_regression(model_men, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Host Country",
               subtitle = "How Competing in Their Home Country Affects Mens' Scores")

# Ladies
model_ladies <- stan_glm(Total.Points ~ Home,
                      data = worlds_ladies,
                      refresh = 0)

table_ladies <- tbl_regression(model_ladies, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Host Country",
               subtitle = "How Competing in Their Home Country Affects Ladies Scores")

# Pairs
model_pairs <- stan_glm(Total.Points ~ Home,
                      data = worlds_pairs,
                      refresh = 0)

table_pairs <- tbl_regression(model_pairs, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Host Country",
               subtitle = "How Competing in Their Home Country Affects Pairs Scores")

# Dance
model_dance <- stan_glm(Total.Points ~ Home,
                      data = worlds_dance,
                      refresh = 0)

table_dance <- tbl_regression(model_dance, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Host Country",
               subtitle = "How Competing in Their Home Country Affects Dance Scores")

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
             gt_output(outputId = "mensTable"),
             h4("Discussion"),
             p("This regression table was made with a Bayesian generalized linear model (stan_glm), modeling a skater's 
               total score as the outcome and factoring in whether they were competing in their home country or not. The Intercept
               value of 218 refers to the average World Championships score for skaters not competing in their home country. 
               The Beta value for the Home variable is 21, which means that men competing in their home country receive, on average,
               a score 21 points higher than those who are not competing in their home country."),
               
             p("We are 95% confident the true value of the total score for a male athlete competing outside of his home country lies between (214, 221),
             and the true difference in value of a male athlete competing at home is between (8.2,33). In the future, I can add year and the nation that 
             they're in as predictors to control for changes in scoring over time and to account for the differences in the way that each nation inherently scores."),### FINISH HERE, add the latex equation in markdown
             gt_output(outputId = "ladiesTable"),
             h4("Discussion"),
             p("The Intercept value of 162 refers to the average World Championships score for female skaters not competing in their home country. 
               The Beta value for the Home variable is 22, which means that ladies competing in their home country receive, on average,
               a score 22 points higher than those who are not competing in their home country."),
             p("We are 95% confident the true value of the total score for a female athlete competing outside of his home country lies between (159, 166),
             and the true difference in value of a female athlete competing at home is between (11,33)."),
             gt_output(outputId = "pairsTable"),
             h4("Discussion"),
             p("The Intercept value of 176 refers to the average World Championships score for pair skaters not competing in their home country. 
               The Beta value for the Home variable is 8.7, which means that pair skaters competing in their home country receive, on average,
               a score 22 points higher than those who are not competing in their home country."),
             
             p("We are 95% confident the true value of the total score for a pairs skater competing outside of his home country lies between (172, 180),
             and the true different in value of a male athlete competing at home is between (-4.9,23), meaning that pair skaters competing in their home country 
             may not always have a higher score on average than those who are not."),
             gt_output(outputId = "danceTable"),
             h4("Discussion"),
             p("The Intercept value of 164 refers to the average World Championships score for ice dancers not competing in their home country. 
               The Beta value for the Home variable is 9.3, which means that ice dancers competing in their home country receive, on average,
               a score 9.3 points higher than those who are not competing in their home country."),
             
             p("We are 95% confident the true value of the total score for ice dancers competing outside of their home country lies between (161, 166),
             and the true different in value of ice dancers competing at home is between (-1.1,20). It is evident that partnered disciplines (i.e. pairs, dance)
               experience less of a home country advantage than singles skates (i.e. mens, ladies).")
             ), ## add the other disciplines 
    
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
    output$ladiesTable<-render_gt({
        expr = table_ladies
    })
    output$pairsTable<-render_gt({
        expr = table_pairs
    })
    output$danceTable<-render_gt({
        expr = table_dance
    })
    # 
    # output$barPlot <- renderPlot({
    #     create.non.ranked.plot(input$yearSlide)
    # })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
