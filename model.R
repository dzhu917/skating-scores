
library(rstanarm)
library(gtsummary)
library(gt)
library(tidyverse)

# I first used lm instead of gaussian (use for posterior)

worlds_men <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Men")
worlds_ladies <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Ladies") 
worlds_pairs <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Pairs")
worlds_dance <- read.csv("data/complete_dataset.csv") %>% filter(Discipline == "Dance")

# Can create different fit objects 

fit_obj_men <- lm(Total.Points ~ Year + Nation + Host + Home,
              data = worlds_men) 
fit_obj_ladies <- lm(Total.Points ~ Year + Nation + Host + Home,
                  data = worlds_ladies) 
fit_obj_pairs <- lm(Total.Points ~ Year + Nation + Host + Home,
                  data = worlds_pairs) 
fit_obj_dance <- lm(Total.Points ~ Year + Nation + Host + Home,
                  data = worlds_dance) 

summary(fit_obj)

# Pr is the p value, and if it has the *** (less than .05) it is significant
# controlling for host nation and year, jap gets 63 points more than other teams 
# regardless of where the skater is from, in [HostChina] there were -23 points less than Canada 
# (whichever one is omitted). also look @ relative differences 

# note the r squared (1 is best) is not bad

# I also tried looking at the interactions between the variables, but the low r squared value
# indicated to me that I should probably stick with other models 

fit_obj_1 <- lm(Total.Points ~ Year*Nation + Host,
              data = worlds_men) 

summary(fit_obj_1)


# Now for stan_glm



model_men <- stan_glm(Total.Points ~ Year + Nation + Host + Home, 
                       data = worlds_men, 
                       refresh = 0)

model_ladies <- stan_glm(Total.Points ~ Year + Nation + Host + Home, 
                      data = worlds_ladies, 
                      refresh = 0)

model_pairs <- stan_glm(Total.Points ~ Year + Nation + Host + Home, 
                      data = worlds_pairs, 
                      refresh = 0)

model_dance <- stan_glm(Total.Points ~ Year + Nation + Host + Home, 
                      data = worlds_dance, 
                      refresh = 0)



print(model_men)

# Code to create the aesthetics of the table 

model_men <- stan_glm(Total.Points ~ Home, 
                       data = worlds_men, 
                       refresh = 0)

print(model_men)

model_men %>% 
  as_tibble() %>% 
  mutate(home_intercept = `(Intercept)` + Home)


tbl_regression(model_men)
table_men <- tbl_regression(model_men, intercept = TRUE) %>% 
  as_gt() %>% 
  tab_header(title = "Regression of Host Country",
             subtitle = "How Competing in Their Home Country Affects Athletes' Scores")

table_men

model_men %>% as_gt()

### 
table_men <- huxreg(model1_men,
                    stars = NULL,
                    statistics = NULL)

bold(table_men)[1,]           <- TRUE
bottom_border(table_men)[1,]  <- 0.4
align(table_men)[,2]          <- 'right'
right_padding(table_men)      <- 10
left_padding(table_men)       <- 10
width(table_men)              <- 0.35
number_format(table_men)      <- 2

table_men
