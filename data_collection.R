library(tidyverse)
library(rvest)
library(XML)
library(RCurl)
library(rlist)
library(janitor)

# Specify url
url_worlds_2019 <- paste0("https://en.wikipedia.org/wiki/2019_World_Figure_Skating_Championships")
url_worlds_2018 <- paste0("https://en.wikipedia.org/wiki/2018_World_Figure_Skating_Championships")
url_worlds_2017 <- paste0("https://en.wikipedia.org/wiki/2017_World_Figure_Skating_Championships")
url_worlds_2016 <- paste0("https://en.wikipedia.org/wiki/2016_World_Figure_Skating_Championships")
url_worlds_2015 <- paste0("https://en.wikipedia.org/wiki/2015_World_Figure_Skating_Championships")
url_worlds_2014 <- paste0("https://en.wikipedia.org/wiki/2014_World_Figure_Skating_Championships")
url_worlds_2013 <- paste0("https://en.wikipedia.org/wiki/2013_World_Figure_Skating_Championships")
url_worlds_2012 <- paste0("https://en.wikipedia.org/wiki/2012_World_Figure_Skating_Championships")
url_worlds_2011 <- paste0("https://en.wikipedia.org/wiki/2011_World_Figure_Skating_Championships")
url_worlds_2010 <- paste0("https://en.wikipedia.org/wiki/2010_World_Figure_Skating_Championships")
url_worlds_2009 <- paste0("https://en.wikipedia.org/wiki/2009_World_Figure_Skating_Championships")
url_worlds_2008 <- paste0("https://en.wikipedia.org/wiki/2008_World_Figure_Skating_Championships")
url_worlds_2007 <- paste0("https://en.wikipedia.org/wiki/2007_World_Figure_Skating_Championships")
url_worlds_2006 <- paste0("https://en.wikipedia.org/wiki/2006_World_Figure_Skating_Championships")

# Loading Worlds 2019 scores 
worlds_2019 <- read_html(url_worlds_2019)
worlds_2018 <- read_html(url_worlds_2018)
worlds_2017 <- read_html(url_worlds_2017)
worlds_2016 <- read_html(url_worlds_2016)
worlds_2015 <- read_html(url_worlds_2015)
worlds_2014 <- read_html(url_worlds_2014)
worlds_2013 <- read_html(url_worlds_2013)
worlds_2012 <- read_html(url_worlds_2012)
worlds_2011 <- read_html(url_worlds_2011)
worlds_2010 <- read_html(url_worlds_2010)
worlds_2009 <- read_html(url_worlds_2009)
worlds_2008 <- read_html(url_worlds_2008)
worlds_2007 <- read_html(url_worlds_2007)
worlds_2006 <- read_html(url_worlds_2006)

# Extracts just the tables from HTML 
tab_2019 <- worlds_2019 %>% html_nodes("table")
tab_2018 <- worlds_2018 %>% html_nodes("table")
tab_2017 <- worlds_2017 %>% html_nodes("table")
tab_2016 <- worlds_2016 %>% html_nodes("table")
tab_2015 <- worlds_2015 %>% html_nodes("table")
tab_2014 <- worlds_2014 %>% html_nodes("table")
tab_2013 <- worlds_2013 %>% html_nodes("table")
tab_2012 <- worlds_2012 %>% html_nodes("table")
tab_2011 <- worlds_2011 %>% html_nodes("table")
tab_2010 <- worlds_2010 %>% html_nodes("table")
tab_2009 <- worlds_2009 %>% html_nodes("table")
tab_2008 <- worlds_2008 %>% html_nodes("table")
tab_2007 <- worlds_2007 %>% html_nodes("table")
tab_2006 <- worlds_2006 %>% html_nodes("table")

# Always in the order of Men, Ladies, Pairs, Dance 


# MEN ---------------------------------------------------------------------
mens_2019 <- tab_2019[[7]] %>% html_table(fill = TRUE)
mens_2018 <- tab_2018[[8]] %>% html_table(fill = TRUE)
mens_2017 <- tab_2017[[7]] %>% html_table(fill = TRUE)
mens_2016 <- tab_2016[[7]] %>% html_table(fill = TRUE)
mens_2015 <- tab_2015[[5]] %>% html_table(fill = TRUE) 
mens_2014 <- tab_2014[[7]] %>% html_table(fill = TRUE)
mens_2013 <- tab_2013[[5]] %>% html_table(fill = TRUE)
mens_2012 <- tab_2012[[5]] %>% html_table(fill = TRUE)
mens_2011 <- tab_2011[[4]] %>% html_table(fill = TRUE)
mens_2010 <- tab_2010[[9]] %>% html_table(fill = TRUE)
mens_2009 <- tab_2009[[5]] %>% html_table(fill = TRUE)
mens_2008 <- tab_2008[[4]] %>% html_table(fill = TRUE)
mens_2007 <- tab_2007[[3]] %>% html_table(fill = TRUE)
mens_2006 <- tab_2006[[3]] %>% html_table(fill = TRUE)

# Rename Headers and eliminate FNR scores 
mens_2019 <- mens_2019 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10)
mens_2018 <- mens_2018 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10)
mens_2017 <- mens_2017 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10)
mens_2016 <- mens_2016 %>%
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:10) %>% select(-a, -b)
mens_2015 <- mens_2015 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:10) %>% select(-a, -b)
mens_2014 <- mens_2014 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:10) %>% select(-a, -b)
mens_2013 <- mens_2013 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:10) %>% select(-a, -b)
mens_2012 <- mens_2012 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:10) %>% select(-a, -b)
mens_2011 <- mens_2011 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:10) %>% select(-a, -b)
mens_2010 <- mens_2010 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10)
mens_2009 <- mens_2009 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10)
mens_2008 <- mens_2008 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10)
mens_2007 <- mens_2007 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10)
mens_2006 <- mens_2006 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "c", "d", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:10) %>% select(-a, -b, -c, -d)

# Adding a column for year  (ONLY RUN ONCE)
mens_2019$Year <- 2019
mens_2018$Year <- 2018
mens_2017$Year <- 2017
mens_2016$Year <- 2016
mens_2015$Year <- 2015
mens_2014$Year <- 2014
mens_2013$Year <- 2013
mens_2012$Year <- 2012
mens_2011$Year <- 2011
mens_2010$Year <- 2010
mens_2009$Year <- 2009
mens_2008$Year <- 2008
mens_2007$Year <- 2007
mens_2006$Year <- 2006

# Bind all of the years together and write it to a csv 
worlds_men <- rbind(mens_2019, mens_2018, mens_2017, mens_2016, mens_2015, mens_2014, mens_2013, 
                    mens_2012, mens_2011, mens_2010, mens_2009, mens_2008, mens_2007, mens_2006)
write.csv(worlds_men, 'data/worlds_men.csv')


# LADIES ------------------------------------------------------------------

