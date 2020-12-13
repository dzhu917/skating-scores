library(tidyverse)
library(rvest)
library(XML)
library(RCurl)
library(rlist)

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

# Rename Headers and eliminate FNR scores (top 24)

mens_2019 <- mens_2019 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
mens_2018 <- mens_2018 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
mens_2017 <- mens_2017 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
mens_2016 <- mens_2016 %>%
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
mens_2015 <- mens_2015 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
mens_2014 <- mens_2014 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
mens_2013 <- mens_2013 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
mens_2012 <- mens_2012 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24) %>% select(-a, -b)
mens_2011 <- mens_2011 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24) %>% select(-a, -b)
mens_2010 <- mens_2010 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
mens_2009 <- mens_2009 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
mens_2008 <- mens_2008 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
mens_2007 <- mens_2007 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
mens_2006 <- mens_2006 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "c", "d", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24) %>% select(-a, -b, -c, -d)

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

# Add the host country name 

mens_2019$Host <- "Japan"
mens_2018$Host <- "Italy"
mens_2017$Host <- "Finland"
mens_2016$Host <- "United States"
mens_2015$Host <- "China"
mens_2014$Host <- "Japan"
mens_2013$Host <- "Canada"
mens_2012$Host <- "France"
mens_2011$Host <- "Russia"
mens_2010$Host <- "Italy"
mens_2009$Host <- "United States"
mens_2008$Host <- "Sweden"
mens_2007$Host <- "Japan"
mens_2006$Host <- "Canada"

# Bind all of the years together and write it to a csv 

worlds_men <- rbind(mens_2019, mens_2018, mens_2017, mens_2016, mens_2015, mens_2014, mens_2013, 
                    mens_2012, mens_2011, mens_2010, mens_2009, mens_2008, mens_2007, mens_2006)

write.csv(worlds_men, 'data/worlds_men.csv')


# LADIES ------------------------------------------------------------------

ladies_2019 <- tab_2019[[8]] %>% html_table(fill = TRUE)
ladies_2018 <- tab_2018[[9]] %>% html_table(fill = TRUE)
ladies_2017 <- tab_2017[[8]] %>% html_table(fill = TRUE)
ladies_2016 <- tab_2016[[8]] %>% html_table(fill = TRUE)
ladies_2015 <- tab_2015[[6]] %>% html_table(fill = TRUE) 
ladies_2014 <- tab_2014[[8]] %>% html_table(fill = TRUE)
ladies_2013 <- tab_2013[[6]] %>% html_table(fill = TRUE)
ladies_2012 <- tab_2012[[6]] %>% html_table(fill = TRUE)
ladies_2011 <- tab_2011[[5]] %>% html_table(fill = TRUE)
ladies_2010 <- tab_2010[[10]] %>% html_table(fill = TRUE)
ladies_2009 <- tab_2009[[8]] %>% html_table(fill = TRUE)  
ladies_2008 <- tab_2008[[5]] %>% html_table(fill = TRUE)
ladies_2007 <- tab_2007[[4]] %>% html_table(fill = TRUE)
ladies_2006 <- tab_2006[[4]] %>% html_table(fill = TRUE)

# Rename Headers and eliminate FNR scores (top 24)

ladies_2019 <- ladies_2019 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
ladies_2018 <- ladies_2018 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
ladies_2017 <- ladies_2017 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
ladies_2016 <- ladies_2016 %>%
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
ladies_2015 <- ladies_2015 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
ladies_2014 <- ladies_2014 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
ladies_2013 <- ladies_2013 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:24) %>% select(-a, -b)
ladies_2012 <- ladies_2012 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24) %>% select(-a, -b)
ladies_2011 <- ladies_2011 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24) %>% select(-a, -b)
ladies_2010 <- ladies_2010 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
ladies_2009 <- ladies_2009 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
ladies_2008 <- ladies_2008 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
ladies_2007 <- ladies_2007 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24)
ladies_2006 <- ladies_2006 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "c", "d", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:24) %>% select(-a, -b, -c, -d)

# Adding a column for year  (ONLY RUN ONCE)

ladies_2019$Year <- 2019
ladies_2018$Year <- 2018
ladies_2017$Year <- 2017
ladies_2016$Year <- 2016
ladies_2015$Year <- 2015
ladies_2014$Year <- 2014
ladies_2013$Year <- 2013
ladies_2012$Year <- 2012
ladies_2011$Year <- 2011
ladies_2010$Year <- 2010
ladies_2009$Year <- 2009
ladies_2008$Year <- 2008
ladies_2007$Year <- 2007
ladies_2006$Year <- 2006

# Add the host country name 

ladies_2019$Host <- "Japan"
ladies_2018$Host <- "Italy"
ladies_2017$Host <- "Finland"
ladies_2016$Host <- "United States"
ladies_2015$Host <- "China"
ladies_2014$Host <- "Japan"
ladies_2013$Host <- "Canada"
ladies_2012$Host <- "France"
ladies_2011$Host <- "Russia"
ladies_2010$Host <- "Italy"
ladies_2009$Host <- "United States"
ladies_2008$Host <- "Sweden"
ladies_2007$Host <- "Japan"
ladies_2006$Host <- "Canada"

# Bind all of the years together and write it to a csv 

worlds_ladies <- rbind(ladies_2019, ladies_2018, ladies_2017, ladies_2016, ladies_2015, ladies_2014, ladies_2013, 
                       ladies_2012, ladies_2011, ladies_2010, ladies_2009, ladies_2008, ladies_2007, ladies_2006)

write.csv(worlds_ladies, 'data/worlds_ladies.csv')

# PAIRS -------------------------------------------------------------------

pairs_2019 <- tab_2019[[9]] %>% html_table(fill = TRUE)
pairs_2018 <- tab_2018[[10]] %>% html_table(fill = TRUE)
pairs_2017 <- tab_2017[[9]] %>% html_table(fill = TRUE)
pairs_2016 <- tab_2016[[9]] %>% html_table(fill = TRUE)
pairs_2015 <- tab_2015[[7]] %>% html_table(fill = TRUE) 
pairs_2014 <- tab_2014[[9]] %>% html_table(fill = TRUE)
pairs_2013 <- tab_2013[[7]] %>% html_table(fill = TRUE)
pairs_2012 <- tab_2012[[7]] %>% html_table(fill = TRUE)
pairs_2011 <- tab_2011[[6]] %>% html_table(fill = TRUE)
pairs_2010 <- tab_2010[[11]] %>% html_table(fill = TRUE)
pairs_2009 <- tab_2009[[11]] %>% html_table(fill = TRUE) # add 3 instead of 1
pairs_2008 <- tab_2008[[6]] %>% html_table(fill = TRUE)
pairs_2007 <- tab_2007[[5]] %>% html_table(fill = TRUE)
pairs_2006 <- tab_2006[[5]] %>% html_table(fill = TRUE)

# Rename Headers and eliminate FNR scores (top 16)

pairs_2019 <- pairs_2019 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16)
pairs_2018 <- pairs_2018 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16)
pairs_2017 <- pairs_2017 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16)
pairs_2016 <- pairs_2016 %>%
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:16) %>% select(-a, -b)
pairs_2015 <- pairs_2015 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:16) %>% select(-a, -b)
pairs_2014 <- pairs_2014 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:16) %>% select(-a, -b)
pairs_2013 <- pairs_2013 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:16) %>% select(-a, -b)
pairs_2012 <- pairs_2012 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16) %>% select(-a, -b)
pairs_2011 <- pairs_2011 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16) %>% select(-a, -b)
pairs_2010 <- pairs_2010 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16)
pairs_2009 <- pairs_2009 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16)
pairs_2008 <- pairs_2008 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16)
pairs_2007 <- pairs_2007 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16)
pairs_2006 <- pairs_2006 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:16) 

# Adding a column for year  (ONLY RUN ONCE)

pairs_2019$Year <- 2019
pairs_2018$Year <- 2018
pairs_2017$Year <- 2017
pairs_2016$Year <- 2016
pairs_2015$Year <- 2015
pairs_2014$Year <- 2014
pairs_2013$Year <- 2013
pairs_2012$Year <- 2012
pairs_2011$Year <- 2011
pairs_2010$Year <- 2010
pairs_2009$Year <- 2009
pairs_2008$Year <- 2008
pairs_2007$Year <- 2007
pairs_2006$Year <- 2006

# Add the host country name 

pairs_2019$Host <- "Japan"
pairs_2018$Host <- "Italy"
pairs_2017$Host <- "Finland"
pairs_2016$Host <- "United States"
pairs_2015$Host <- "China"
pairs_2014$Host <- "Japan"
pairs_2013$Host <- "Canada"
pairs_2012$Host <- "France"
pairs_2011$Host <- "Russia"
pairs_2010$Host <- "Italy"
pairs_2009$Host <- "United States"
pairs_2008$Host <- "Sweden"
pairs_2007$Host <- "Japan"
pairs_2006$Host <- "Canada"

# Bind all of the years together and write it to a csv 

worlds_pairs <- rbind(pairs_2019, pairs_2018, pairs_2017, pairs_2016, pairs_2015, pairs_2014, pairs_2013, 
                       pairs_2012, pairs_2011, pairs_2010, pairs_2009, pairs_2008, pairs_2007, pairs_2006)

write.csv(worlds_pairs, 'data/worlds_pairs.csv')


# DANCE -------------------------------------------------------------------

dance_2019 <- tab_2019[[10]] %>% html_table(fill = TRUE)
dance_2018 <- tab_2018[[11]] %>% html_table(fill = TRUE)
dance_2017 <- tab_2017[[10]] %>% html_table(fill = TRUE)
dance_2016 <- tab_2016[[10]] %>% html_table(fill = TRUE)
dance_2015 <- tab_2015[[8]] %>% html_table(fill = TRUE) 
dance_2014 <- tab_2014[[10]] %>% html_table(fill = TRUE)
dance_2013 <- tab_2013[[8]] %>% html_table(fill = TRUE)
dance_2012 <- tab_2012[[8]] %>% html_table(fill = TRUE)
dance_2011 <- tab_2011[[7]] %>% html_table(fill = TRUE)
dance_2010 <- tab_2010[[12]] %>% html_table(fill = TRUE)
dance_2009 <- tab_2009[[15]] %>% html_table(fill = TRUE) # add 3 instead of 1
dance_2008 <- tab_2008[[7]] %>% html_table(fill = TRUE)
dance_2007 <- tab_2007[[6]] %>% html_table(fill = TRUE)
dance_2006 <- tab_2006[[6]] %>% html_table(fill = TRUE)

# Rename Headers and eliminate FNR scores (top 16)

dance_2019 <- dance_2019 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20)
dance_2018 <- dance_2018 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20)
dance_2017 <- dance_2017 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20)
dance_2016 <- dance_2016 %>%
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2015 <- dance_2015 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2014 <- dance_2014 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2013 <- dance_2013 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "SP_rank", "SP_score", "FS_rank", "FS_score", "a", "b")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2012 <- dance_2012 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points","a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2011 <- dance_2011 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points","a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20) %>% select(-a, -b)

# There used to be three types of dances 

dance_2010 <- dance_2010 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2009 <- dance_2009 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2008 <- dance_2008 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2007 <- dance_2007 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20) %>% select(-a, -b)
dance_2006 <- dance_2006 %>% 
  setNames(c("Rank", "Name", "Nation", "Total Points", "a", "b", "SP_rank", "SP_score", "FS_rank", "FS_score")) %>% 
  slice(1:20) %>% select(-a, -b)

# Adding a column for year  (ONLY RUN ONCE)

dance_2019$Year <- 2019
dance_2018$Year <- 2018
dance_2017$Year <- 2017
dance_2016$Year <- 2016
dance_2015$Year <- 2015
dance_2014$Year <- 2014
dance_2013$Year <- 2013
dance_2012$Year <- 2012
dance_2011$Year <- 2011
dance_2010$Year <- 2010
dance_2009$Year <- 2009
dance_2008$Year <- 2008
dance_2007$Year <- 2007
dance_2006$Year <- 2006

# Add the host country name 

dance_2019$Host <- "Japan"
dance_2018$Host <- "Italy"
dance_2017$Host <- "Finland"
dance_2016$Host <- "United States"
dance_2015$Host <- "China"
dance_2014$Host <- "Japan"
dance_2013$Host <- "Canada"
dance_2012$Host <- "France"
dance_2011$Host <- "Russia"
dance_2010$Host <- "Italy"
dance_2009$Host <- "United States"
dance_2008$Host <- "Sweden"
dance_2007$Host <- "Japan"
dance_2006$Host <- "Canada"

# Bind all of the years together and write it to a csv 

worlds_dance<- rbind(dance_2019, dance_2018, dance_2017, dance_2016, dance_2015, dance_2014, dance_2013, 
                     dance_2012, dance_2011, dance_2010, dance_2009, dance_2008, dance_2007, dance_2006)

write.csv(worlds_dance, 'data/worlds_dance.csv')


# Complete Dataset --------------------------------------------------------

worlds_men$Discipline <- "Men"
worlds_ladies$Discipline <- "Ladies"
worlds_pairs$Discipline <- "Pairs"
worlds_dance$Discipline <- "Dance"

complete_dataset <- rbind(worlds_men, worlds_ladies, worlds_pairs, worlds_dance)

write.csv(complete_dataset, 'data/complete_dataset.csv')


# Adding Home Column ------------------------------------------------------

complete_dataset$Home <- FALSE

# If the skater is competing in their home country, set this column value to TRUE 

for (row in 1:nrow(complete_dataset)) {
  host <- complete_dataset[row, "Host"]
  nation  <- complete_dataset[row, "Nation"]
  
  if(host == nation) {
    complete_dataset[row, "Home"] = TRUE
    
  }
}

write.csv(complete_dataset, 'data/complete_dataset.csv')

