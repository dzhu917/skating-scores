library(tidyverse)
library(rvest)
library(XML)
library(RCurl)
library(rlist)
library(janitor)

# read in the urls for ladies 
url_ladies <- c("http://www.isuresults.com/results/owg2006/CAT002RS.HTM",
         "http://www.isuresults.com/results/owg2010/CAT002RS.HTM", 
         "http://www.isuresults.com/results/owg2014/CAT002RS.HTM", 
         "http://www.isuresults.com/results/season1718/owg2018/CAT002RS.HTM")

# read in urls for mens 
url_men <- c("http://www.isuresults.com/results/owg2006/CAT001RS.HTM", 
             "http://www.isuresults.com/results/owg2010/CAT001RS.HTM",
             "http://www.isuresults.com/results/owg2014/CAT001RS.HTM",
             "http://www.isuresults.com/results/season1718/owg2018/CAT001RS.HTM")

url_pairs <- c("http://www.isuresults.com/results/owg2006/CAT003RS.HTM",
               "http://www.isuresults.com/results/owg2010/CAT003RS.HTM",
               "http://www.isuresults.com/results/owg2014/CAT003RS.HTM",
               "http://www.isuresults.com/results/season1718/owg2018/CAT003RS.HTM")

url_dance <- c("http://www.isuresults.com/results/owg2006/CAT004RS.HTM",
               "http://www.isuresults.com/results/owg2010/CAT004RS.HTM",
               "http://www.isuresults.com/results/owg2014/CAT004RS.HTM",
               "http://www.isuresults.com/results/season1718/owg2018/CAT004RS.HTM")

# With for loop -----------------------------------------------------------

olympic_pairs <- c() # empty data set

for (i in 1:length(url_pairs)) {
  
  tables <- readHTMLTable(url_ladies[i], as.data.frame = TRUE)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  test <- tables[[which.max(n.rows)]]
  test2 <- na.omit(test)
  test2$year <- i*4 + 2002
  olympic_pairs = rbind(olympic_pairs, test2)
}

olympic_pairs$discipline <- "pairs"

pairs1 <- pairs[,-1] %>%
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename(year = x2006)

pairs1 <- pairs1 %>% filter((points != "FNR") & (points != "Points"))
new_d$points <- as.double(new_d$points) 


write.csv(olympic_ladies, 'olympic_pairs.csv')