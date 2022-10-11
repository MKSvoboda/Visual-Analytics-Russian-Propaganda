#### Scraping and processing data visualization

library(tidyverse)
library(academictwitteR)
library(lubridate)
library(jsonlite)
setwd("D:/Google Drive/MSc Applied Social Data Analysis/Data Visualisation/Assessment 2 data visualization/Data processing")
#set_bearer()
get_bearer()


processtweets <- function(data){
  
  data <-select(data,!c(entities, withheld,attachments))
  
  data$like_count <- data$public_metrics$like_count
  data$retweet_count <- data$public_metrics$retweet_count
  data$reply_count <- data$public_metrics$reply_count
  
  data$public_metrics <- NULL
  data$geo <- NULL
  
  data
}



#scraped <-
#  get_all_tweets(
#    query = c("Ukraine","Donbas","Crimea","Luhansk","Donetsk"),
#    users = c("RT_com","tassagency_en","SputnikInt"),
#    start_tweets = "2008-01-01T00:00:00Z",
#    end_tweets = "2022-03-28T00:00:00Z",
#    n = 1000000
#  )

table(scraped$author_id)

rawtweets <- processtweets(scraped)
rawtweets$date <- str_replace(rawtweets$created_at,"\\T.*","")
rawtweets$date <- as.Date(rawtweets$date)

write_csv(rawtweets,"rawtweets.csv")

####################################################################################################################




### Loading data

rawtweets <- read.csv("rawtweets.csv")
trainingdata <- read.csv("codedtweetsold.csv") %>% filter(training == 1)


### recoding main dataset

tweets <- rawtweets
tweets$training <- 0
tweets$propaganda <- NA

table(tweets$author_id)



### saving as both JSON for python and CSV for visualization




trainingdata$roughintensityperiod <- NULL
trainingdata$exactintensityperiod <- NULL
trainingdata$identifier <- NULL


data <- rbind(select(trainingdata,text,propaganda,), select(tweets, text, propaganda))

colnames(tweets)
colnames(trainingdata)















################# old

tweetsjson <- toJSON(x = select(tweets, text, propaganda), dataframe = 'values', pretty = T)
write(tweetsjson, "tweetsjson.json")

trainingjson <- toJSON(x = select(trainingdata, text, propaganda), dataframe = 'values', pretty = F)
write(tweetsjson, "trainingjson.json")


