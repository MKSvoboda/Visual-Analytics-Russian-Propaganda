
### Data vis assessment 2 visualization
library(tidyverse)
library(lubridate)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library("quanteda")
library("quanteda.textplots")
library("quanteda.textstats")
library("readtext")
library("seededlda")
library("philentropy")
library("gridExtra")
Sys.setlocale("LC_TIME", "C");
setwd('D:/Google Drive/MSc Applied Social Data Analysis/Data Visualisation/Assessment 2 data visualization/visualization')


#Reading and preparing data

dataunlabeled <- read.csv('D:/Google Drive/MSc Applied Social Data Analysis/Data Visualisation/Assessment 2 data visualization/Data processing/rawtweets.csv')
predicted <- read.csv('D:/Google Drive/MSc Applied Social Data Analysis/Data Visualisation/Assessment 2 data visualization/Data processing/Python labeling/labeled data and labels/y_predictdf.csv')
tokens <- read.csv('D:/Google Drive/MSc Applied Social Data Analysis/Data Visualisation/Assessment 2 data visualization/Data processing/Python labeling/labeled data and labels/tokens.csv')



data <- dataunlabeled
data$propaganda <- predicted$X0

data <- data %>%
  mutate(user = case_when(
    author_id == 64643056 ~ "RT",
    author_id == 1903712426 ~ "TASS",
    author_id == 34262462 ~ "Sputnik"))

data <- data[ ,c(3,15,16,14,c(4:13))]

tokenslist <- tokens$X0
tokenslist2 <- tokenslist[901:24478]

data$tokens <- tokenslist2

#write.csv(data, "D://Google Drive//MSc Applied Social Data Analysis//Data Visualisation//Assessment 2 data visualization//visualization//russtatemediacoded.csv")

##############################################################################################

### Defining a function that propaganda aggregates by day
aggregatebyday1 <- function(data){
  x <- data %>%
    group_by(as.Date(date)) %>%
    summarise_at(vars(propaganda), list(frequency = sum))

  y <- data %>%
    group_by(as.Date(date)) %>%
    summarise_at(vars(propaganda), list(relfrequency = mean))
  
  output <- full_join(x,y, by = "as.Date(date)")
  output <- rename(output, date = "as.Date(date)")
  
  output
}
###################################



a <- aggregatebyday1(filter(data, user == "RT"))
a$Source <- "RT"
b <- aggregatebyday1(filter(data, user == "TASS"))
b$Source <- "TASS"
c <- aggregatebyday1(filter(data, user == "Sputnik"))
c$Source <- "Sputnik"

timeseriesbysource <- rbind(a,b,c)

######################################
aggregatebyday2 <- function(data){
  x <- data %>%
    group_by(as.Date(date)) %>%
    summarise_at(vars(retweet_count), list(frequency = sum))
  
  y <- data %>%
    group_by(as.Date(date)) %>%
    summarise_at(vars(retweet_count), list(relfrequency = mean))
  
  output <- full_join(x,y, by = "as.Date(date)")
  output <- rename(output, date = "as.Date(date)")
  
  output
}

##########
d <- aggregatebyday2(filter(data, user == "RT"))
d$Source <- "RT"
e <- aggregatebyday2(filter(data, user == "TASS"))
e$Source <- "TASS"
f <- aggregatebyday2(filter(data, user == "Sputnik"))
f$Source <- "Sputnik"

tsretweets <- rbind(d,e,f)



###Descriptive stats########################################################
mean(filter(data, user == "RT")$propaganda)
mean(filter(data, user == "TASS")$propaganda)
mean(filter(data, user == "Sputnik")$propaganda)

range(data$date)








###### Visualization #####################################################

### media by popularity
media <- c("RT","RT","RT","TASS","TASS","TASS","Sputnik","Sputnik","Sputnik")
metric <- as.factor(c("Likes","Retweets","Replies","Likes","Retweets","Replies","Likes","Retweets","Replies"))
value <- c(mean(filter(data, user == "RT")$like_count),mean(filter(data, user == "RT")$retweet_count),mean(filter(data, user == "RT")$reply_count),
           mean(filter(data, user == "TASS")$like_count),mean(filter(data, user == "TASS")$retweet_count),mean(filter(data, user == "TASS")$reply_count),
           mean(filter(data, user == "Sputnik")$like_count),mean(filter(data, user == "Sputnik")$retweet_count),mean(filter(data, user == "Sputnik")$reply_count))

metric<- factor(metric, levels = c("Likes","Retweets","Replies"))

popplotdata <- data.frame(media,metric,value)





ggplot(data = popplotdata, aes(x = metric, y = value, fill = media)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  
  labs(title = "Russian state media\npopularity comparison",
       y = "Average per tweet", x = "Popularity metric type")





















### timeseries by popularity
ggplot(data = tsretweets, aes(x = as.Date(date), y = frequency, color = Source))+
  
  geom_smooth(se=F) + 
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(limit=c(10,NA),oob=squish) +
  
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  
  labs(title = "Reach of RT, TASS and Sputnik over time",
       x = "Time", y = "Average number of retweets")



#############################

### wordclouds
set.seed(42)

# data preparation
datawc <- data
datawc$tokens <- gsub("russia", "", datawc$tokens)
datawc$tokens <- gsub("say", "", datawc$tokens)
datawc$tokens <- gsub("online", "", datawc$tokens)
datawc$tokens <- gsub("world", "", datawc$tokens)
datawc$tokens <- gsub("update", "", datawc$tokens)
datawc$tokens <- gsub("breaking", "", datawc$tokens)
datawc$tokens <- gsub("sputnikupdates", "", datawc$tokens)
datawc$tokens <- gsub("live", "", datawc$tokens)
datawc$tokens <- gsub("sputnik", "", datawc$tokens)
datawc$tokens <- gsub(" u ", "", datawc$tokens)
datawc$tokens <- gsub(" s ", "", datawc$tokens)
datawc$tokens <- gsub(" s", "", datawc$tokens)
datawc$tokens <- gsub("n ", "", datawc$tokens)
datawc$tokens <- gsub("ukraine", "", datawc$tokens)
datawc$tokens <- gsub("ukrainian", "", datawc$tokens)
datawc$tokens <- gsub("president", "", datawc$tokens)
datawc$tokens <- gsub("ial", "", datawc$tokens)
datawc$tokens <- gsub("flu", "", datawc$tokens)
datawc$tokens <- gsub("amid", "", datawc$tokens)
datawc$tokens <- gsub("amp", "", datawc$tokens)
datawc$tokens <- gsub("urgent", "", datawc$tokens)
datawc$tokens <- gsub("photo", "", datawc$tokens)
datawc$tokens <- gsub("??", "", datawc$tokens)
datawc$tokens <- gsub(" ?? ", "", datawc$tokens)
datawc$tokens <- gsub(" ??", "", datawc$tokens)
datawc$tokens <- gsub("?? ", "", datawc$tokens)








textspos <- corpus(filter(datawc, propaganda == 1), text_field = "tokens")
textsall <- corpus(datawc, text_field = "tokens")


##############


corpus_subset(textspos, 
              user %in% c("RT", "Sputnik", "TASS")) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm() %>%
  dfm_group(groups = user) %>%
  dfm_trim(min_termfreq = 3, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE,
                     color=c("chartreuse3","orange", "blue"), 
                     max_words=100, 
                     min_size=.2, 
                     max_size=5)


#
corpus_subset(textsall, 
              propaganda %in% c(0,1)) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm() %>%
  dfm_group(groups = propaganda) %>%
  textstat_keyness(target = "1") %>%
  textplot_keyness(color = c("red3", "darkblue"), n = 20)








##################################






















### Full timeseries plot ###
ggplot(data = timeseriesbysource, aes(x = as.Date(date), y = relfrequency, color = Source))+
  
  geom_smooth(se=F) + 
  
  scale_x_date(date_breaks = "1 year", date_labels = "%b/%Y")+
  
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  
  labs(title = "Propaganda intensity timeline",
       x = "Time", y = " Relative frequency of propaganda per day") +
  
  ggplot2::annotate("rect",xmin = as.Date("2009-04-24"), xmax = as.Date("2014-02-20"), ymin = 0.45, ymax = 0.48, alpha = 1, fill = "deepskyblue3") +
  ggplot2::annotate("rect",xmin = as.Date("2014-02-20"), xmax = as.Date("2022-02-24"), ymin = 0.45, ymax = 0.48, alpha = 1, fill = "darkorange") +
  ggplot2::annotate("rect",xmin = as.Date("2022-02-24"), xmax = as.Date("2022-03-27"), ymin = 0.45, ymax = 0.48, alpha = 1, fill = "red2") +
  
  ggplot2::annotate("segment", x = as.Date("2021-11-10"),xend = as.Date("2021-11-10"), y = 0.45, yend = 0.43 ,size = 1, colour = "black")+
  ggplot2::annotate("text", x = as.Date("2021-08-10"), y = 0.42, size = 5, label = "Rus. mobilization (apx.)") +
  
  ggplot2::annotate("segment", x = as.Date("2014-09-05"),xend = as.Date("2014-09-05"), y = 0.45, yend = 0.41 ,size = 1, colour = "black")+
  ggplot2::annotate("text", x = as.Date("2014-09-05"), y = 0.4, size = 5, label = "Minsk protocol") +
  
  ggplot2::annotate("segment", x = as.Date("2013-11-21"),xend = as.Date("2013-11-21"), y = 0.45, yend = 0.43 ,size = 1, colour = "black")+
  ggplot2::annotate("text", x = as.Date("2013-07-21"), y = 0.42, size = 5, label = "Euromaidan protest") +
  
  ggplot2::annotate("segment", x = as.Date("2018-11-25"),xend = as.Date("2018-11-25"), y = 0.45, yend = 0.43 ,size = 1, colour = "black")+
  ggplot2::annotate("text", x = as.Date("2018-11-25"), y = 0.42, size = 5, label = "Kerch Strait incident") +
  
  ggplot2::annotate("segment", x = as.Date("2022-02-24"),xend = as.Date("2022-02-24"), y = 0.48, yend = 0.5 ,size = 1, colour = "black")+
  ggplot2::annotate("text", x = as.Date("2021-05-24"), y = 0.51, size = 5, label = "Invasion of Ukraine") +
  
  ggplot2::annotate("segment", x = as.Date("2014-02-20"),xend = as.Date("2014-02-20"), y = 0.48, yend = 0.5 ,size = 1, colour = "black")+
  ggplot2::annotate("text", x = as.Date("2012-12-20"), y = 0.51, size = 5, label = "Annexation of Crimea") +
  
  ggplot2::annotate("segment", x = as.Date("2014-04-06"),xend = as.Date("2014-04-06"), y = 0.48, yend = 0.5 ,size = 1, colour = "black")+
  ggplot2::annotate("text", x = as.Date("2015-07-01"), y = 0.51, size = 5, label = "Start of War in Donbas ") 
  
# export 1300x600

#### propaganda by reach #############################



ggplot(data = drop_na(data,propaganda), aes(x=as.factor(propaganda), y=retweet_count, fill = as.factor(propaganda))) + 
  geom_boxplot() +
  ylim(0, 100) +
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  
  labs(title = "Tweet reach by propaganda",
       y = "Average per tweet", x = "Popularity metric type") + 
  facet_wrap(~ class, nrow = 2)














############################################






ggplot(data = popplotdata, aes(x = metric, y = value, fill = media)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  
  labs(title = "Russian state media\npopularity comparison",
       y = "Average per tweet", x = "Popularity metric type")+ scale_color_identity(name = "Period:",
                     breaks = c("RT", "Sputnik", "TASS"),
                     labels = c("Life", "Job", "Income"),
                     guide = "legend")




library("scatterplot3d")

scatterplot3d(drop_na(data,propaganda)[,c(15,17,16)], pch = 16, color= as.factor(drop_na(data,propaganda)[,3]), xlim = c(0,1000), ylim = c(0,200), zlim = c(0,100) )


ggplot(data = drop_na(data,propaganda), aes(x = like_count, y = retweet_count, color = as.factor(propaganda))) +
  geom_point() + xlim(0,1000) + ylim(0,300)
