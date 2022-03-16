library(readr)
library(tidyverse)
library(ggplot2)


userprofile_data <- read_csv("https://raw.githubusercontent.com/Daniel28052/dp_tweets/main/data/userprofile_data.csv")
names(userprofile_data)

## select variables of interest
user_data <- userprofile_data%>%
  select(c("user_id","created_at","screen_name","text","location","description","followers_count",
           "friends_count"))
  
View(user_data)
## count how many "depressed users"
user_data%>%
  summarize(count_distinct = n_distinct(screen_name))

## count how many times has the phrase been said
str_subset(user_data$text, "I have been diagnosed with depression", negate = FALSE)


## count mean followers for "depressed users"
user_data%>%
  summarize(mean_followers = mean(followers_count))

## plot   
user_data%>%
  ggplot(aes(x = user_id, followers_count)) +
  geom_point() + 
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE))

## slice users with top 7 followers 
followers_max <- user_data%>%
  arrange(desc(followers_count))%>%
  slice_max(followers_count, n = 7)

followers_max


followers_max%>%
  ggplot(aes(x = user_id, followers_count)) +
  geom_point() + 
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE))
   

