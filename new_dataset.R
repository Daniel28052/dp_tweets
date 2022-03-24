library(readr)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(httr)
options(scipen=999)

################################################################################
#################### Get depressed user profiles ###############################

#search user profiles
#----------------------

# FUNCTIONS - need to be run first

#----------------------
# FUNCTION 1
#----------------------

#get twitter bearer token from .Renviron and add it to header
#to open your .Renviron file "usethis::edit_r_environ()" in console
get_auth_headers<-function(){
  #get token from the environment var
  bearer_token<-Sys.getenv('TWITTER_BEARER')
  
  if (identical(bearer_token, "")){
    stop("check your Twitter bearer_token in the .Renviron file")
  }
  headers = c(
    `Authorization` = sprintf('Bearer %s', bearer_token)
  )
  return (headers)
}

#-----------------------
# FUNCTION 2
#-----------------------

#building a query - generic
build_query<-function(endpoint_url, params, auth_headers=get_auth_headers()){
  
  #App rate limit: 300 requests per 15-minute window shared among all users of your app
  #App rate limit: 1 request per second shared among all users of your app
  time0<-Sys.time()
  # print(auth_headers) # debug
  response<- httr::GET(
    endpoint_url,
    httr::add_headers(auth_headers), query = params)
  
  #checking time rate
  time_dif<-as.numeric(Sys.time()- time0)
  if (time_dif<1) {(
    Sys.sleep(1)
  )
  }
  
  #check errors
  status_code<-httr::status_code(response)
  # print(paste0("Status code:", status_code))
  
  
  if(status_code(response)!=200) {
    stop(paste(" error code:", status_code ))
  }
  
  df <-jsonlite::fromJSON(httr::content(response, "text"))
  df
}

#-------------------------------
# SEARCH USERS

#App rate limit (OAuth 2.0 App Access Token): 1500 requests per 15-minute window shared among all users of your app
#User rate limit (OAuth 2.0 user Access Token): 900 requests per 15-minute window per each authenticated user
#User rate limit (OAuth 1.0a): 900 requests per 15-minute window per each authenticated user

tweets<-read_csv("https://raw.githubusercontent.com/maria-pro/tutorials/main/depression.csv")

endpoint_url<-"https://api.twitter.com/2/users/"
#based on Twitter API2.0 endpoint https://api.twitter.com/2/users/:id/tweets
# curl --request GET 'https://api.twitter.com/2/users/USER_ID/tweets' --header 'Authorization: Bearer XXXXXX'


users<-tweets$author_id
chunks <-  unname(split(users, (seq_along(users) - 1) %/% 100))

df.all <- data.frame()
users_not_found<-""

for (i in chunks){
  
  params <- list(
    "ids" = paste(unlist(chunks[1]), collapse=","),
    "user.fields" = "created_at,description,entities,id,location,name,pinned_tweet_id,
    profile_image_url,protected,public_metrics,url,username,verified,withheld"
  )
  
  
  response<- httr::GET(
    endpoint_url,
    httr::add_headers(headers), query = params)
  
  # send request and parse response
  df<-build_query(endpoint_url, params)
  users_not_found<-c(users_not_found, df[["errors"]][["resource_id"]])
  
  #merge new tweets with what is there
  df.all <- dplyr::bind_rows(df.all, df$data)
}

df.all%>%count(id)
users_not_found

df.all



################################################################################
########################## Read depression dataset #############################

userprofile_data <- read_csv("https://raw.githubusercontent.com/Daniel28052/dp_tweets/main/data/userprofile_data.csv")
names(userprofile_data)

## select variables of interest
user_data <- userprofile_data%>%
  select(c("user_id","created_at","text","screen_name","location","description",
           "followers_count","friends_count"))

################################################################################
############################## cleaning dataset ################################
remove_reg <- "&amp;|&lt;|&gt;|\\d+\\w*\\d*|#\\w+|[^\x01-\x7F]|[[:punct:]]|https\\S*"
# &amp = @
# &lt;= <
# &gt; >

## removing retweets characters
user_data <- user_data %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg))%>%
  filter(!str_detect(description, "^RT")) %>%
  mutate(text = str_remove_all(description, remove_reg))

################################################################################
######################## Following and followers ###############################

## count how many "depressed users" and mean followers
skimr::skim(user_data$followers_count)



################################################################################
############### Sentiment analysis on 'text', and 'description' #############
library(tidytext)
library(textdata)

## This shows the three types of lexicons in 'textdata' package
get_sentiments("afinn") ## assigns a value to positive and negative words
get_sentiments("bing") ## codes words a positive or negative
get_sentiments("nrc") ## assigns a sentiment to words

## Unnest 'text' - get sentiment using 'bing'-'nrc' and 'afinn' - rename columns 
user_data_2 <- user_data%>%
  unnest_tokens(word,text)%>%
  full_join(get_sentiments("bing"), by="word")%>%
  full_join(get_sentiments("afinn"), by="word")%>%
  rename(afinn_text = value, bing_text = sentiment)
user_data_2 <- user_data_2%>%
  full_join(get_sentiments("nrc"), by="word")%>%
  rename(nrc_text = sentiment,text = word)

## Unnest 'description' - get sentiment using 'bing'-'nrc' and 'afinn' - rename columns
user_data_2 <- user_data_2%>%
  unnest_tokens(word,description)%>%
  full_join(get_sentiments("bing"), by="word")%>%
  full_join(get_sentiments("afinn"), by="word")%>%
  rename(afinn_desc = value, bing_desc = sentiment)
user_data_2 <- user_data_2%>%
  full_join(get_sentiments("nrc"), by="word")%>%
  rename(nrc_desc = sentiment,description = word)



## filter NA in nrc_text, nrc_desc, bing_text, and bing_desc
user_data_3 <- user_data_2%>%
  filter(!if_all(-c(nrc_text, nrc_desc, bing_text, bing_desc), ~ is.na(.)), 
         if_all(c(nrc_text, nrc_desc, bing_text, bing_desc), ~ !is.na(.)))

  
## count and plot how many positive and negative words in pinned tweets and profile description with lexicon BING
library(patchwork)

user_data_3%>%
  count(bing_text, sort = TRUE)
user_data_3%>%
  count(bing_desc, sort = TRUE)


p1 <- user_data_3%>%
  ggplot(aes(bing_text, fill = bing_text))+
  geom_bar(stat = "count") + 
  labs(x = "Pinned tweets") + 
  theme(legend.position = "none")

p2 <- user_data_3%>%
  ggplot(aes(bing_desc, fill = bing_desc))+
  geom_bar(stat = "count") + 
  labs(x = "Profile description", y ="", fill = "Sentiment") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

pp1 <- p1 | p2 
pp1 + plot_annotation(title = 'Sentiment analysis with Lexicon Bing',
                      theme = theme(plot.title = element_text(hjust = 0.5)))


## count and plot frequency of sentiment in pinned tweets and profile description with lexicon NRC
user_data_3%>%
  count(nrc_text, sort=TRUE)
user_data_3%>%
  count(nrc_desc, sort=TRUE)


p3 <- user_data_3%>%
  ggplot(aes(nrc_text, fill = nrc_text)) +
  geom_bar(stat = "count") + 
  labs(y = "Word frequency", x = "Pinned tweets", fill = "") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- user_data_3%>%
  ggplot(aes(nrc_desc, fill = nrc_desc)) +
  geom_bar(stat = "count") + 
  labs(x = "Profile description") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
pp2 <- p3 | p4 
pp2 + plot_annotation(title = 'Sentiment analysis with Lexicon NRC',
                      theme = theme(plot.title = element_text(hjust = 0.5)))  


## Plot sentiment value in pinned tweets and profile description with lexicon AFINN
p5 <- user_data_3%>%
  ggplot(aes(afinn_text))+
  geom_density(alpha = 0.6, color = "yellow4", fill = "yellow4")  +
  labs(y = "Density", x = "Pinned tweets") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

p6 <- user_data_3%>%
  ggplot(aes(afinn_desc))+
  geom_density(alpha = 0.6, color = "turquoise4", fill = "turquoise4")  +
  labs(y = "", x = "Profile description") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
pp3 <- p5 | p6 
pp3 + plot_annotation(title = 'Sentiment analysis with Lexicon Afinn',
                      theme = theme(plot.title = element_text(hjust = 0.5)))



