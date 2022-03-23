library(readr)
library(tidyverse)
library(ggplot2)
options(scipen=999)

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
############## run sentiment analysis on 'text', and 'description' #############
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

  
## count and plot how many positive and negative words in pinned tweets and profile description
## with lexicon BING
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


## count and plot frequency of sentiment in pinned tweets and profile description 
## with lexicon NRC
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

