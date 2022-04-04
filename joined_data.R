library(tidyverse)
library(tidytext)
library(purrr)
library(haven)
library(SnowballC)
library(stopwords)
library(scales)
library(ggpubr)
library(lubridate)
library(patchwork)
options("scipen"=999, "digits" = 4)

################################################################################
########################### joining depression data ############################
################################################################################

## Join data folder a
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_a")

filesd1<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd1 <- map_dfr(filesd1, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd1

## Join data folder b
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_b")

filesd2<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd2 <- map_dfr(filesd2, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd2

## Join data folder c
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_c")

filesd3<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd3 <- map_dfr(filesd3, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd3

## Join data folder d
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_d")

filesd4<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd4 <- map_dfr(filesd4, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd4

## Join data folder e
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_e")

filesd5<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd5 <- map_dfr(filesd5, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd5

## Join data folder f
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_f")

filesd6<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd6 <- map_dfr(filesd6, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd6

## Join data folder g
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_g")

filesd7<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd7 <- map_dfr(filesd7, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd7

## Join data folder h
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_h")

filesd8<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd8 <- map_dfr(filesd8, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd8

## Join data folder i
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_i")

filesd9<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd9 <- map_dfr(filesd9, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd9

## Join data folder j
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_j")

filesd10<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd10 <- map_dfr(filesd10, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd10

## Join data folder k
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_k")

filesd11<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd11 <- map_dfr(filesd11, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd11

## Join data folder l
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_l")

filesd12<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd12 <- map_dfr(filesd12, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd12

## Join data folder m
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_m")

filesd13<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd13 <- map_dfr(filesd13, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd13

## Join data folder n
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_n")

filesd14<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd14 <- map_dfr(filesd14, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd14

## Join data folder o
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_o")

filesd15<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd15 <- map_dfr(filesd15, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd15

## Join data folder p
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_p")

filesd16<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd16 <- map_dfr(filesd16, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd16

## Join data folder q
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_q")

filesd17<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd17 <- map_dfr(filesd17, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd17

## Join data folder r
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Depression tweets/Depression_tweets_r")

filesd18<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputd18 <- map_dfr(filesd18, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputd18

## Combine many DFs

many_DF_1 = list(outputd1,outputd2,outputd3,outputd4,outputd5,outputd6,outputd7,outputd8,
                 outputd9,outputd10,outputd11,outputd12,outputd13,outputd14,outputd15,outputd16,
                 outputd17,outputd18)
merged.data <- Reduce(function(...) merge(..., all=T), many_DF_1)


## Add column to identify 'depressed participants'
merged.data$Condition <- "Depressed"

tail(merged.data)


################################################################################
############################# joining healthy data #############################
################################################################################

## Join data folder a
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_a")

filesh1<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh1 <- map_dfr(filesh1, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh1

## Join data folder b
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_b")

filesh2<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh2 <- map_dfr(filesh2, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh2

## Join data folder c
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_c")

filesh3<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh3 <- map_dfr(filesh3, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh3

## Join data folder d
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_d")

filesh4<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh4 <- map_dfr(filesh4, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh4

## Join data folder e
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_e")

filesh5<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh5 <- map_dfr(filesh5, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh5

## Join data folder f
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_f")

filesh6<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh6 <- map_dfr(filesh6, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh6

## Join data folder g
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_g")

filesh7<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh7 <- map_dfr(filesh7, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh7

## Join data folder h
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_h")

filesh8<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh8 <- map_dfr(filesh8, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh8

## Join data folder i
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_i")

filesh9<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh9 <- map_dfr(filesh9, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh9

## Join data folder j
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_j")

filesh10<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh10 <- map_dfr(filesh10, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh10

## Join data folder k
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_k")

filesh11<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh11 <- map_dfr(filesh11, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh11

## Join data folder l
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_l")

filesh12<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh12 <- map_dfr(filesh12, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh12

## Join data folder m
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_m")

filesh13<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh13 <- map_dfr(filesh13, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh13

## Join data folder n
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_n")

filesh14<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh14 <- map_dfr(filesh14, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh14

## Join data folder o
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data/Healthy tweets/Healthy_o")

filesh15<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
outputh15 <- map_dfr(filesh15, read_csv, col_types = cols_only(
  text = col_character(),
  created = col_character(),
  id = col_number(),
  screenName = col_character()
))

outputh15



## Combine many DFs

many_DF_2 = list(outputh1,outputh2,outputh3,outputh4,outputh5,outputh6,outputh7,outputh8,
                 outputh9,outputh10,outputh11,outputh12,outputh13,outputh14,outputh15)
merged.data.h <- Reduce(function(...) merge(..., all=T), many_DF_2)


## Add column to identify 'depressed participants'
merged.data.h$Condition <- "Healthy"

tail(merged.data.h)

################################################################################
#### Merge depressed and healthy datasets and delete duplicated Tweets #########
################################################################################

## merge depressed and healthy tweets
full_data <- full_join(merged.data, merged.data.h)

## remove duplicated tweets
non_dup_full_data <- full_data[!duplicated(full_data$text), ]

## write csv with depressed and healthy tweets
setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data")
write_csv(non_dup_full_data, "dep_health_tweets.csv")

################################################################################
############################# Read raw dataset #################################
################################################################################

## this dataset is available from
## https://drive.google.com/drive/folders/11nl761b-FAzPLTs9U58SypAovi-B__li

setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data")
data_tweets <- read_csv("dep_health_tweets.csv")

################################################################################
############################### clean data #####################################
################################################################################

## Checking for missing values 
length(which(!complete.cases(data_tweets)))

## Convert condition (depressed - healthy) into factor
data_tweets$Condition <- as.factor(data_tweets$Condition)

## remove all non-alphanumeric characters and replace with a space
data_tweets_1 <- data_tweets %>%
  mutate(text = str_remove_all(text, "[^[:alnum:]]"))

## cleaning
remove_reg <- "&amp;|&lt;|&gt;|\\d+\\w*\\d*|#\\w+|[^\x01-\x7F]|[[:punct:]]|https\\S*"
# &amp = @
# &lt;= <
# &gt; >


## removing retweets characters
data_tweets <- data_tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg))

data_tweets_1 <- data_tweets%>%
  filter(str_detect(text, "[^0-9]"))%>%
  mutate(text = str_remove_all(text, "[1234567890]"))


################################################################################
############################## Explore data ####################################
################################################################################

## How many users by condition
data_tweets_1%>%
  group_by(Condition)%>%
  summarize(screenName = n_distinct(screenName))

## How many tweets by condition
data_tweets_1%>%
  count(Condition)

## Add a column with text length and inspect summary of Text Length for both conditions
data_tweets_1$TextLength <- nchar(data_tweets_1$text)
skimr::skim(data_tweets_1$TextLength)

## Inspect summary of Text Length for depressed
data_tweets_d <- data_tweets_1%>%
  filter(Condition == "Depressed")
summary(data_tweets_d$TextLength)

## Inspect summary of Text Length for healthy
data_tweets_h <- data_tweets_1%>%
  filter(Condition == "Healthy")
summary(data_tweets_h$TextLength)

## Visualize diff in length between conditions
data_tweets_1 %>%
  filter(TextLength < 250)%>%
  ggplot(aes(x = TextLength, fill = Condition)) + 
  geom_histogram(binwidth = 5) +
  labs(y = "Text count", x = "Text Length", 
       title = "Distribution of text length by condition") +
  theme_bw() 

################################################################################
############################ Temporal Features #################################
################################################################################
data_tweets_1 <- read_csv(data_tweets_1, "data_tweets_1.csv")


data_tweets_1<-data_tweets_1%>%
  mutate(timestamp=lubridate::ymd_hms(created),
         day_of_week=lubridate::wday(timestamp, label=TRUE),
         day_weekday=(lubridate::wday(timestamp) %in% 2:6),
         month=lubridate::month(timestamp),
         hour=lubridate::hour(timestamp))


## Most tweeted day - overall sample
data_tweets_1%>%count(day_of_week, sort=TRUE)

## Tweets during the week vs weekend - overall sample
data_tweets_1%>%count(day_weekday, sort=TRUE)

## Comparing tweet frequency by day across conditions
frequency_by_day <- data_tweets_1%>%
  group_by(Condition)%>%
  count(day_of_week, sort=TRUE)

ss1 <- data_tweets_1%>%
  group_by(Condition)%>%
  count(day_of_week, sort=TRUE)%>%
  ggplot(aes(x = `day_of_week`, y = n, group = Condition, color = Condition)) +
  geom_line(size = 1.5) + 
  labs(y = "Tweet frequency", x = "Frequency of Tweets by day", title = "") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")


## two way anova - condition and day of week on tweet frequency
frequency_by_day$Condition <- as.factor(frequency_by_day$Condition)

frequency_by_day%>%
  group_by(Condition) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE))

anova_by_day <- aov(n ~Condition+day_of_week, data =frequency_by_day)
summary(anova_by_day)


## Comparing tweet frequency weekday vs weekend across conditions
frequency_by_weekend <- data_tweets_1%>%
  group_by(Condition)%>%
  count(day_weekday, sort=TRUE)

frequency_by_weekend$day_weekday <- as.factor(frequency_by_weekend$day_weekday)
frequency_by_weekend$day_weekday <- recode_factor(frequency_by_weekend$day_weekday,`TRUE`="Weekday",`FALSE` = "Weekend")

ss2 <- frequency_by_weekend%>%
  ggplot(aes(x = `day_weekday`, y = n, group = Condition, color = Condition)) +
  geom_line(size = 1.5) + 
  labs(y = "", x = "Weekday vs Weekend", title = "") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")

## two way anova - condition and day of week on tweet frequency
frequency_by_weekend$day_weekday <- as.factor(frequency_by_weekend$day_weekday)

frequency_by_weekend%>%
  group_by(Condition) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE))

anova_by_weekend <- aov(n ~ Condition*day_weekday, frequency_by_weekend)
summary(anova_by_weekend)


## time of day by condition
tweets_by_hour <- data_tweets_1%>%
  count(Condition, hour)%>%
  pivot_wider(names_from = Condition, values_from = n)

library(formattable)
formattable(tweets_by_hour)

ss3 <- data_tweets_1%>%
  count(Condition, hour)%>%
  ggplot(aes(x = hour, y= n, fill = Condition, color = Condition)) + 
  geom_smooth() +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(x = "Time of day", y = "Tweet frequency") + 
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


(ss1 | ss2) / ss3 


################################################################################
############################# Read clean dataset ###############################
################################################################################

write_csv(data_tweets_1, "clean_tweets.csv")
data_tweets_1 <- read_csv("clean_tweets.csv")

View(data_tweets_1)
################################################################################
############################### Tokenizing #####################################
################################################################################
data_tweets_1$day_weekday <- as.factor(data_tweets_1$day_weekday)
data_tweets_1$day_weekday <- recode_factor(data_tweets_1$day_weekday,`TRUE`="Weekday",`FALSE` = "Weekend")

## Tokenization as explained in https://www.tidytextmining.com/tidytext.html 

unn_data_tweets <- data_tweets_1%>%
  select(text,screenName,created,Condition,TextLength,timestamp,day_of_week,
         day_weekday,hour)%>%
  unnest_tokens(word, text)


## replace specific words
unn_data_tweets$word <- unn_data_tweets$word%>%
  str_replace("^u$", "you")%>%
  str_replace("^m$", "me")%>%
  str_replace("^don$", "don't")%>%
  str_replace("^ll$", "will")%>%
  str_replace("^hes$", "he")%>%
  str_replace("^th$", "the")%>%
  str_replace("^nd$", "and")%>%
  str_replace("^b$", "be")%>%
  str_replace("^d$", "would")%>%
  str_replace("^bc$", "because")%>%
  str_replace("^bb$", "bye")%>%
  str_replace("^c$", "see")%>%
  str_replace("^thi$", "nothing")%>%
  str_replace("ar$", "are")%>%
  str_replace("wa$", "want")%>%
  str_replace("^thei$", "they")%>%
  str_replace("^im$","i")
  


## filter unwanted words
unn_data_tweets_1 <- unn_data_tweets%>%
  filter(!word %in% c("f","a","e", "zod","ziyech","https", "t", "co","kxffdubxc","fd","btmstdha","s",
                      "mysglsswv","yqrvfrkk","kdegdto","bfg","ejebpzcet","chi","frkluz",
                      "daqcbzisb","qyswoqlfm","oszbaeeqme","mkbvlegqv","mseoqdjxw","anfeylzsd",
                      "cyjfikd","xmvlbhfwzg","mlnbulfvf","oiefvfecel","ihnapez","zmrmkwzbv","mnbvnruw",
                      "frufblivu","nyqpbssw","cqxpvgx","chmbspgzr","wwyqofmk","pglynfylh","kingttjqo",
                      "iincqwre","naboregjeringene","regjeringen","cdykmjp","ybmfqnjtlf","ff","ffe","ofegujiq",
                      "hvordan","wzgsenneut","ontfgb","rshdzipmi","urllib","rshdzipmi","whugjsella","gt", "bcd",
                      "fc","ba", "fa", "t.co","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa"))


## Write csv
write_csv(unn_data_tweets_1, "unn_data_tweets_1.csv")

################################################################################
########################## Read tokenized dataset ##############################
################################################################################

unn_data_tweets_1 <-  read_csv("unn_data_tweets_1.csv")

################################################################################
######################### Count word frequency #################################
################################################################################

## add a column counting how many words per condition and plot
unn_tweets <- unn_data_tweets_1%>%
  count(word, Condition, sort = TRUE)%>%
  ungroup()

unn_tweets%>%
  group_by(Condition)%>%
  slice_max(n, n = 10)%>%
  ungroup()%>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(n, word, fill = Condition)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Condition, scales = "free_y") +
  labs(x = "Condition",
       y = NULL) + 
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))


## remove stopwords and plot word frequency by condition
unn_tweets_2 <- unn_tweets%>%
  filter(!(word %in% stopwords(source = "snowball")))%>%
  filter(!word %in% c("ea","fef","bf","ed","eb","bbe","bbf","n","bae","ec","baa","de","fff","ng",
                      "fb","af","cb"))

unn_tweets_2 <- unn_tweets_2%>%
  group_by(Condition)%>%
  mutate(word = reorder(word, n))%>%
  ungroup()

unn_tweets_2%>%
  group_by(Condition)%>%
  slice_max(n, n = 10)%>%
    ggplot(aes(n, 
             fct_reorder(word, n),
             fill = Condition)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Condition, scales = "free_y") +
  labs(x = "Condition",
       y = NULL)



## comparing word frequency by condition
frequency <- unn_tweets_2%>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(Condition, word) %>%
  group_by(Condition) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  filter(proportion>0.000004)%>%
  pivot_wider(names_from = Condition, values_from = proportion)


frequency <- na.omit(frequency)
frequency <- frequency%>%
  filter(!word %in% c("a","aaaaaaaaa","aaaaaaaaaa","aaaaaaaaaaa","aaaaaaaaaaaaa",
  "aaaaaaaaaaaa","aaaaaaaaaaaaaa","aaaaaaaaaaaaaaa","aaaaaaaaaaaaaaaa","aaaaaaaaaaaaaaaaa",
  "m","s","x","itz","ak","al","c","b","k","r","e","df",
  "a'tin","f","j","d","ae","tsm","aj","ej","the","bini","aespa","ad","ah","ii","sana",
  "t","bts","el","o","h","g","sid","an","ab","vj","eu","da","n","l","la","mo","p","woodz",
  "mz","fn","ph","og","em","aaj","ai","aba","aimeeke","cfc","afc"))

## plot word correlation between conditions
ggplot(frequency, aes(x = Depressed, y = Healthy)) +
  geom_abline(color = "slateblue", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3, color = "chartreuse4") +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "Healthy Tweets", x = "Depressed Tweets")

## test word correlation between conditions
cor.test(frequency$Depressed, frequency$Healthy)


################################################################################
####### Looking at weighted Log odd  words by condition using tidylo ###########
################################################################################
library(tidylo)

log_odds_tweets_nostopwords <- unn_tweets_2%>%
  bind_log_odds(Condition, word, n)

log_odds_tweets_nostopwords%>%
  group_by(Condition)%>%
  slice_max(log_odds_weighted, n = 10)%>%
  ungroup()%>%
  ggplot(aes(log_odds_weighted,
             fct_reorder(word, log_odds_weighted),
             fill = Condition)) + 
  geom_col(show.legend = TRUE) + 
  facet_wrap(vars(Condition), scales = "free_y") + 
  labs(x = "log odds (weighted)", y = NULL)



################################################################################
####################### Sentiment Analysis setup ###############################
################################################################################
library(textdata)

## This shows the three types of lexicons in 'textdata' package
get_sentiments("afinn") ## assigns a value to positive and negative words
get_sentiments("bing") ## codes words a positive or negative
get_sentiments("nrc") ## assigns a sentiment to words


## First, use lexicon 'bing'
## Create column with sentiment via bing lexicon
unn_data_tweets_2 <- unn_data_tweets_1%>%
  full_join(get_sentiments("bing"), by="word")


unn_data_tweets_2$Condition <- as.factor(unn_data_tweets_2$Condition)
## rename column 'sentiment' as 'sent_bing' and turn it into a factor
unn_data_tweets_2 <- rename(unn_data_tweets_2, sent_bing = sentiment)
unn_data_tweets_2$sent_bing <- as.factor(unn_data_tweets_2$sent_bing)

## Second, use lexicon 'nrc'
## Create column with sentiment via nrc lexicon
unn_data_tweets_2 <- unn_data_tweets_2%>%
  full_join(get_sentiments("nrc"), by="word")

## rename column 'sentiment' as 'sent_nrc'
unn_data_tweets_2 <- rename(unn_data_tweets_2, sent_nrc = sentiment)

## Third, use lexicon 'afinn'
## Create column with sentiment via afinn lexicon
unn_data_tweets_2 <- unn_data_tweets_2%>%
  full_join(get_sentiments("afinn"), by="word")

## rename column 'value' as 'sent_value'
unn_data_tweets_2 <- rename(unn_data_tweets_2, sent_value = value)

################################################################################
####################### Running Sentiment Analysis #############################
################################################################################

## filter NA in nrc_text, nrc_desc, bing_text, and bing_desc
unn_data_tweets_3 <- unn_data_tweets_2%>%
  filter(!if_all(-c(sent_bing, sent_nrc, sent_value,Condition), ~ is.na(.)), 
         if_all(c(sent_bing, sent_nrc, sent_value,Condition), ~ !is.na(.)))

## count how many positive and negative words by condition
unn_data_tweets_3%>%
  group_by(Condition)%>%
  count(sent_bing, sort = TRUE)


## plot how many positive and negative words by condition
unn_data_tweets_3%>%
  group_by(Condition)%>%
  ggplot(aes(sent_bing, fill = Condition))+
  geom_bar(stat = "count") + 
  labs(x = "Frequency of positive and negative words")

## plot proportion of positive and negative words by condition
bing <- unn_data_tweets_3%>%
  ggplot(aes(sent_bing, group = Condition)) + 
  geom_bar(aes(y = ..prop..,fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y=..prop..), stat="count", vjust = 1.5, color = "white") +
  scale_y_continuous(labels=scales::percent) + 
  labs(x = "", y = "Proportion")+
  theme(legend.position = "null",
        axis.title.y = element_text(face= "bold"),
        axis.text.x = element_text(face = "bold"))+
  facet_grid(~Condition) 


## run chi square between pos/neg words and condition
tbl = table(chi_sq_bing$Condition, chi_sq_bing$sent_bing)
chi_sq_bing <- unn_data_tweets_3%>%
  select(Condition, sent_bing)
chisq.test(tbl)


## count frequency of word description by condition 
unn_data_tweets_3%>%
  group_by(Condition)%>%
  count(sent_nrc, sort=TRUE)

## plot frequency of word description by condition 
plot_nrc_1 <- unn_data_tweets_3%>%
  group_by(Condition)%>%
  count(sent_nrc, sort=TRUE)%>%
  ggplot(aes(x = `sent_nrc`, y = n, group = Condition, color = Condition)) +
  geom_line(size = 1.5) + 
  labs(y = "Frequency", x = "NRC frequency", 
       title = "") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


## plot proportion of nrc sentiments by condition
nrc <- unn_data_tweets_3%>%
  group_by(Condition)%>%
  count(sent_nrc)%>%
  pivot_wider(names_from = sent_nrc, values_from = n)%>%
  mutate(sum = as.character(rowSums(select(cur_data(), is.numeric))))%>%
  summarise_if(is.numeric, ~. / as.numeric(sum))%>%
  pivot_longer(cols = anger:trust, names_to = "sent_nrc", values_to = "n")



plot_nrc_2 <- nrc%>%
  ggplot(aes(x=sent_nrc, y=n, fill=Condition)) + 
  geom_bar(aes(y = n), position = "dodge", stat = "identity") +
  geom_text(aes(x=sent_nrc, y = n, label= paste0(round((100*n),1),"%")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, color = "black", size = 2) + 
  scale_y_continuous(labels=percent) + 
  labs(x = "NRC proportion", y = "Percentage") +
  theme(plot.title = element_text
        (face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text(face = "bold"))



(plot_nrc_1 + plot_nrc_2) + plot_annotation(title = 'Sentiment analysis with NRC Lexicon ',
                                            theme = theme(plot.title = element_text(hjust = 0.5)))




unn_data_tweets_3%>%
  group_by(Condition)%>%
  ggplot(aes(sent_nrc, group = Condition)) + 
  geom_bar(aes(y = ..prop..,fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(..prop..),
                 y=..prop..), stat="count", vjust = 1.5, color = "black") +
  scale_y_continuous(labels=scales::percent) + 
  labs(x = "Sentiment proportion", y = "Proportion")+
  theme(legend.position = "null")+
  facet_grid(~Condition) +  
  theme(plot.title = element_text
        (face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))



## plot Sentiment value by condition
afinn_plot <- unn_data_tweets_3%>%
  ggplot(aes(sent_value, fill = Condition))+
  geom_density(alpha = 0.6)  +
  labs(y = "Density", x = "Afinn value", 
       title = "") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")


## plot bing to fit patchwork
bing2 <- unn_data_tweets_3%>%
  group_by(Condition)%>%
  count(sent_bing)%>%
  pivot_wider(names_from = sent_bing, values_from = n)%>%
  mutate(sum = as.character(rowSums(select(cur_data(), is.numeric))))%>%
  summarise_if(is.numeric, ~. / as.numeric(sum))%>%
  pivot_longer(cols = negative:positive, names_to = "sent_bing", values_to = "n")

bing_plot <- bing2%>%
  ggplot(aes(sent_bing, y=n, fill = Condition)) + 
  geom_bar(aes(y = n), position = "dodge", stat="identity") +
  geom_text(aes(x=sent_bing, y = n, label= paste0(round((100*n),1),"%")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, color = "black", size = 2) +
  scale_y_continuous(labels=percent) + 
  labs(x = "Bing", y = "Proportion")+
  theme(plot.title = element_text
        (face = "bold", hjust = 0.5), 
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(hjust = 1),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "null")


plot_nrc_1 + plot_nrc_2 + afinn_plot + bing_plot
  plot_layout(ncol = 2)

##
write_csv(unn_data_tweets_2, "up_to_sent_analysis.csv")



################################################################################
################## Remove stopwords, stemming, and tf idf ######################
################################################################################
unn_data_tweets_2 <- read_csv("sent_analysis.csv")


## remove stopwords
unn_data_tweets_4 <- unn_data_tweets_3%>%
  anti_join(stop_words)

## stem words
stemmed_tweets <- unn_data_tweets_4%>%
  mutate(stem = wordStem(word))

## joined stemmed dataset with full dataset
unn_dat_tweets_5 <- full_join(stemmed_tweets, unn_data_tweets_4)

## run tf-idf
tweets_tf_idf <- unn_dat_tweets_5%>%
  count(Condition, stem, sort = TRUE)%>%
  bind_tf_idf(stem, Condition, n)

## join tf-idf dataset with full dataset
unn_dat_tweets_6 <- full_join(tweets_tf_idf, unn_dat_tweets_5)

## look for top tf-idf
top_tf_idf <- unn_dat_tweets_6%>%
  arrange(desc(tf_idf))
  slice_max(tf_idf, n = 10000000) 

  
##top_tf_idf <- top_tf_idf%>%
##filter(!stem %in% c("animaldefencebz","fdfa","animalsholbox","anipal","laurenjauregui",
                      ##"thomassanders","hunchoseann","rramshackled","reapearls","rubina",
                      ##"patton","thenameisyash","ammiedude","khng","fantasyugh","coffeaxxict",
                      ##"rubinav","jypetwice","slyfox","fxntasyfull","jlkblackburn","bloodygoatmama",
                      ##"seungyoun","sadorchestra","ecmclaughlin","rubidilaik","viexkitty",
                      ##"thebeanweasel","rubiholics","quackitysrose","defendfuriously",
                      ##"thng","macchuoffcl","gashapun","ashtheromansimp","yash","luliscott",
                      ##"rolipoii","actorvijay","cupofmatcha","rurumadrid","asgore","ishehnaazgill",
                      ##"saintsup","miyabihalcus","sagarmediainc","dystoman","bayameyo","adderallblack",
                      ##"ningning","trong","craigo","gregorywhitta","fdf"))


  
write_csv(unn_dat_tweets_6, "stemmed_tweets.csv")
write_csv(unn_data_tweets_3, "sent_analysis.csv")
################################################################################
######################### Part of speech tagging ###############################
################################################################################



################################################################################
############################# Prep ML models ###################################
################################################################################
library(tidymodels)

## load both datasets - stemmed and tokenized tweets 
token_twts <- read_csv("sent_analysis.csv")
stemmed_tweets <- read_csv("stemmed_tweets.csv")
## select variables of interest and make chr as factor
token_twts_1 <- token_twts%>%
  select(Condition, timestamp, TextLength,day_of_week,day_weekday,hour,sent_bing,sent_nrc,sent_value)%>%
  mutate(timestamp = as.numeric(timestamp),
         day_of_week = factor(day_of_week),
         day_weekday = factor(day_weekday),
         sent_bing = factor(sent_bing),
         sent_nrc = factor(sent_nrc))%>%
  na.omit()

## naming factor levels and converting to numeric for token_twts
token_twts_1$day_of_week <- recode(token_twts_1$day_of_week,
                                   "Mon"= "1","Tue"= "2","Wed"= "3","Thu"= "4",
                                    "Fri"= "5","Sat"= "6","Sun"= "7")
token_twts_1$day_of_week <- as.numeric(token_twts_1$day_of_week)

token_twts_1$day_weekday <- recode(token_twts_1$day_weekday  ,
                                   "Weekday"= "1","Weekend"= "2")
token_twts_1$day_weekday <- as.numeric(token_twts_1$day_weekday)

token_twts_1$sent_bing <- recode(token_twts_1$sent_bing  ,
                                 "positive"= "1","negative"= "2")
token_twts_1$sent_bing <- as.numeric(token_twts_1$sent_bing)

token_twts_1$sent_nrc <- recode(token_twts_1$sent_nrc,
                                "negative"="1","positive"="2","joy"="3", 
                                "sadness"="4","anger"="5","trust"="6","fear"="7",
                                "anticipation"="8","disgust"="9","surprise"="10")
token_twts_1$sent_nrc <- as.numeric(token_twts_1$sent_nrc)


stemmed_tweets_1 <- stemmed_tweets%>%
  select(Condition, timestamp, TextLength,day_of_week,day_weekday,hour,sent_bing,sent_nrc,sent_value,tf_idf)%>%
  mutate(timestamp = as.numeric(timestamp),
         weekday = factor(day_of_week),
         weekend = factor(day_weekday),
         bing = factor(sent_bing),
         nrc = factor(sent_nrc))%>%
  na.omit()

## naming factor levels and converting to numeric for stemmed_tweets
stemmed_tweets_1$day_of_week <- recode(stemmed_tweets_1$day_of_week,
                                   "Mon"= "1","Tue"= "2","Wed"= "3","Thu"= "4",
                                   "Fri"= "5","Sat"= "6","Sun"= "7")
stemmed_tweets_1$day_of_week <- as.numeric(stemmed_tweets_1$day_of_week)

stemmed_tweets_1$day_weekday <- recode(stemmed_tweets_1$day_weekday  ,
                                   "Weekday"= "1","Weekend"= "2")
stemmed_tweets_1$day_weekday <- as.numeric(stemmed_tweets_1$day_weekday)

stemmed_tweets_1$sent_bing <- recode(stemmed_tweets_1$sent_bing  ,
                                 "positive"= "1","negative"= "2")
stemmed_tweets_1$sent_bing <- as.numeric(stemmed_tweets_1$sent_bing)

stemmed_tweets_1$sent_nrc <- recode(stemmed_tweets_1$sent_nrc,
                                "negative"="1","positive"="2","joy"="3", 
                                "sadness"="4","anger"="5","trust"="6","fear"="7",
                                "anticipation"="8","disgust"="9","surprise"="10")
stemmed_tweets_1$sent_nrc <- as.numeric(stemmed_tweets_1$sent_nrc)

## Split train/test both datasets
set.seed(123)

token_twts_split <- initial_split(token_twts_1, strata = Condition)
token_twts_train <- training(token_twts_split)
token_twts_test <- testing(token_twts_split)

stemmed_tweets_split <- initial_split(stemmed_tweets_1, strata = Condition)
stemmed_tweets_train <- training(stemmed_tweets_split)
stemmed_tweets_test <- testing(stemmed_tweets_split)

set.seed(234)
token_twts_folds <- vfold_cv(token_twts_train, strata = Condition)
stemmed_tweets_folds <- vfold_cv(stemmed_tweets_train, strata = Condition)


## Prep recipe - i.e., feature engineering for both datasets
## Brute force approach to balancing class sub samples via themis package
library(textrecipes)
library(themis)

token_rec <- recipe(Condition ~ timestamp + TextLength + day_of_week + day_weekday + 
                      hour + sent_bing + sent_nrc + sent_value, 
         data = token_twts_train)%>%
  step_downsample(Condition)



stemmed_rec <- recipe(Condition ~ timestamp + TextLength +  day_of_week + day_weekday + 
                        hour + sent_bing + sent_nrc + sent_value + tf_idf, 
                      data = stemmed_tweets_train)%>%
  step_downsample(Condition)


################################################################################
############################ LASSO Regression ##################################
################################################################################

## model specification
token_spec <- multinom_reg(penalty = tune(), mixture = 1)%>%
  set_mode("classification")%>%
  set_engine("glmnet")

stemmed_spec <- multinom_reg(penalty = tune(), mixture = 1)%>%
  set_mode("classification")%>%
  set_engine("glmnet")

## model workflow
token_wf <- workflow(token_rec, token_spec)
stemmed_wf <- workflow(stemmed_rec, stemmed_spec)

## set grid of values for tuning hyper parameter 
lasso_grid <- grid_regular(penalty(range = c(-5, 0)), levels = 20)

## this accelerates computational processes by allocating more core processing power
doParallel::registerDoParallel()
set.seed(2021)

## train models
token_result <- tune_grid(token_wf,
                          token_twts_folds,
                          grid = lasso_grid)


stem_result <- tune_grid(stemmed_wf,
                         stemmed_tweets_folds,
                         grid = lasso_grid)

## plot how accuracy and roc_auc drop as regularization increases
autoplot(token_result)
autoplot(stem_result)

## select best regularization value to tune hyper parameter 
## we pick best option within one SD
final_penalty_token <- token_result%>%
  select_by_one_std_err(metric = "roc_auc", desc(penalty))

final_token <- token_wf%>%
  finalize_workflow(final_penalty_token)%>%
  last_fit(token_twts_split)


final_penalty_stem <- stem_result%>%
  select_by_one_std_err(metric = "roc_auc", desc(penalty))

final_stem <- stemmed_wf%>%
  finalize_workflow(final_penalty_stem)%>%
  last_fit(stemmed_tweets_split)

  
## Evaluate model accuracy
collect_metrics(final_token)
collect_metrics(final_stem)

## Plot confusion matrix
collect_predictions(final_token)%>%
  conf_mat(Condition, .pred_class)%>%
  autoplot()


collect_predictions(final_stem)%>%
    conf_mat(Condition, .pred_class)%>%
    autoplot()
  
## Plot ROC to detect depression
collect_predictions(final_token)%>%
  roc_curve(Condition, .pred_Depressed)%>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(slope = 1, color = "gray50", lty = 2, alpha = 0.8) +
  geom_path(size = 1.5, alpha = 0.7, color = "steelblue4") + 
  labs(color = NULL)
  

collect_predictions(final_stem)%>%
  roc_curve(Condition, .pred_Depressed)%>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(slope = 1, color = "gray50", lty = 2, alpha = 0.8) +
  geom_path(size = 1.5, alpha = 0.7, color = "steelblue4") + 
  labs(color = NULL)


################################################################################
############################### Naive Bayes ####################################
################################################################################
library(discrim)
library(naivebayes)

# model specification
nb_spec <- naive_Bayes()%>%
  set_mode("classification")%>%
  set_engine("naivebayes")

# model workflow
token_nb_wf <- workflow() %>%
  add_recipe(token_rec)

stem_nb_wf <- workflow() %>%
  add_recipe(stemmed_rec)

## fit NB
nb_fit_token <- token_nb_wf %>%
  add_model(nb_spec) %>%
  fit(data = token_twts_train)

nb_fit_stem <- stem_nb_wf %>%
  add_model(nb_spec) %>%
  fit(data = stemmed_tweets_train)


## train models
token_nb_wf_2 <- workflow() %>%
  add_recipe(token_rec) %>%
  add_model(nb_spec)

stem_nb_wf_2 <- workflow() %>%
  add_recipe(stemmed_rec) %>%
  add_model(nb_spec)


## estimate how well the model performs
nb_tw_results <- fit_resamples(
  token_nb_wf_2, token_twts_folds,
  control = control_resamples(save_pred = TRUE)
) 

nb_st_results <- fit_resamples(
  stem_nb_wf_2, stemmed_tweets_folds,
  control = control_resamples(save_pred = TRUE)
)

## collect metrics and predictions
nb_tw_rs_metrics <- collect_metrics(nb_tw_results)
nb_tw_rs_predictions <- collect_predictions(nb_tw_results)

nb_st_rs_metrics <- collect_metrics(nb_st_results)
nb_st_rs_predictions <- collect_predictions(nb_st_results)


## plot confusion matrix and ROC
conf_mat_resampled(nb_tw_results, tidy = FALSE) %>%
  autoplot(type = "heatmap")

collect_predictions(nb_tw_results)%>%
  roc_curve(Condition, .pred_Depressed)%>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(slope = 1, color = "gray50", lty = 2, alpha = 0.8) +
  geom_path(size = 1.5, alpha = 0.7, color = "steelblue4") + 
  labs(color = NULL)


conf_mat_resampled(nb_st_results, tidy = FALSE) %>%
  autoplot(type = "heatmap")

collect_predictions(nb_st_results)%>%
  roc_curve(Condition, .pred_Depressed)%>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(slope = 1, color = "gray50", lty = 2, alpha = 0.8) +
  geom_path(size = 1.5, alpha = 0.7, color = "steelblue4") + 
  labs(color = NULL)


