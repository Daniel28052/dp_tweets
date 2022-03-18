library(tidyverse)
library(tidytext)
library(purrr)
library(haven)
library(SnowballC)
library(stopwords)
library(scales)
library(ggpubr)
library(lubridate)
##library(textcat)
##library(tm)
##library(quanteda)
##library(tokenizers)


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
############################### Read dataset ###################################
################################################################################

## this dataset is available from
## https://drive.google.com/drive/folders/11nl761b-FAzPLTs9U58SypAovi-B__li

setwd("D:/Research/Vasilis Stavropoulos/Digital Phenotype/Twitter data")
data_tweets <- read_csv("dep_health_tweets.csv")

################################################################################
########################## Load and clean data #################################
################################################################################

## Checking for missing values 
length(which(!complete.cases(data_tweets)))

## Convert condition (depressed - healthy) into factor
data_tweets$Condition <- as.factor(data_tweets$Condition)

## remove all non-alphanumeric characters and replace with a space
data_tweets_1 <- data_tweets %>%
  mutate(text = str_replace_all(text, "[^[:alnum:]]"," "))




#cleaning
remove_reg <- "&amp;|&lt;|&gt;|\\d+\\w*\\d*|#\\w+|[^\x01-\x7F]|[[:punct:]]|https\\S*"
# &amp = @
# &lt;= <
# &gt; >


#removing retweets characters
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg))



################################################################################
############################## Explore data ####################################
################################################################################

data_tweets_1 <- data_tweets%>%
  filter(str_detect(text, "[^0-9]"))%>%
  mutate(text = str_remove_all(text, "[1234567890]"))

## How many users by condition
data_tweets_1%>%
  group_by(Condition)%>%
  summarize(screenName = n_distinct(screenName))

## How many tweets by condition
data_tweets_1%>%
  count(Condition)

## Add a column with text length and inspect summary of Text Length for both conditions
data_tweets_1$TextLength <- nchar(data_tweets_1$text)
summary(data_tweets_1$TextLength)

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
############################## Tokenizing ######################################
################################################################################

## Tokenization as explained in https://www.tidytextmining.com/tidytext.html 
## select variables of interest, filter out symbols and numbers
unn_data_tweets <- data_tweets_1%>%
  select(text,created,Condition, TextLength)%>%
  filter(str_detect(text, "[^0-9]"))%>%
  mutate(text = str_remove_all(text, "[1234567890]"))%>%
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
  str_replace("^thei$", "they")
  


## filter unwanted words
unn_data_tweets_1 <- unn_data_tweets%>%
  filter(!word %in% c("f","a","e", "zod","ziyech","https", "t", "co","kxffdubxc","fd","btmstdha","s",
                      "mysglsswv","yqrvfrkk","kdegdto","bfg","ejebpzcet","chi","frkluz",
                      "daqcbzisb","qyswoqlfm","oszbaeeqme","mkbvlegqv","mseoqdjxw","anfeylzsd",
                      "cyjfikd","xmvlbhfwzg","mlnbulfvf","oiefvfecel","ihnapez","zmrmkwzbv","mnbvnruw",
                      "frufblivu","nyqpbssw","cqxpvgx","chmbspgzr","wwyqofmk","pglynfylh","kingttjqo",
                      "iincqwre","naboregjeringene","regjeringen","cdykmjp","ybmfqnjtlf","ff","ffe","ofegujiq",
                      "hvordan","wzgsenneut","ontfgb","rshdzipmi","urllib","rshdzipmi","whugjsella","gt", "bcd",
                      "fc","ba", "fa", "t.co"))


## Write and read csv
setwd("C:/Users/danie/Desktop/dp_tweets")
write_csv(unn_data_tweets_1, "unn_data_tweets_1.csv")
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

unn_tweets_2%>%
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
  filter(!word %in% c("a","m","s","x","itz","ak","al","c","b","k","r","e","df",
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




## Stemming 
stemmed_tweets <- unn_tweets_2%>%
  mutate(stem = wordStem(word)) %>%
  group_by(Condition)%>%
  count(stem, sort = TRUE)

stemmed_tweets$stem <- stemmed_tweets$stem%>%
  str_replace("^thi$", "thing")%>%
  str_replace("^thei$", "they")

## Write and read a csv file with tokenized data to avoid loading everything
write_csv(stemmed_tweets, "stemmed_tweets.csv")
stemmed_tweets <- read_csv("stemmed_tweets.csv")










##                                   ###
## trial with subset (sliced) sample ###
##                                   ###
tweets_sliced <- stemmed_tweets%>%
  slice(321200:321300)
tweets_sliced





################################################################################
##################### Temporal Features ########################################
################################################################################

