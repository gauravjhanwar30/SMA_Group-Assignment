
###################_________ Social Media Analytics ________#####################
###################_________  Group Assignment - 1 ________######################



#Submitted By:                                      #Submitted To:
  # Gaurav Jhanwar  (2017213050)                       # Prof. Lalit Ojha
  # Mukesh Atone    (2017223077)
  # Nilesh Pandey   (2017143175)
  # Twinkle Gupta   (2017233140)

# Indian Business Leader Choosen: Mr. Manu Kumar Jain (MD, Xiaomi India)

#*******************************************************************************#

#Library used for Text and Action Analysis
library(rtweet)
library(twitteR)
library(ggplot2)
library(plyr)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)

#Library used for Network and Location Analysis
library(widyr)
library(tidytext)
library(tidyr)
library(igraph)

################################################################################
############################# AUTHENTICATION ####################################
################################################################################

appname <- "pawcare"

key <- "RoeZI0OeKMLev9OAuhAqQLOPI"

secret <- "hCEZeKlB8F9ioqCppz7QsdFYBA00fWLmZoTK5UOsYevqGfxzmm"

OauthToken = '419502821-muPWtOBZGF8Q9eAppvM008OqoIlOvIxmeKC8CbxV'

OauthaSecret = 'N0xbKk9HpqdqaBTI1px9prhVEIWjGqbYGphMC2u5bbDw2'

setup_twitter_oauth(key, secret, OauthToken, OauthaSecret)

################################################################################
############################### TEXT ANALYSIS ###################################
################################################################################

manukumarjain_tweets <- search_tweets(q = "manukumarjain", n = 500,
                                      lang = "en",
                                      include_rts = FALSE)
head(manukumarjain_tweets$text)

# remove http elements manually
manukumarjain_tweets$stripped_text <- gsub("http.*","",  manukumarjain_tweets$text)
manukumarjain_tweets$stripped_text <- gsub("https.*","", manukumarjain_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
manukumarjain_tweets_clean <- manukumarjain_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word,stripped_text)

head(manukumarjain_tweets_clean)

manukumarjain_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

data("stop_words")
head(stop_words)
nrow(manukumarjain_tweets_clean)

# remove stop words from your list of words
manukumarjain_tweet_words <- manukumarjain_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(manukumarjain_tweet_words)

# plot the top 15 words -- notice any issues?
manukumarjain_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

################################################################################
############################## NETWORK ANALYSIS ################################
################################################################################


# remove punctuation, convert to lowercase, add id for each tweet!
manukumarjain_tweets_paired_words <- manukumarjain_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)
#ngrams specifies pairs and 2 is the number of words together

manukumarjain_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

manukumarjain_tweets_separated_words <- manukumarjain_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

manukumarjain_tweets_filtered <- manukumarjain_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
manukumarjain_words_counts <- manukumarjain_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(manukumarjain_words_counts)

# plot word network
manukumarjain_words_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - manukumarjain",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#################################################################################
############################ ACTION ANALYSIS #######################################
#################################################################################

# find recent tweets with #manukumarjain but ignore retweets
manukumarjain_tweets <- search_tweets("#manukumarjain", n = 500,
                               include_rts = FALSE)
# view top 2 rows of data
head(manukumarjain_tweets)
head(manukumarjain_tweets$text)

head(manukumarjain_tweets$screen_name)

# get a list of unique usernames
unique(manukumarjain_tweets$screen_name)

Tweetsofmanukumarjain <- userTimeline("@manukumarjain", n=10)
Tweetsofmanukumarjain

#friends of manukumarjain 
Friends = get_friends("manukumarjain" , n = 100)
Friends
Followers = get_followers("manukumarjain", n = 200)
Followers

#Retweets
manukumarjain_tweets$status_id
Retweets = get_retweets("1092727942231281664")
Retweets
Retweets$screen_name
Retweets$text

################################################################################
############################ LOCATION ANALYSIS #####################################
################################################################################

# how many locations are represented
length(unique(users$location))

users %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users - unique locations ")

#Let's sort by count and just plot the top locations.
users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(5) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")

#Removing NA values 
users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(5) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")


