rm(list=ls())

#Loading libraries.
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(tidytext)
library(syuzhet)
library(SnowballC)
library(qdap)
library(mgsub)

options(stringsAsFactors = FALSE)

Amazon_Unlocked_Mobile <- read_csv("Amazon_Unlocked_Mobile.csv")

# Select only apple branded products
amzn_apple <- Amazon_Unlocked_Mobile[which(Amazon_Unlocked_Mobile$`Brand Name`=='Apple'), ]

# Continue with first 10000 reviews (for speed during demonstrations)
reviews_df <- amzn_apple[1:10000,]

# save for stage three without stemming and including punctuation
reviews_backup <- reviews_df 

# Load "translation" of emoticons
data(emoticon)
head(emoticon)

reviews_df$Reviews <- as.character(reviews_df$Reviews)  %>%
  tolower() %>% # Transform everything in colum Reviews into lowercase
  {gsub("\\n", " ", .)} %>%                        # Remove \n (newline)     
  {gsub("[?!]+",".",.)} %>%                        # Remove ? and ! (replace by single .)
  {gsub("[\\[\\*\\]]*"," ",.)} %>%                 # Remove [ and ] * (replace by single space)
  {gsub("(\"| |\\$)-+\\.-+"," number ", .)} %>%    # Find numbers
  {gsub("(-+:)*-+ *am"," timeam", .)} %>%          # Find time AM
  {gsub("(-+:)*-+ *pm"," timepm", .)} %>%          # Find time PM
  {gsub("-+:-+","time", .)} %>%                    # Find general time
  {gsub("( |\\$)--+"," number ", .)} %>%           # Find remaining numbers
  {gsub("-"," ", .)} %>%                           # Remove all -
  {gsub("\"+"," ", .)} %>%                         # Remove all "
  {gsub(";+"," ", .)} %>%                          # Remove excess ;
  {gsub("\\.+","\\. ", .)} %>%                     # Remove excess .
  {gsub(" +"," ", .)} %>%                          # Remove excess spaces
  {gsub("\\. \\.","\\. ", .)}                      # Remove space between periods


all_words <- reviews_df[,] %>%
  unnest_tokens("Reviews", output = "word") %>%
  anti_join(stop_words, by = "word") %>% # Remove stop words
  count(word, sort = TRUE) %>% # how often words appear
  filter(n>50) # words that appear more than 50 times filter

#all_words$sentiment <- get_sentiment(all_words$word, method = "bing", language = "english")
sentiment_scores <- polarity(all_words$word)$all
all_words$sentiment <- sentiment_scores[,"polarity"]

all_words %>%
  #  filter(n > 1500) %>%
  filter(sentiment != 0) %>%
  mutate(n = ifelse(sentiment == -1, -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  mutate(Sentiment = ifelse(sentiment == 1, "Postive","Negative")) %>%
  ggplot(aes(word, n, fill = Sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to \"total\" sentiment", x = "Word (min freq = 50)")
