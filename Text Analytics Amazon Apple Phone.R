#Amazon_Unlocked_Mobile <- read.csv("~/Downloads/Amazon_Unlocked_Mobile.csv", stringsAsFactors=FALSE)# 413,840 observations, 6 columns
#getting rid of useless colums
#Amazon_Unlocked_Mobile <- Amazon_Unlocked_Mobile[-c(1,3,6)] #reduced to 3 columns
#Omit NAs
#Amazon_Unlocked_Mobile <- na.omit(Amazon_Unlocked_Mobile) # 348,628 observations, 3 columns
# Choosing only apple phone
#apple <- Amazon_Unlocked_Mobile[c(Amazon_Unlocked_Mobile$Brand.Name == 'Apple'),]

# load libraries
library(dplyr)
library(ggplot2)
library(tokenizers)
library(tidytext)
library(SnowballC)
library(tm)
library(stringi)
library(ggrepel)
library(stopwords)
#library(qdap)

options(stringsAsFactors = FALSE)

# Load dataset
Amazon_Apple_Phone <- read_csv("Amazon_Apple_Phone.csv")
View(Amazon_Apple_Phone)

# getting rid of brand name
apple <- Amazon_Apple_Phone[-c(2)]

apple1 <- apple

# continue with 10,000 observations
apple_reduced <- apple[1:10000,]
View(apple_reduced)

# Stage 1 
# Data pre processing

#Create corpus
options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C')
#Add ID to dataframe to keep track of reviews
reviews <- data.frame(doc_id=seq(1:nrow(apple_reduced)),text=apple_reduced$Reviews)

# Return NA instead of tolower error
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Function to clean corpus
clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)}

# List of stopwords to remove from corpus, note that I added hotel and stay as stopwords
custom.stopwords <- c(stopwords('english'), 'iphone','phone','apple')
#meta.data.reader <- DataframeSource(mapping=list(content='text', id='doc_id'))
# pre-cleaned corpus
corpus <- VCorpus(DataframeSource(reviews))
# Clean corpus
corpus_new<-clean.corpus(corpus)

# Stem corpus
corpus_stemmed <- tm_map(corpus_new, stemDocument)

# See how many characters are still inside the corpus for the first row (248 vs 135 vs 122)
# Pre-cleaned & pre-stemmed corpus
inspect(corpus[[1]])
# Cleaned & pre-stemmed corpus
inspect(corpus_new[[1]])
# Cleaned & stemmed corpus
inspect(corpus_stemmed[[1]])

# With all the infrequent words in here, the TDM will be huge in size. Three options:
# 1. Remove infrequent words as done above
# 2. Work with sparse representations
# 3. use subset of reviews

# Create TermDocumentMatrix with first 20K rows 
tdm <- TermDocumentMatrix(corpus_stemmed,control=list(weighting=weightTf))
tdm
#Set a max. wordcap to filter sparse words, filtered by frequency
max_words_in_TDM <- 9000
tdm <- tdm[names(tail(sort(rowSums(as.matrix(tdm))), max_words_in_TDM)), ]
tdm.reviews.m <- as.matrix(tdm)
View(tdm.reviews.m)
# Dimensions: 10000 (words) x 20000 (documents)
dim(tdm.reviews.m)
tdm.reviews.m[1:25,1:25]

stri_replace_all_regex('123|456|789', '([0-9]).([0-9])', '$2-$1' )


wordfreqs <- rowSums(tdm.reviews.m) 
wordfreqs <- data.frame(word = names(wordfreqs), n=wordfreqs)

wordfreqs %>%
  mutate(word = reorder(word,n)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram ")

library(tokenizers)
library(tidytext)
library(SnowballC)
library(tm)
library(stringi)
library(ggrepel)
library(stopwords)
library(qdap)

options(stringsAsFactors = FALSE)

# Load dataset
Amazon_Apple_Phone <- read_csv("Amazon_Apple_Phone.csv")
View(Amazon_Apple_Phone)

# getting rid of brand name
apple <- Amazon_Apple_Phone[-c(2)]

apple1 <- apple

# continue with 10,000 observations
apple_reduced <- apple[1:10000,]
View(apple_reduced)

# Stage 1 
# Data pre processing

#Create corpus
options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C')
#Add ID to dataframe to keep track of reviews
reviews <- data.frame(doc_id=seq(1:nrow(apple_reduced)),text=apple_reduced$Reviews)

# Return NA instead of tolower error
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Function to clean corpus
clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)}

# List of stopwords to remove from corpus, note that I added hotel and stay as stopwords
custom.stopwords <- c(stopwords('english'), 'iphone','phone','apple')
#meta.data.reader <- DataframeSource(mapping=list(content='text', id='doc_id'))
# pre-cleaned corpus
corpus <- VCorpus(DataframeSource(reviews))
# Clean corpus
corpus_new<-clean.corpus(corpus)

# Stem corpus
corpus_stemmed <- tm_map(corpus_new, stemDocument)

# See how many characters are still inside the corpus for the first row (248 vs 135 vs 122)
# Pre-cleaned & pre-stemmed corpus
inspect(corpus[[1]])
# Cleaned & pre-stemmed corpus
inspect(corpus_new[[1]])
# Cleaned & stemmed corpus
inspect(corpus_stemmed[[1]])

# With all the infrequent words in here, the TDM will be huge in size. Three options:
# 1. Remove infrequent words as done above
# 2. Work with sparse representations
# 3. use subset of reviews

# Create TermDocumentMatrix with first 20K rows 
tdm <- TermDocumentMatrix(corpus_stemmed,control=list(weighting=weightTf))
tdm
#Set a max. wordcap to filter sparse words, filtered by frequency
max_words_in_TDM <- 10000
tdm <- tdm[names(tail(sort(rowSums(as.matrix(tdm))), max_words_in_TDM)), ]
tdm.matrix <- as.matrix(tdm)
# Dimensions: 10000 (words) x 20000 (documents)
dim(tdm.reviews.m)
tdm.reviews.m[1:25,1:25]

stri_replace_all_regex('123|456|789', '([0-9]).([0-9])', '$2-$1' )

##############################################





######### Stage 2
# MDS aims at putting words at positions that reflect their distance

library(smacof)
library(ggfortify)
library(ggthemes)
library(quanteda)
library(anacor)
library(stringi)
library(stringr)
library(igraph)
library(dendextend)
library(circlize)
library(SnowballC)
library(factoextra)

reviews_corp <- corpus(apple_reduced, docid_field = "Ratings", text_field = "Reviews")
?corpus

# feature cooccurrence matrix : fcm()
Burt_fcm <- fcm(x = applecorpus, context = "document", count = "boolean", tri=FALSE)


#Need number of documents with each word on the diagonal
hotel.dfm <- dfm(corpus) # get document frequency matrix

# Does not seem to work due to RAM error
counts <- colSums(as.matrix(hotel.dfm)>0) # count how often frequency is positive
Burt_fcm <- as.matrix(Burt_fcm)
diag(Burt_fcm) <- counts[subset_words[, 1] > 0]








