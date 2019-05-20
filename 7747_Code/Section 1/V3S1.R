# Register API using Twitter account
# https://apps.twitter.com/

# Insert values
api_key <- 'xxx'
api_secret <- 'xxx'
access_token <- 'xxx'
access_token_secret <- 'xxx'

library(twitteR)
setup_twitter_oauth(api_key,
                    api_secret,
                    access_token,
                    access_token_secret)

# Extract tweets
tweets <- searchTwitter("$aapl", n=1000, lang = 'en')
length(tweets)
tweets
apple <- twListToDF(tweets)

# Create CSV file
write.csv(apple, file = '~/Desktop/apple.csv', row.names = F)

# Read data file
apple <- read.csv(file.choose(), header = T)
str(apple)

# Build corpus
library(tm)
corpus <- iconv(apple$text, to = 'utf-8-mac')
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, gsub, 
                   pattern = "stocks", 
                   replacement = "stock")

# Create term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>= 20)
barplot(w, las = 2, col = rainbow(40))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(375)
wordcloud(words = names(w),
          freq = w,
          random.order = FALSE,
          max.words = 200,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.4),
          rot.per = 0.8)

# Load packages
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

# Read File
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to = "utf-8-mac")

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
get_nrc_sentiment('delay')
head(s)
tail(s)
tweets[4]

# Plot sentiment scores
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Total Count',
        main = 'Sentiment Score for Apple Tweets')
