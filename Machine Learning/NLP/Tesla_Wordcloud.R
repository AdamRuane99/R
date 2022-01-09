
##install.packages("base64enc")
##devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")

api_key <- "xxx"
api_secret <- "xxxx"
access_token <- "xxxx"
access_token_secret <- "xxxx"
bearer <- "xxxxx"
  
library(ROAuth)
library("httr")
library("jsonlite")
library(dplyr)
library("openssl")
library("httpuv")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(rtweet)



token <- create_token(app = "xxx",
                      consumer_key = api_key,
                      consumer_secret = api_secret)

## --- Search Tweets --- ##
Search_Criteria <- "Tesla"

tweets <- search_tweets(q = Search_Criteria, 
                        n = 100,
                        include_rts = FALSE,
                        lang = "en")

### - ----
tweets.text <- tweets$text


tweets.text <- iconv(tweets.text, 'UTF-8', 'ASCII') # remove emoticons
tweet.corpus <- Corpus(VectorSource(tweets.text)) # create a corpus

term.doc.matrix <- TermDocumentMatrix(tweet.corpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords = c("tesla","http", stopwords("english")),
                                                     removeNumbers = TRUE,tolower = TRUE))

term.doc.matrix <- as.matrix(term.doc.matrix)

word.freqs <- sort(rowSums(term.doc.matrix), decreasing=TRUE) 
dm <- data.frame(word=names(word.freqs), freq=word.freqs)

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))


