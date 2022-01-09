
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


## ---- Functions ---- ##
GetPositiveWords <- function() {
  library(dplyr)
  library(tidytext)
  # ===========================================================================
  # Fetch positive words from file
  # ===========================================================================
  positive.words <- get_sentiments("nrc") %>%
    filter(sentiment == "positive")
  
  return(positive.words)
}

GetNegativeWords <- function() {
  library(dplyr)
  library(tidytext)
  # ===========================================================================
  # Fetch negative words from file
  # ===========================================================================
  negative.words <- get_sentiments("nrc") %>%
    filter(sentiment == "negative")
  
  return(negative.words)
}











## ---- Functions ---- ##

## -- Define sentiment --- ##
kPosText <- GetPositiveText()
kNegText <- GetNegativeText()

# get positive and negative wordlists
kPosTerms <- GetPositiveWords()
kNegTerms <- GetNegativeWords()



ComputeSentimentScores <- function(sentences){
  # ===========================================================================
  #     Compute sentiment score on a sentences dataframe
  #
  # Args:
  #   sentences: a dataframe of sentences
  #
  # Returns:
  #   scores: 
  # ===========================================================================
  
  scores <- laply(sentences, ComputeSentimentScore)
  
  return(scores)
}

ComputeSentimentScore <- function(sentence, neg.terms=kNegTerms,
                                  pos.terms=kPosTerms) {
  # ===========================================================================
  #            Compute the sentiment score of a sentence
  #
  # Args:
  #   sentence: a string of words
  #   neg.terms: negative wordlist
  #   pos.terms: positive wordlist
  #
  # Returns:
  #   score (): (sentence, negative_matches, positive_matches)
  #             e.g. ("there will be happy and a sad day", "1", "2")
  # ===========================================================================
  
  # create holder for original sentence
  orig.sentence <- sentence
  
  # remove unnecessary characters using chained substitutions
  # TODO: look into TM package for better ways of doing this
  sentence <- tolower(gsub('\\d+', '', 
                           gsub('[[:cntrl:]]', '', 
                                gsub('[[:punct:]]', '', sentence))))
  
  # split sentence into words
  words <- unlist(str_split(sentence, '\\s+'))
  
  # build vector with matches between words and each category
  # and sum up the number of words in each category
  neg.matches <- sum(!is.na(match(words, neg.terms)))
  pos.matches <- sum(!is.na(match(words, pos.terms)))
  
  score <- c(orig.sentence, neg.matches, pos.matches)
  
  return(score)
}

# build tables of positive and negative sentences with scores
# TODO: consider adding a neutral class
pos.results <- cbind(as.data.frame(ComputeSentimentScores(kPosText)), 'positive')
neg.results <- cbind(as.data.frame(ComputeSentimentScores(kNegText)), 'negative')

colnames(pos.results) <- c('sentence', 'neg', 'pos', 'sentiment')
colnames(neg.results) <- c('sentence', 'neg', 'pos', 'sentiment')

total.results <- rbind(pos.results, neg.results)

# turn the outcome variable (last column) into a factor
# TODO: re-think the necessity of this step
total.results[,4] <- as.factor(total.results[,4])

# run the naive bayes model
NaiveBayesClassifier <- naiveBayes(total.results[,2:3], total.results[,4])

