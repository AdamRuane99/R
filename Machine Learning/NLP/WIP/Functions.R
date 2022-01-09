##Functions

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



GetPositiveText <- function() {
  # ===========================================================================
  # Fetch positive polarity data
  # ===========================================================================
  
  pos.tweets <- readLines( "C:/Users/Adam/Documents/Adam's File/Data Analyst Portfolio ETL SQL PBI/Machine_learning_Exploration/ML/positive-tweets.txt", encoding="UTF-8")
  pos.reviews <- readLines( "C:/Users/Adam/Documents/Adam's File/Data Analyst Portfolio ETL SQL PBI/Machine_learning_Exploration/ML/rt-polarity-pos.txt", encoding="latin1")
  
  return(append(pos.tweets, pos.reviews))
}

GetNegativeText <- function() {
  # ===========================================================================
  # Fetch negative polarity data
  # ===========================================================================
  neg.tweets <- readLines( "C:/Users/Adam/Documents/Adam's File/Data Analyst Portfolio ETL SQL PBI/Machine_learning_Exploration/ML/negative-tweets.txt", encoding="UTF-8")
  neg.reviews <- readLines( "C:/Users/Adam/Documents/Adam's File/Data Analyst Portfolio ETL SQL PBI/Machine_learning_Exploration/ML/rt-polarity-neg.txt", encoding="latin1")
  
  
  
  return(append(neg.tweets, neg.reviews))
}



