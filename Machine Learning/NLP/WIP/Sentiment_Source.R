##Sentiment-Source ##

source('Functions.R')
library(stringr)

# get polarity datasets
kPosText <- GetPositiveText()
kNegText <- GetNegativeText()

# get positive and negative wordlists
kPosTerms <- GetPositiveWords()
kNegTerms <- GetNegativeWords()


orig.sentence <-  tweets$text[5] ## Taking a random sentence containing tesla from twiiter ##

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
  
  scores <- lapply(sentences, ComputeSentimentScore)
  
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

total.results[,4] <- as.factor(total.results[,4])

# run the naive bayes model
NaiveBayesClassifier <- naiveBayes(total.results[,2:3], total.results[,4])

