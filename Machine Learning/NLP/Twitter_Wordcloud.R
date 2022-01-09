library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)


soccer.tweets <- searchTwitter("soccer", n=2000, lang="en")
soccer.text <- sapply(soccer.tweets, function(x) x$getText())


soccer.text <- iconv(soccer.text, 'UTF-8', 'ASCII') # remove emoticons
soccer.corpus <- Corpus(VectorSource(soccer.text)) # create a corpus


term.doc.matrix <- TermDocumentMatrix(soccer.corpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords = c("soccer","http", stopwords("english")),
                                                     removeNumbers = TRUE,tolower = TRUE))


head(term.doc.matrix)

term.doc.matrix <- as.matrix(term.doc.matrix)

word.freqs <- sort(rowSums(term.doc.matrix), decreasing=TRUE) 
dm <- data.frame(word=names(word.freqs), freq=word.freqs)


## Get Word Cloud ##
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))