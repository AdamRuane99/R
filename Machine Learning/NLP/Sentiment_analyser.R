library(syuzhet)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(ROAuth)
library(stringr)
library("httr")
library("jsonlite")
library(dplyr)
library("openssl")
library("httpuv")
library(rtweet)
library(sentimentr)
library(plotrix)
library(tidyr)


ckey <- "NP0fC5PHRmSeKcLRqsfuPs7e1"
skey <- "sjFpvWMZQlBCYxhyPk6IRD7Hm3E2RTz47I4Xup3Gud2D8UTrxu"
Bearertoken <- "AAAAAAAAAAAAAAAAAAAAAKmuXwEAAAAAshE6S1rSKV2fDY%2BSe2NM%2F35FqmQ%3D50k8KfTIpoyb4JMEzkUN5kHkPPs2y6CEJ4dEnJmzMaihrGhNoR"
AccessTok <- "1480126935300820992-q2lz7s4fR9P2K3324AtxkzsKcV24c4"
sAccessToken <- "v2YZKTUEdPokgoEi0UUgVWjogMF8g0QJwhCMHp3n70QzQ"


token <- create_token(app = "NLPDataGather",
                      consumer_key = ckey,
                      consumer_secret = skey)

### --------------------------------------------------------------------------------

##Functions 
cleanTweets = function(object.with.tweets){
  # list to dataframe
  df.tweets <- as.data.frame(object.with.tweets)
  ### - ----
  # Removes RT
  df.tweets$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
  
  #Remove non-ASCII characters
  Encoding(df.tweets$text_clean) = "latin1"
  iconv(df.tweets$text_clean, "latin1", "ASCII", sub = "")
  
  # Removes @<twitter handle>
  df.tweets$text_clean = gsub("@\\w+", "", df.tweets$text_clean)
  # Removes punctuations
  df.tweets$text_clean = gsub("[[:punct:]]", "", df.tweets$text_clean)
  # Removes numbers
  df.tweets$text_clean = gsub("[[:digit:]]", "", df.tweets$text_clean)
  # Removes html links
  df.tweets$text_clean = gsub("http\\w+", "", df.tweets$text_clean)
  # Removes unnecessary spaces
  df.tweets$text_clean = gsub("[ \t]{2,}", "", df.tweets$text_clean)
  df.tweets$text_clean = gsub("^\\s+|\\s+$", "", df.tweets$text_clean)
  # Fix for error related to formatting 'utf8towcs'"
  df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  return(df.tweets)
}

searchThis = function(search_string,number.of.tweets = 100, include_rts = FALSE, lang = "en")
{
  search_tweets(q = search_string, 
                n = number.of.tweets,
                include_rts = include_rts,
                lang = lang)
}


get_sentiment_prediction <- function(x) {
  x <- sentimentr::get_sentences(x)
  x <- sentiment(x) %>%
    mutate(polarity_level = ifelse(sentiment < 0.2, "Negative",
                                   ifelse(sentiment > 0.2, "Positive","Neutral")))
}

get_sentiment_scores <- function(prediction, polarity_level, sentiment){ 
  
  table_final <- spread(prediction, polarity_level, sentiment)
  table_final$Positive[is.na(table_final$Positive)] <- 0
  table_final$Negative[is.na(table_final$Negative)] <- 0
  table_final$Score <- table_final$Positive - table_final$Negative
  
  as_data_frame(table_final)
}

pred <- get_sentiment_prediction(deb)
tab <- get_sentiment_scores(pred, polarity_level = polarity_level, sentiment = sentiment )



tab <- table_final
p=table_final$Positive/(table_final$Positive+table_final$Negative)
p[ is.nan(p) ] <- 0
table_final$Postive_percentage=p


rename(table_final, c("Positive_percentage"=Pos_percent"))
n=table_final$Positive/(table_final$Positive+table_final$Negative)
n[ is.nan(n) ] <- 0
table_final$Neg_percent=n


#Creating Histogramm

hist(table_final$Score, colour=rainbow(10))
hist(table_final$Positive, colour=rainbow(10))
hist(table_final$Negative, colour=rainbow(10))

#Creating Pie Chart

##install.packages("plotrix")
library(plotrix)

 slices <- c(sum(table_final$Positive), sum(table_final$Negative))
lbls <- c("Positive", "Negative")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Sentiment Analysis")
pie3D(slices, labels = lbls, explode=0.0, col=rainbow(length(lbls)), main="Sentiment Analysis")

#Creating Pie chart with percentages for degree of positive,neagtive,neutral

Sc= table_final$Score
good<- sapply(table_final$Score, function(Sc) Sc > 0 && Sc <= 3)
pos1=table_final$Score[good]
pos1_len=length(pos1)

vgood<- sapply(table_final$Score, function(Sc) Sc > 3 && Sc < 5)
pos2=table_final$Score[vgood]
pos2_len=length(pos2)

vvgood<- sapply(table_final$Score, function(Sc) Sc >= 6)
pos3=table_final$Score[vvgood]
pos3_len=length(pos3)

Sc= table_final$Score
bad<- sapply(table_final$Score, function(Sc) Sc < 0 && Sc >= -3)
neg1=table_final$Score[bad]
neg1_len=length(neg1)

vbad<- sapply(table_final$Score, function(Sc) Sc < -3 && Sc >= -5)
neg2=table_final$Score[vbad]
neg2_len=length(neg2)

vvbad<- sapply(table_final$Score, function(Sc) Sc <= -6)
neg3=table_final$Score[vvbad]
neg3_len=length(neg3)

neutral= sapply(table_final$Score, function(Sc) Sc == 0)
neu=table_final$Score[neutral]
neu_len=length(neu)

slices1 <- c(pos1_len,neg3_len, neg1_len, pos2_len,  neg2_len, neu_len, pos3_len)
lbls1 <- c( "Good","Awful","Unsatisfactory", "Great", "Poor", "Neutral", "Outstanding")
pct=round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1, pct) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels ))


pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="No. of tweets with particular sentiment")

